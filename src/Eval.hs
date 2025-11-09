{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval
    ( eval
    , primitiveBindings
    , apply
    ) where

import LispVal
import Parser (readExpr)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO (hFlush, stdout, openFile, hClose, hGetChar, hPutChar, hPutStr, IOMode(..), hIsEOF)

-- | Scheme式の評価
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Rational _ _) = return val
eval _ val@(Complex _ _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Char _) = return val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred', conseq, alt]) = do
    result <- eval env pred'
    case result of
        Bool False -> eval env alt
        _          -> eval env conseq
eval env (List [Atom "if", pred', conseq]) = do
    result <- eval env pred'
    case result of
        Bool False -> return Unspecified  -- R5RS: 未定義値を返す
        _          -> eval env conseq
eval _ (List [Atom "begin"]) = return Unspecified  -- 空のbegin
eval env (List (Atom "begin" : exprs)) = evalBegin env exprs
  where
    -- 末尾再帰最適化版のbegin
    evalBegin _ [] = return Unspecified
    evalBegin e [expr] = eval e expr  -- 末尾の式は評価するだけ（スタック消費なし）
    evalBegin e (expr:rest) = eval e expr >> evalBegin e rest  -- 中間の式は結果を捨てる
eval env (List (Atom "cond" : clauses)) = evalCond env clauses
eval env (List (Atom "case" : key : clauses)) = do
    keyVal <- eval env key
    evalCase env keyVal clauses
eval env (List (Atom "let" : List bindings : body')) = evalLet env bindings body'
eval env (List (Atom "let*" : List bindings : body')) = evalLetStar env bindings body'
eval env (List (Atom "letrec" : List bindings : body')) = evalLetRec env bindings body'
eval env (List (Atom "and" : exprs)) = evalAnd env exprs
eval env (List (Atom "or" : exprs)) = evalOr env exprs
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define-syntax" : Atom name : List (Atom "syntax-rules" : List _literals : rules) : _)) =
    defineVar env name (Macro (map extractPattern rules) (map extractTemplate rules))
  where
    extractPattern (List (List (_macroName : patternArgs) : _)) = List patternArgs
    extractPattern other = other
    extractTemplate (List (_ : template : _)) = template
    extractTemplate other = other
eval env (List (Atom "define" : List (Atom var : params') : body')) =
    makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
    makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
    makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
    makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
    makeVarArgs varargs env [] body'
eval _ (List [Atom "delay", expr]) = do
    forcedRef <- liftIO $ newIORef False
    valueRef <- liftIO $ newIORef (Bool False)
    return $ Promise forcedRef valueRef expr
eval env (List [Atom "force", promise]) = do
    p <- eval env promise
    case p of
        Promise forcedRef valueRef thunkExpr -> do
            isForced <- liftIO $ readIORef forcedRef
            if isForced
                then liftIO $ readIORef valueRef
                else do
                    result <- eval env thunkExpr
                    liftIO $ writeIORef forcedRef True
                    liftIO $ writeIORef valueRef result
                    return result
        other -> return other  -- 既に評価済みの値はそのまま返す
eval env (List [Atom "call/cc", func]) = do
    funcVal <- eval env func
    result <- catchError 
        (apply funcVal [Continuation (\val -> throwError $ Continue val)])
        (\e -> case e of
            Continue val -> return val
            other -> throwError other)
    return result
eval env (List [Atom "call-with-current-continuation", func]) = do
    funcVal <- eval env func
    result <- catchError 
        (apply funcVal [Continuation (\val -> throwError $ Continue val)])
        (\e -> case e of
            Continue val -> return val
            other -> throwError other)
    return result
eval env (List (function : args)) = do
    func <- eval env function
    case func of
        Macro macroPatterns macroTemplates -> do
            -- マクロ展開を試みる
            expanded <- expandMacro (List (function : args)) macroPatterns macroTemplates
            eval env expanded
        _ -> do
            argVals <- mapM (eval env) args
            apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | マクロ展開
expandMacro :: LispVal -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
expandMacro (List (_macroName : args)) macroPatterns macroTemplates = 
    case tryMatch args macroPatterns macroTemplates of
        Just expanded -> return expanded
        Nothing -> throwError $ Default "No matching macro pattern"
  where
    tryMatch [] [] [] = Nothing
    tryMatch macroArgs (pattern' : pats) (template : temps) =
        case matchPattern macroArgs pattern' of
            Just bindings -> Just (substituteTemplate template bindings)
            Nothing -> tryMatch macroArgs pats temps
    tryMatch _ _ _ = Nothing
    
    -- パターンマッチング（...対応）
    matchPattern :: [LispVal] -> LispVal -> Maybe [(String, LispVal)]
    matchPattern argList (List patternArgs) = matchArgs argList patternArgs
    matchPattern _ _ = Nothing
    
    -- 引数リストとパターンのマッチング
    matchArgs :: [LispVal] -> [LispVal] -> Maybe [(String, LispVal)]
    matchArgs [] [] = Just []
    -- ... パターン: 直前の変数に残りすべてをバインド
    matchArgs allArgs [Atom name, Atom "..."] = Just [(name, List allArgs)]
    matchArgs (arg:argRest) (Atom name : Atom "..." : patRest) = do
        -- 残りのパターンにマッチする最小限を残して、残りをnameにバインド
        let minRest = length patRest
        if length argRest < minRest
            then Nothing
            else do
                let (repeated, rest) = splitAt (length argRest - minRest + 1) (arg:argRest)
                restBindings <- matchArgs rest patRest
                return $ (name, List repeated) : restBindings
    matchArgs (arg:argRest) (Atom name : patRest) = do
        rest <- matchArgs argRest patRest
        return $ (name, arg) : rest
    matchArgs _ _ = Nothing
    
    -- テンプレート展開（...対応）
    substituteTemplate :: LispVal -> [(String, LispVal)] -> LispVal
    substituteTemplate (Atom name) bindings = 
        case lookup name bindings of
            Just val -> val
            Nothing -> Atom name
    substituteTemplate (List items) bindings = 
        List (expandItems items bindings)
    substituteTemplate other _ = other
    
    -- リスト要素の展開（...パターン処理）
    expandItems :: [LispVal] -> [(String, LispVal)] -> [LispVal]
    expandItems [] _ = []
    expandItems [Atom name, Atom "..."] bindings =
        case lookup name bindings of
            Just (List vals) -> vals
            _ -> [Atom name, Atom "..."]
    expandItems (item : Atom "..." : rest) bindings =
        case item of
            Atom name -> case lookup name bindings of
                Just (List vals) -> vals ++ expandItems rest bindings
                _ -> item : Atom "..." : expandItems rest bindings
            List subItems -> case findEllipsisVars subItems bindings of
                Just expanded -> expanded ++ expandItems rest bindings
                Nothing -> (substituteTemplate item bindings) : expandItems rest bindings
            _ -> (substituteTemplate item bindings) : expandItems rest bindings
    expandItems (item : rest) bindings =
        substituteTemplate item bindings : expandItems rest bindings
    
    -- ...パターンで複数値を持つ変数を見つけて展開
    findEllipsisVars :: [LispVal] -> [(String, LispVal)] -> Maybe [LispVal]
    findEllipsisVars items bindings = 
        let vars = [name | Atom name <- items, isListBinding name bindings]
        in if null vars
           then Nothing
           else Just $ expandEllipsisPattern items vars bindings
    
    isListBinding :: String -> [(String, LispVal)] -> Bool
    isListBinding name bindings = 
        case lookup name bindings of
            Just (List _) -> True
            _ -> False
    
    -- ...パターンのリスト展開
    expandEllipsisPattern :: [LispVal] -> [String] -> [(String, LispVal)] -> [LispVal]
    expandEllipsisPattern pattern' vars bindings =
        let lengths = [len | var <- vars, Just (List vals) <- [lookup var bindings], let len = length vals]
            maxLen = if null lengths then 0 else maximum lengths
            expandAt i = substituteTemplate (List pattern') 
                [(var, if i < len then vals !! i else List []) 
                | var <- vars
                , Just (List vals) <- [lookup var bindings]
                , let len = length vals]
        in [case expandAt i of List [x] -> x; x -> x | i <- [0..maxLen-1]]
expandMacro _ _ _ = throwError $ Default "Invalid macro form"

-- | 関数の適用
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Continuation contFunc) [arg] = contFunc arg
apply (Continuation _) args = throwError $ NumArgs 1 args
apply (Func params' varargs body' closure') args =
    if num params' /= num args && varargs == Nothing
        then throwError $ NumArgs (num params') args
        else liftIO (bindVars closure' $ zip params' args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params') args
        num = toInteger . length
        -- 末尾再帰最適化版: 最後の式だけを評価して返す
        evalBody env = evalBodyExprs env body'
        evalBodyExprs _ [] = return Unspecified
        evalBodyExprs e [expr] = eval e expr  -- 末尾式（スタック消費なし）
        evalBodyExprs e (expr:rest) = eval e expr >> evalBodyExprs e rest
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
apply notAFunction _ = throwError $ NotFunction "Not a function" (show notAFunction)

-- | ラムダ関数の作成
makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params' body' = return $ Func (map showVal params') varargs body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

-- | let特殊形式の評価
evalLet :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalLet env bindings body' = do
    bindingPairs <- mapM extractBinding bindings
    vals <- mapM (eval env . snd) bindingPairs
    let vars = map fst bindingPairs
    newEnv <- liftIO $ bindVars env (zip vars vals)
    fmap last $ mapM (eval newEnv) body'
  where
    extractBinding (List [Atom var, val]) = return (var, val)
    extractBinding badForm = throwError $ BadSpecialForm "Bad let binding" badForm

-- | let*特殊形式の評価（逐次バインド）
evalLetStar :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalLetStar env [] body' = fmap last $ mapM (eval env) body'
evalLetStar env (List [Atom var, val] : rest) body' = do
    evaledVal <- eval env val
    newEnv <- liftIO $ bindVars env [(var, evaledVal)]
    evalLetStar newEnv rest body'
evalLetStar _ (badForm : _) _ = throwError $ BadSpecialForm "Bad let* binding" badForm

-- | letrec特殊形式の評価（再帰的バインド）
evalLetRec :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalLetRec env bindings body' = do
    bindingPairs <- mapM extractBinding bindings
    let vars = map fst bindingPairs
    -- まず未定義値で環境を拡張
    newEnv <- liftIO $ bindVars env [(var, Bool False) | var <- vars]
    -- 新しい環境で値を評価
    vals <- mapM (eval newEnv . snd) bindingPairs
    -- 評価した値を設定
    mapM_ (\(var, val) -> setVar newEnv var val) (zip vars vals)
    fmap last $ mapM (eval newEnv) body'
  where
    extractBinding (List [Atom var, val]) = return (var, val)
    extractBinding badForm = throwError $ BadSpecialForm "Bad letrec binding" badForm

-- | cond特殊形式の評価
evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond _ [] = throwError $ Default "cond: no matching clause"
evalCond env (List [Atom "else", expr] : _) = eval env expr
evalCond env (List (test : exprs) : rest) = do
    result <- eval env test
    case result of
        Bool False -> evalCond env rest
        _ -> if null exprs 
             then return result
             else fmap last $ mapM (eval env) exprs
evalCond _ (badClause : _) = throwError $ BadSpecialForm "Bad cond clause" badClause

-- | case特殊形式の評価
evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase _ _ [] = throwError $ Default "case: no matching clause"
evalCase env _ (List (Atom "else" : exprs) : _) = 
    fmap last $ mapM (eval env) exprs
evalCase env keyVal (List (List datums : exprs) : rest) = do
    match <- anyM (eqvVal keyVal) datums
    if match
        then fmap last $ mapM (eval env) exprs
        else evalCase env keyVal rest
  where
    eqvVal v1 v2 = do
        result <- liftThrows $ eqv [v1, v2]
        case result of
            Bool b -> return b
            _ -> return False
    anyM _ [] = return False
    anyM f (x:xs) = do
        b <- f x
        if b then return True else anyM f xs
evalCase _ _ (badClause : _) = throwError $ BadSpecialForm "Bad case clause" badClause

-- | and特殊形式の評価（短絡評価）
evalAnd :: Env -> [LispVal] -> IOThrowsError LispVal
evalAnd _ [] = return $ Bool True
evalAnd env [expr] = eval env expr
evalAnd env (expr : rest) = do
    result <- eval env expr
    case result of
        Bool False -> return $ Bool False
        _ -> evalAnd env rest

-- | or特殊形式の評価（短絡評価）
evalOr :: Env -> [LispVal] -> IOThrowsError LispVal
evalOr _ [] = return $ Bool False
evalOr env [expr] = eval env expr
evalOr env (expr : rest) = do
    result <- eval env expr
    case result of
        Bool False -> evalOr env rest
        _ -> return result

-- | プリミティブ関数の定義
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", smartAdd)
             , ("-", smartSub)
             , ("*", smartMul)
             , ("/", smartDiv)
             , ("mod", integerDiv mod)
             , ("quotient", integerDiv quot)
             , ("remainder", integerDiv rem)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("caar", caar)
             , ("cadr", cadr)
             , ("cdar", cdar)
             , ("cddr", cddr)
             , ("caaar", caaar)
             , ("caadr", caadr)
             , ("cadar", cadar)
             , ("caddr", caddr)
             , ("cdaar", cdaar)
             , ("cdadr", cdadr)
             , ("cddar", cddar)
             , ("cdddr", cdddr)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)
             , ("not", notFunc)
             , ("list", list)
             , ("list?", isList)
             , ("null?", isNull)
             , ("length", lengthFunc)
             , ("reverse", reverseFunc)
             , ("append", appendFunc)
             , ("number?", isNumber)
             , ("string?", isString)
             , ("symbol?", isSymbol)
             , ("pair?", isPair)
             , ("procedure?", isProcedure)
             , ("boolean?", isBoolean)
             , ("char?", isChar)
             , ("vector?", isVector)
             , ("string-length", stringLength)
             , ("string-append", stringAppend)
             , ("char->integer", charToInt)
             , ("integer->char", intToChar)
             , ("abs", absFunc)
             , ("max", maxFunc)
             , ("min", minFunc)
             , ("even?", evenFunc)
             , ("odd?", oddFunc)
             , ("zero?", zeroFunc)
             , ("positive?", positiveFunc)
             , ("negative?", negativeFunc)
             , ("member", member)
             , ("memq", memq)
             , ("assoc", assoc)
             , ("assq", assq)
             , ("symbol->string", symbolToString)
             , ("string->symbol", stringToSymbol)
             , ("string-ref", stringRef)
             , ("substring", substringFunc)
             , ("string->list", stringToList)
             , ("list->string", listToString)
             , ("error", errorFunc)
             , ("sqrt", sqrtFunc)
             , ("expt", exptFunc)
             , ("sin", sinFunc)
             , ("cos", cosFunc)
             , ("tan", tanFunc)
             , ("log", logFunc)
             , ("exp", expFunc)
             , ("list-ref", listRefFunc)
             , ("list-tail", listTailFunc)
             , ("number->string", numberToString)
             , ("string->number", stringToNumber)
             ]

-- | IO関数のリスト
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("map", mapProc)
               , ("for-each", forEachProc)
               , ("filter", filterProc)
               , ("eval", evalProc)
               , ("load", loadProc)
               , ("display", displayProc)
               , ("newline", newlineProc)
               , ("read", readProc)
               , ("open-input-file", openInputFileProc)
               , ("open-output-file", openOutputFileProc)
               , ("close-input-port", closePortProc)
               , ("close-output-port", closePortProc)
               , ("read-char", readCharProc)
               , ("write-char", writeCharProc)
               , ("write", writeProc)
               , ("eof-object?", eofObjectProc)
               , ("set-car!", setCarProc)
               , ("set-cdr!", setCdrProc)
               , ("mcons", mconsProc)
               , ("mcar", mcarProc)
               , ("mcdr", mcdrProc)
               , ("make-mutable-string", makeMutableStringProc)
               , ("string-set!", stringSetProc)
               , ("mutable-string-ref", mutableStringRefProc)
               , ("mutable-string->string", mutableStringToStringProc)
               , ("vector", makeVectorProc)
               , ("make-vector", makeVectorInitProc)
               , ("vector-ref", vectorRefProc)
               , ("vector-set!", vectorSetProc)
               , ("vector-length", vectorLengthProc)
               ]

-- | プリミティブ関数を環境にバインド
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars 
    ( [makeFunc' id' func | (id', func) <- primitives] ++
      [makeIOFunc id' func | (id', func) <- ioPrimitives] )
    where 
        makeFunc' id' func = (id', PrimitiveFunc func)
        makeIOFunc id' func = (id', IOFunc func)

-- | 数値演算 - 数値タワー対応版
numericBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params' = do
    -- 有理数同士の演算は有理数を保持
    if all isRationalNum params'
        then rationalBinop op params'
        else do
            nums <- mapM toDouble params'
            let result = foldl1 op nums
            return $ if all isInteger' params' && isIntResult result
                     then Number (floor result)
                     else Float result
  where
    isInteger' (Number _) = True
    isInteger' _ = False
    isRationalNum (Rational _ _) = True
    isRationalNum (Number _) = True  -- 整数も有理数として扱える
    isRationalNum _ = False
    isIntResult x = x == fromIntegral (floor x :: Integer)

-- | 有理数演算
rationalBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
rationalBinop op params' = do
    rationals <- mapM toRationalPair params'
    let (num, denom) = foldl1 (rationalOp op) rationals
    let g = gcd num denom
    let simplified = (num `div` g, denom `div` g)
    return $ if snd simplified == 1
             then Number (fst simplified)
             else Rational (fst simplified) (snd simplified)
  where
    toRationalPair (Rational n d) = return (n, d)
    toRationalPair (Number n) = return (n, 1)
    toRationalPair val = throwError $ TypeMismatch "rational or integer" val
    
    rationalOp f (n1, d1) (n2, d2) =
        let result = f (fromIntegral n1 / fromIntegral d1) (fromIntegral n2 / fromIntegral d2)
            -- 結果を分数に変換（近似）
            denom = d1 * d2
            numer = round (result * fromIntegral denom)
        in (numer, denom)

-- | 正確な有理数除算
rationalDivision :: [LispVal] -> ThrowsError LispVal
rationalDivision [] = throwError $ NumArgs 2 []
rationalDivision [_] = throwError $ NumArgs 2 []
rationalDivision params' = do
    rationals <- mapM toRationalPair params'
    let result = foldl1 divRational rationals
    let g = gcd (fst result) (snd result)
    let simplified = (fst result `div` g, snd result `div` g)
    return $ if snd simplified == 1
             then Number (fst simplified)
             else Rational (fst simplified) (snd simplified)
  where
    toRationalPair (Rational n d) = return (n, d)
    toRationalPair (Number n) = return (n, 1)
    toRationalPair val = throwError $ TypeMismatch "rational or integer" val
    
    -- 正確な有理数除算: (n1/d1) / (n2/d2) = (n1*d2) / (d1*n2)
    divRational (n1, d1) (n2, d2) = (n1 * d2, d1 * n2)

-- | 正確な有理数加算
rationalAdd :: [LispVal] -> ThrowsError LispVal
rationalAdd [] = throwError $ NumArgs 2 []
rationalAdd params' = do
    rationals <- mapM toRationalPair params'
    let result = foldl1 addRational rationals
    let g = gcd (fst result) (snd result)
    let simplified = (fst result `div` g, snd result `div` g)
    return $ if snd simplified == 1
             then Number (fst simplified)
             else Rational (fst simplified) (snd simplified)
  where
    toRationalPair (Rational n d) = return (n, d)
    toRationalPair (Number n) = return (n, 1)
    toRationalPair val = throwError $ TypeMismatch "rational or integer" val
    
    -- (n1/d1) + (n2/d2) = (n1*d2 + n2*d1) / (d1*d2)
    addRational (n1, d1) (n2, d2) = (n1 * d2 + n2 * d1, d1 * d2)

-- | 正確な有理数減算
rationalSub :: [LispVal] -> ThrowsError LispVal
rationalSub [] = throwError $ NumArgs 2 []
rationalSub params' = do
    rationals <- mapM toRationalPair params'
    let result = foldl1 subRational rationals
    let g = gcd (fst result) (snd result)
    let simplified = (fst result `div` g, snd result `div` g)
    return $ if snd simplified == 1
             then Number (fst simplified)
             else Rational (fst simplified) (snd simplified)
  where
    toRationalPair (Rational n d) = return (n, d)
    toRationalPair (Number n) = return (n, 1)
    toRationalPair val = throwError $ TypeMismatch "rational or integer" val
    
    -- (n1/d1) - (n2/d2) = (n1*d2 - n2*d1) / (d1*d2)
    subRational (n1, d1) (n2, d2) = (n1 * d2 - n2 * d1, d1 * d2)

-- | 正確な有理数乗算
rationalMul :: [LispVal] -> ThrowsError LispVal
rationalMul [] = throwError $ NumArgs 2 []
rationalMul params' = do
    rationals <- mapM toRationalPair params'
    let result = foldl1 mulRational rationals
    let g = gcd (fst result) (snd result)
    let simplified = (fst result `div` g, snd result `div` g)
    return $ if snd simplified == 1
             then Number (fst simplified)
             else Rational (fst simplified) (snd simplified)
  where
    toRationalPair (Rational n d) = return (n, d)
    toRationalPair (Number n) = return (n, 1)
    toRationalPair val = throwError $ TypeMismatch "rational or integer" val
    
    -- (n1/d1) * (n2/d2) = (n1*n2) / (d1*d2)
    mulRational (n1, d1) (n2, d2) = (n1 * n2, d1 * d2)

-- | 数値を Double に変換
toDouble :: LispVal -> ThrowsError Double
toDouble (Number n) = return $ fromIntegral n
toDouble (Float f) = return f
toDouble (Rational num denom) = return $ fromIntegral num / fromIntegral denom
toDouble (Complex r _) = return r  -- 注意: 実部のみ使用、虚部は無視される
toDouble notNum = throwError $ TypeMismatch "number" notNum

-- | 複素数演算 - 虚部も含めた完全な演算
complexAdd :: LispVal -> LispVal -> LispVal
complexAdd (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
complexAdd (Complex r i) (Number n) = Complex (r + fromIntegral n) i
complexAdd (Complex r i) (Float f) = Complex (r + f) i
complexAdd (Number n) (Complex r i) = Complex (fromIntegral n + r) i
complexAdd (Float f) (Complex r i) = Complex (f + r) i
complexAdd v1 v2 = 
    let r1 = realPart v1
        r2 = realPart v2
    in if r1 == fromIntegral (floor r1 :: Integer) && r2 == fromIntegral (floor r2 :: Integer)
       then Number (floor r1 + floor r2)
       else Float (r1 + r2)

complexSub :: LispVal -> LispVal -> LispVal
complexSub (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)
complexSub (Complex r i) (Number n) = Complex (r - fromIntegral n) i
complexSub (Complex r i) (Float f) = Complex (r - f) i
complexSub (Number n) (Complex r i) = Complex (fromIntegral n - r) (-i)
complexSub (Float f) (Complex r i) = Complex (f - r) (-i)
complexSub v1 v2 = 
    let r1 = realPart v1
        r2 = realPart v2
    in if r1 == fromIntegral (floor r1 :: Integer) && r2 == fromIntegral (floor r2 :: Integer)
       then Number (floor r1 - floor r2)
       else Float (r1 - r2)

complexMul :: LispVal -> LispVal -> LispVal
complexMul (Complex r1 i1) (Complex r2 i2) = 
    Complex (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)
complexMul (Complex r i) (Number n) = Complex (r * fromIntegral n) (i * fromIntegral n)
complexMul (Complex r i) (Float f) = Complex (r * f) (i * f)
complexMul (Number n) c@(Complex _ _) = complexMul c (Number n)
complexMul (Float f) c@(Complex _ _) = complexMul c (Float f)
complexMul v1 v2 = 
    let r1 = realPart v1
        r2 = realPart v2
        result = r1 * r2
    in if r1 == fromIntegral (floor r1 :: Integer) && r2 == fromIntegral (floor r2 :: Integer)
       then Number (floor r1 * floor r2)
       else Float result

complexDiv :: LispVal -> LispVal -> LispVal
complexDiv (Complex r1 i1) (Complex r2 i2) = 
    let denom = r2 * r2 + i2 * i2
    in Complex ((r1 * r2 + i1 * i2) / denom) ((i1 * r2 - r1 * i2) / denom)
complexDiv (Complex r i) (Number n) = Complex (r / fromIntegral n) (i / fromIntegral n)
complexDiv (Complex r i) (Float f) = Complex (r / f) (i / f)
complexDiv (Number n) (Complex r i) = 
    let denom = r * r + i * i
    in Complex (fromIntegral n * r / denom) (-(fromIntegral n * i) / denom)
complexDiv (Float f) (Complex r i) = 
    let denom = r * r + i * i
    in Complex (f * r / denom) (-(f * i) / denom)
complexDiv v1 v2 = 
    let r1 = realPart v1
        r2 = realPart v2
    in Float (r1 / r2)

realPart :: LispVal -> Double
realPart (Number n) = fromIntegral n
realPart (Float f) = f
realPart (Rational n d) = fromIntegral n / fromIntegral d
realPart (Complex r _) = r
realPart _ = 0

-- | 複素数対応の数値演算
complexBinop :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
complexBinop _ [] = throwError $ NumArgs 2 []
complexBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
complexBinop op params' = return $ foldl1 op params'

-- | スマート演算（型に応じて最適な演算を選択）
smartAdd :: [LispVal] -> ThrowsError LispVal
smartAdd params' 
    | any isComplex params' = complexBinop complexAdd params'
    | all isRational' params' = rationalAdd params'
    | otherwise = numericBinop (+) params'

smartSub :: [LispVal] -> ThrowsError LispVal
smartSub params'
    | any isComplex params' = complexBinop complexSub params'
    | all isRational' params' = rationalSub params'
    | otherwise = numericBinop (-) params'

smartMul :: [LispVal] -> ThrowsError LispVal
smartMul params'
    | any isComplex params' = complexBinop complexMul params'
    | all isRational' params' = rationalMul params'
    | otherwise = numericBinop (*) params'

smartDiv :: [LispVal] -> ThrowsError LispVal
smartDiv params'
    | any isComplex params' = complexBinop complexDiv params'
    | all isRational' params' = rationalDivision params'
    | otherwise = numericBinop (/) params'

isComplex :: LispVal -> Bool
isComplex (Complex _ _) = True
isComplex _ = False

isRational' :: LispVal -> Bool
isRational' (Rational _ _) = True
isRational' (Number _) = True
isRational' _ = False

-- | 整数除算用
integerDiv :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerDiv _ [] = throwError $ NumArgs 2 []
integerDiv _ singleVal@[_] = throwError $ NumArgs 2 singleVal
integerDiv op params' = mapM unpackNum params' >>= return . Number . foldl1 op

-- | 真偽値演算
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                 left <- unpacker $ args !! 0
                                 right <- unpacker $ args !! 1
                                 return $ Bool $ left `op` right

numBoolBinop :: (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop toDouble

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

-- | 値のアンパック
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- | リスト操作
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- | car/cdrの組み合わせ関数
caar :: [LispVal] -> ThrowsError LispVal
caar [x] = car [x] >>= car . (:[])
caar badArgList = throwError $ NumArgs 1 badArgList

cadr :: [LispVal] -> ThrowsError LispVal
cadr [x] = cdr [x] >>= car . (:[])
cadr badArgList = throwError $ NumArgs 1 badArgList

cdar :: [LispVal] -> ThrowsError LispVal
cdar [x] = car [x] >>= cdr . (:[])
cdar badArgList = throwError $ NumArgs 1 badArgList

cddr :: [LispVal] -> ThrowsError LispVal
cddr [x] = cdr [x] >>= cdr . (:[])
cddr badArgList = throwError $ NumArgs 1 badArgList

caaar :: [LispVal] -> ThrowsError LispVal
caaar [x] = car [x] >>= car . (:[]) >>= car . (:[])
caaar badArgList = throwError $ NumArgs 1 badArgList

caadr :: [LispVal] -> ThrowsError LispVal
caadr [x] = cdr [x] >>= car . (:[]) >>= car . (:[])
caadr badArgList = throwError $ NumArgs 1 badArgList

cadar :: [LispVal] -> ThrowsError LispVal
cadar [x] = car [x] >>= cdr . (:[]) >>= car . (:[])
cadar badArgList = throwError $ NumArgs 1 badArgList

caddr :: [LispVal] -> ThrowsError LispVal
caddr [x] = cdr [x] >>= cdr . (:[]) >>= car . (:[])
caddr badArgList = throwError $ NumArgs 1 badArgList

cdaar :: [LispVal] -> ThrowsError LispVal
cdaar [x] = car [x] >>= car . (:[]) >>= cdr . (:[])
cdaar badArgList = throwError $ NumArgs 1 badArgList

cdadr :: [LispVal] -> ThrowsError LispVal
cdadr [x] = cdr [x] >>= car . (:[]) >>= cdr . (:[])
cdadr badArgList = throwError $ NumArgs 1 badArgList

cddar :: [LispVal] -> ThrowsError LispVal
cddar [x] = car [x] >>= cdr . (:[]) >>= cdr . (:[])
cddar badArgList = throwError $ NumArgs 1 badArgList

cdddr :: [LispVal] -> ThrowsError LispVal
cdddr [x] = cdr [x] >>= cdr . (:[]) >>= cdr . (:[])
cdddr badArgList = throwError $ NumArgs 1 badArgList

-- | 等値性のチェック
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                              all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left _ -> False
                               Right (Bool val) -> val
                               Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool,
                        AnyUnpacker unpackRational, AnyUnpacker unpackComplex]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || case eqvEquals of Bool x -> x; _ -> False)
equal badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` const (return False)

-- | Unpack rational numbers for equality comparison
unpackRational :: LispVal -> ThrowsError (Integer, Integer)
unpackRational (Rational n d) = return (n, d)
unpackRational notRational = throwError $ TypeMismatch "rational" notRational

-- | Unpack complex numbers for equality comparison
unpackComplex :: LispVal -> ThrowsError (Double, Double)
unpackComplex (Complex r i) = return (r, i)
unpackComplex notComplex = throwError $ TypeMismatch "complex" notComplex

-- | 論理否定
notFunc :: [LispVal] -> ThrowsError LispVal
notFunc [Bool False] = return $ Bool True
notFunc [_] = return $ Bool False
notFunc badArgList = throwError $ NumArgs 1 badArgList

-- | リストの作成
list :: [LispVal] -> ThrowsError LispVal
list args = return $ List args

-- | リストかどうかのチェック
isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList [_] = return $ Bool False
isList badArgList = throwError $ NumArgs 1 badArgList

-- | 空リストのチェック
isNull :: [LispVal] -> ThrowsError LispVal
isNull [List []] = return $ Bool True
isNull [_] = return $ Bool False
isNull badArgList = throwError $ NumArgs 1 badArgList

-- | リストの長さ
lengthFunc :: [LispVal] -> ThrowsError LispVal
lengthFunc [List xs] = return $ Number $ toInteger $ length xs
lengthFunc [badArg] = throwError $ TypeMismatch "list" badArg
lengthFunc badArgList = throwError $ NumArgs 1 badArgList

-- | リストの反転
reverseFunc :: [LispVal] -> ThrowsError LispVal
reverseFunc [List xs] = return $ List $ reverse xs
reverseFunc [badArg] = throwError $ TypeMismatch "list" badArg
reverseFunc badArgList = throwError $ NumArgs 1 badArgList

-- | リストの連結
appendFunc :: [LispVal] -> ThrowsError LispVal
appendFunc [] = return $ List []
appendFunc [List xs] = return $ List xs
appendFunc (List xs : rest) = do
    result <- appendFunc rest
    case result of
        List ys -> return $ List $ xs ++ ys
        _ -> throwError $ TypeMismatch "list" result
appendFunc (badArg : _) = throwError $ TypeMismatch "list" badArg

-- | 型判定述語
isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber badArgList = throwError $ NumArgs 1 badArgList

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString badArgList = throwError $ NumArgs 1 badArgList

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol badArgList = throwError $ NumArgs 1 badArgList

isPair :: [LispVal] -> ThrowsError LispVal
isPair [List (_ : _)] = return $ Bool True
isPair [DottedList _ _] = return $ Bool True
isPair [_] = return $ Bool False
isPair badArgList = throwError $ NumArgs 1 badArgList

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure [PrimitiveFunc _] = return $ Bool True
isProcedure [Func _ _ _ _] = return $ Bool True
isProcedure [_] = return $ Bool False
isProcedure badArgList = throwError $ NumArgs 1 badArgList

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean [_] = return $ Bool False
isBoolean badArgList = throwError $ NumArgs 1 badArgList

isChar :: [LispVal] -> ThrowsError LispVal
isChar [Char _] = return $ Bool True
isChar [_] = return $ Bool False
isChar badArgList = throwError $ NumArgs 1 badArgList

isVector :: [LispVal] -> ThrowsError LispVal
isVector [Vector _] = return $ Bool True
isVector [_] = return $ Bool False
isVector badArgList = throwError $ NumArgs 1 badArgList

-- | 文字列操作
stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ toInteger $ length s
stringLength [badArg] = throwError $ TypeMismatch "string" badArg
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend strs = do
    strings <- mapM unpackStr strs
    return $ String $ concat strings

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number n] = 
    let idx = fromInteger n
    in if idx >= 0 && idx < length s
       then return $ Char $ s !! idx
       else throwError $ Default $ "string-ref: index out of bounds: " ++ show n
stringRef [badArg, _] = throwError $ TypeMismatch "string" badArg
stringRef badArgList = throwError $ NumArgs 2 badArgList

substringFunc :: [LispVal] -> ThrowsError LispVal
substringFunc [String s, Number start, Number end'] = 
    let st = fromInteger start
        en = fromInteger end'
    in if st >= 0 && en <= length s && st <= en
       then return $ String $ take (en - st) $ drop st s
       else throwError $ Default "substring: invalid indices"
substringFunc badArgList = throwError $ NumArgs 3 badArgList

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return $ List $ map Char s
stringToList [badArg] = throwError $ TypeMismatch "string" badArg
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List chars] = do
    cs <- mapM unpackChar chars
    return $ String cs
  where
    unpackChar (Char c) = return c
    unpackChar badArg = throwError $ TypeMismatch "char" badArg
listToString [badArg] = throwError $ TypeMismatch "list" badArg
listToString badArgList = throwError $ NumArgs 1 badArgList

-- | 文字操作
charToInt :: [LispVal] -> ThrowsError LispVal
charToInt [Char c] = return $ Number $ toInteger $ fromEnum c
charToInt [badArg] = throwError $ TypeMismatch "char" badArg
charToInt badArgList = throwError $ NumArgs 1 badArgList

intToChar :: [LispVal] -> ThrowsError LispVal
intToChar [Number n] = return $ Char $ toEnum $ fromInteger n
intToChar [badArg] = throwError $ TypeMismatch "number" badArg
intToChar badArgList = throwError $ NumArgs 1 badArgList

-- | 数値関数
absFunc :: [LispVal] -> ThrowsError LispVal
absFunc [Number n] = return $ Number $ abs n
absFunc [badArg] = throwError $ TypeMismatch "number" badArg
absFunc badArgList = throwError $ NumArgs 1 badArgList

maxFunc :: [LispVal] -> ThrowsError LispVal
maxFunc [] = throwError $ NumArgs 1 []
maxFunc vals = do
    nums <- mapM unpackNum vals
    return $ Number $ maximum nums

minFunc :: [LispVal] -> ThrowsError LispVal
minFunc [] = throwError $ NumArgs 1 []
minFunc vals = do
    nums <- mapM unpackNum vals
    return $ Number $ minimum nums

evenFunc :: [LispVal] -> ThrowsError LispVal
evenFunc [Number n] = return $ Bool $ even n
evenFunc [badArg] = throwError $ TypeMismatch "number" badArg
evenFunc badArgList = throwError $ NumArgs 1 badArgList

oddFunc :: [LispVal] -> ThrowsError LispVal
oddFunc [Number n] = return $ Bool $ odd n
oddFunc [badArg] = throwError $ TypeMismatch "number" badArg
oddFunc badArgList = throwError $ NumArgs 1 badArgList

zeroFunc :: [LispVal] -> ThrowsError LispVal
zeroFunc [Number 0] = return $ Bool True
zeroFunc [Number _] = return $ Bool False
zeroFunc [badArg] = throwError $ TypeMismatch "number" badArg
zeroFunc badArgList = throwError $ NumArgs 1 badArgList

positiveFunc :: [LispVal] -> ThrowsError LispVal
positiveFunc [Number n] = return $ Bool $ n > 0
positiveFunc [badArg] = throwError $ TypeMismatch "number" badArg
positiveFunc badArgList = throwError $ NumArgs 1 badArgList

negativeFunc :: [LispVal] -> ThrowsError LispVal
negativeFunc [Number n] = return $ Bool $ n < 0
negativeFunc [badArg] = throwError $ TypeMismatch "number" badArg
negativeFunc badArgList = throwError $ NumArgs 1 badArgList

-- | リスト検索と連想リスト
member :: [LispVal] -> ThrowsError LispVal
member [_, List []] = return $ Bool False
member [obj, List (x:xs)] = do
    result <- equal [obj, x]
    case result of
        Bool True -> return $ List (x:xs)
        _ -> member [obj, List xs]
member [_, badArg] = throwError $ TypeMismatch "list" badArg
member badArgList = throwError $ NumArgs 2 badArgList

memq :: [LispVal] -> ThrowsError LispVal
memq [_, List []] = return $ Bool False
memq [obj, List (x:xs)] = do
    result <- eqv [obj, x]
    case result of
        Bool True -> return $ List (x:xs)
        _ -> memq [obj, List xs]
memq [_, badArg] = throwError $ TypeMismatch "list" badArg
memq badArgList = throwError $ NumArgs 2 badArgList

assoc :: [LispVal] -> ThrowsError LispVal
assoc [_, List []] = return $ Bool False
assoc [obj, List (List (key:vals):xs)] = do
    result <- equal [obj, key]
    case result of
        Bool True -> return $ List (key:vals)
        _ -> assoc [obj, List xs]
assoc [obj, List (_:xs)] = assoc [obj, List xs]
assoc [_, badArg] = throwError $ TypeMismatch "list" badArg
assoc badArgList = throwError $ NumArgs 2 badArgList

assq :: [LispVal] -> ThrowsError LispVal
assq [_, List []] = return $ Bool False
assq [obj, List (List (key:vals):xs)] = do
    result <- eqv [obj, key]
    case result of
        Bool True -> return $ List (key:vals)
        _ -> assq [obj, List xs]
assq [obj, List (_:xs)] = assq [obj, List xs]
assq [_, badArg] = throwError $ TypeMismatch "list" badArg
assq badArgList = throwError $ NumArgs 2 badArgList

-- | シンボル操作
symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString [badArg] = throwError $ TypeMismatch "symbol" badArg
symbolToString badArgList = throwError $ NumArgs 1 badArgList

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol [badArg] = throwError $ TypeMismatch "string" badArg
stringToSymbol badArgList = throwError $ NumArgs 1 badArgList

-- | エラー関数
errorFunc :: [LispVal] -> ThrowsError LispVal
errorFunc (String msg : _) = throwError $ Default msg
errorFunc (msg : _) = throwError $ Default $ showVal msg
errorFunc [] = throwError $ Default "error called with no arguments"

-- | IO関数の実装
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc [] = throwError $ NumArgs 1 []

mapProc :: [LispVal] -> IOThrowsError LispVal
mapProc [_func, List []] = return $ List []
mapProc [func, List (x:xs)] = do
    result <- apply func [x]
    List rest <- mapProc [func, List xs]
    return $ List (result : rest)
mapProc badArgList = throwError $ NumArgs 2 badArgList

forEachProc :: [LispVal] -> IOThrowsError LispVal
forEachProc [_func, List []] = return $ Bool True
forEachProc [func, List (x:xs)] = do
    _ <- apply func [x]
    forEachProc [func, List xs]
forEachProc [_, badArg] = throwError $ TypeMismatch "list" badArg
forEachProc badArgList = throwError $ NumArgs 2 badArgList

filterProc :: [LispVal] -> IOThrowsError LispVal
filterProc [_predFunc, List []] = return $ List []
filterProc [predFunc, List (x:xs)] = do
    result <- apply predFunc [x]
    rest <- filterProc [predFunc, List xs]
    case rest of
        List rs -> case result of
            Bool False -> return $ List rs
            _ -> return $ List (x : rs)
        _ -> throwError $ Default "filter: internal error"
filterProc [_, badArg] = throwError $ TypeMismatch "list" badArg
filterProc badArgList = throwError $ NumArgs 2 badArgList

evalProc :: [LispVal] -> IOThrowsError LispVal
evalProc [val] = do
    env <- liftIO primitiveBindings
    eval env val
evalProc badArgList = throwError $ NumArgs 1 badArgList

displayProc :: [LispVal] -> IOThrowsError LispVal
displayProc [val] = do
    liftIO $ putStr $ case val of
        String s -> processEscapes s
        _ -> showVal val
    liftIO $ hFlush stdout
    return $ Bool True
  where
    processEscapes [] = []
    processEscapes ('\\':'n':rest) = '\n' : processEscapes rest
    processEscapes ('\\':'t':rest) = '\t' : processEscapes rest
    processEscapes ('\\':'r':rest) = '\r' : processEscapes rest
    processEscapes ('\\':'\\':rest) = '\\' : processEscapes rest
    processEscapes (c:rest) = c : processEscapes rest
displayProc badArgList = throwError $ NumArgs 1 badArgList

newlineProc :: [LispVal] -> IOThrowsError LispVal
newlineProc [] = do
    liftIO $ putStrLn ""
    return $ Bool True
newlineProc badArgList = throwError $ NumArgs 0 badArgList

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = do
    input <- liftIO getLine
    liftThrows $ readExpr input
readProc badArgList = throwError $ NumArgs 0 badArgList

-- | 破壊的リスト操作 - MutablePair対応
setCarProc :: [LispVal] -> IOThrowsError LispVal
setCarProc [MutablePair carRef _, newCar] = do
    liftIO $ writeIORef carRef newCar
    return $ Bool True
setCarProc [List _, _] = throwError $ Default "set-car! requires a mutable pair - use mcons to create one"
setCarProc [DottedList _ _, _] = throwError $ Default "set-car! requires a mutable pair - use mcons to create one"
setCarProc [notPair, _] = throwError $ TypeMismatch "pair" notPair
setCarProc badArgList = throwError $ NumArgs 2 badArgList

setCdrProc :: [LispVal] -> IOThrowsError LispVal
setCdrProc [MutablePair _ cdrRef, newCdr] = do
    liftIO $ writeIORef cdrRef newCdr
    return $ Bool True
setCdrProc [List _, _] = throwError $ Default "set-cdr! requires a mutable pair - use mcons to create one"
setCdrProc [DottedList _ _, _] = throwError $ Default "set-cdr! requires a mutable pair - use mcons to create one"
setCdrProc [notPair, _] = throwError $ TypeMismatch "pair" notPair
setCdrProc badArgList = throwError $ NumArgs 2 badArgList

-- | mcons - 可変ペアの作成
mconsProc :: [LispVal] -> IOThrowsError LispVal
mconsProc [car', cdr'] = do
    carRef <- liftIO $ newIORef car'
    cdrRef <- liftIO $ newIORef cdr'
    return $ MutablePair carRef cdrRef
mconsProc badArgList = throwError $ NumArgs 2 badArgList

-- | mcar - 可変ペアのcar
mcarProc :: [LispVal] -> IOThrowsError LispVal
mcarProc [MutablePair carRef _] = liftIO $ readIORef carRef
mcarProc [notPair] = throwError $ TypeMismatch "mutable-pair" notPair
mcarProc badArgList = throwError $ NumArgs 1 badArgList

-- | mcdr - 可変ペアのcdr
mcdrProc :: [LispVal] -> IOThrowsError LispVal
mcdrProc [MutablePair _ cdrRef] = liftIO $ readIORef cdrRef
mcdrProc [notPair] = throwError $ TypeMismatch "mutable-pair" notPair
mcdrProc badArgList = throwError $ NumArgs 1 badArgList

-- | ベクター操作
makeVectorProc :: [LispVal] -> IOThrowsError LispVal
makeVectorProc args = do
    ref <- liftIO $ newIORef args
    return $ Vector ref

makeVectorInitProc :: [LispVal] -> IOThrowsError LispVal
makeVectorInitProc [Number n] = makeVectorInitProc [Number n, Bool False]
makeVectorInitProc [Number n, init'] = do
    let size = fromInteger n
    ref <- liftIO $ newIORef (replicate size init')
    return $ Vector ref
makeVectorInitProc badArgList = throwError $ NumArgs 1 badArgList

vectorRefProc :: [LispVal] -> IOThrowsError LispVal
vectorRefProc [Vector ref, Number n] = do
    vec <- liftIO $ readIORef ref
    let idx = fromInteger n
    if idx >= 0 && idx < length vec
        then return $ vec !! idx
        else throwError $ Default $ "vector-ref: index out of bounds: " ++ show n
vectorRefProc [badArg, _] = throwError $ TypeMismatch "vector" badArg
vectorRefProc badArgList = throwError $ NumArgs 2 badArgList

vectorSetProc :: [LispVal] -> IOThrowsError LispVal
vectorSetProc [Vector ref, Number n, newValue] = do
    vec <- liftIO $ readIORef ref
    let idx = fromInteger n
    if idx >= 0 && idx < length vec
        then do
            let before = take idx vec
            let after = drop (idx + 1) vec
            liftIO $ writeIORef ref (before ++ [newValue] ++ after)
            return $ Bool True
        else throwError $ Default $ "vector-set!: index out of bounds: " ++ show n
vectorSetProc [badArg, _, _] = throwError $ TypeMismatch "vector" badArg
vectorSetProc badArgList = throwError $ NumArgs 3 badArgList

vectorLengthProc :: [LispVal] -> IOThrowsError LispVal
vectorLengthProc [Vector ref] = do
    vec <- liftIO $ readIORef ref
    return $ Number $ toInteger $ length vec
vectorLengthProc [badArg] = throwError $ TypeMismatch "vector" badArg
vectorLengthProc badArgList = throwError $ NumArgs 1 badArgList

-- | ファイルI/O操作
openInputFileProc :: [LispVal] -> IOThrowsError LispVal
openInputFileProc [String filename] = do
    h <- liftIO $ openFile filename ReadMode
    return $ Port h InputPort
openInputFileProc [notString] = throwError $ TypeMismatch "string" notString
openInputFileProc badArgList = throwError $ NumArgs 1 badArgList

openOutputFileProc :: [LispVal] -> IOThrowsError LispVal
openOutputFileProc [String filename] = do
    h <- liftIO $ openFile filename WriteMode
    return $ Port h OutputPort
openOutputFileProc [notString] = throwError $ TypeMismatch "string" notString
openOutputFileProc badArgList = throwError $ NumArgs 1 badArgList

closePortProc :: [LispVal] -> IOThrowsError LispVal
closePortProc [Port h _] = do
    liftIO $ hClose h
    return $ Bool True
closePortProc [notPort] = throwError $ TypeMismatch "port" notPort
closePortProc badArgList = throwError $ NumArgs 1 badArgList

readCharProc :: [LispVal] -> IOThrowsError LispVal
readCharProc [] = do
    c <- liftIO getChar
    return $ Char c
readCharProc [Port h InputPort] = do
    eof <- liftIO $ hIsEOF h
    if eof
        then return $ Atom "eof"
        else do
            c <- liftIO $ hGetChar h
            return $ Char c
readCharProc [Port _ OutputPort] = throwError $ Default "read-char: port is not an input port"
readCharProc [notPort] = throwError $ TypeMismatch "port" notPort
readCharProc badArgList = throwError $ NumArgs 1 badArgList

writeCharProc :: [LispVal] -> IOThrowsError LispVal
writeCharProc [Char c] = do
    liftIO $ putChar c
    return $ Bool True
writeCharProc [Char c, Port h OutputPort] = do
    liftIO $ hPutChar h c
    return $ Bool True
writeCharProc [Char _, Port _ InputPort] = throwError $ Default "write-char: port is not an output port"
writeCharProc [notChar, _] = throwError $ TypeMismatch "char" notChar
writeCharProc badArgList = throwError $ NumArgs 2 badArgList

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [val] = do
    liftIO $ putStr $ showVal val
    return $ Bool True
writeProc [val, Port h OutputPort] = do
    liftIO $ hPutStr h $ showVal val
    return $ Bool True
writeProc [_, Port _ InputPort] = throwError $ Default "write: port is not an output port"
writeProc badArgList = throwError $ NumArgs 1 badArgList

eofObjectProc :: [LispVal] -> IOThrowsError LispVal
eofObjectProc [Atom "eof"] = return $ Bool True
eofObjectProc [_] = return $ Bool False
eofObjectProc badArgList = throwError $ NumArgs 1 badArgList

-- | 可変文字列操作
makeMutableStringProc :: [LispVal] -> IOThrowsError LispVal
makeMutableStringProc [String s] = do
    ref <- liftIO $ newIORef s
    return $ MutableString ref
makeMutableStringProc [notString] = throwError $ TypeMismatch "string" notString
makeMutableStringProc badArgList = throwError $ NumArgs 1 badArgList

stringSetProc :: [LispVal] -> IOThrowsError LispVal
stringSetProc [MutableString ref, Number idx, Char c] = do
    str <- liftIO $ readIORef ref
    let i = fromInteger idx
    if i >= 0 && i < length str
        then do
            let before = take i str
            let after = drop (i + 1) str
            liftIO $ writeIORef ref (before ++ [c] ++ after)
            return $ Bool True
        else throwError $ Default $ "string-set!: index out of bounds: " ++ show idx
stringSetProc [String _, _, _] = throwError $ Default "string-set! requires a mutable string - use make-mutable-string"
stringSetProc [notString, _, _] = throwError $ TypeMismatch "mutable-string" notString
stringSetProc badArgList = throwError $ NumArgs 3 badArgList

mutableStringRefProc :: [LispVal] -> IOThrowsError LispVal
mutableStringRefProc [MutableString ref, Number idx] = do
    str <- liftIO $ readIORef ref
    let i = fromInteger idx
    if i >= 0 && i < length str
        then return $ Char (str !! i)
        else throwError $ Default $ "mutable-string-ref: index out of bounds: " ++ show idx
mutableStringRefProc [notString, _] = throwError $ TypeMismatch "mutable-string" notString
mutableStringRefProc badArgList = throwError $ NumArgs 2 badArgList

mutableStringToStringProc :: [LispVal] -> IOThrowsError LispVal
mutableStringToStringProc [MutableString ref] = do
    str <- liftIO $ readIORef ref
    return $ String str
mutableStringToStringProc [notString] = throwError $ TypeMismatch "mutable-string" notString
mutableStringToStringProc badArgList = throwError $ NumArgs 1 badArgList

-- | 数学関数
sqrtFunc :: [LispVal] -> ThrowsError LispVal
sqrtFunc [Number n] = return $ Float (sqrt $ fromIntegral n)
sqrtFunc [Float f] = return $ Float (sqrt f)
sqrtFunc [Rational n d] = return $ Float (sqrt $ fromIntegral n / fromIntegral d)
sqrtFunc [notNum] = throwError $ TypeMismatch "number" notNum
sqrtFunc badArgList = throwError $ NumArgs 1 badArgList

exptFunc :: [LispVal] -> ThrowsError LispVal
exptFunc [Number base, Number exp'] = return $ Number (base ^ exp')
exptFunc [base, exp'] = do
    b <- toDouble base
    e <- toDouble exp'
    return $ Float (b ** e)
exptFunc badArgList = throwError $ NumArgs 2 badArgList

sinFunc :: [LispVal] -> ThrowsError LispVal
sinFunc [n] = toDouble n >>= return . Float . sin
sinFunc badArgList = throwError $ NumArgs 1 badArgList

cosFunc :: [LispVal] -> ThrowsError LispVal
cosFunc [n] = toDouble n >>= return . Float . cos
cosFunc badArgList = throwError $ NumArgs 1 badArgList

tanFunc :: [LispVal] -> ThrowsError LispVal
tanFunc [n] = toDouble n >>= return . Float . tan
tanFunc badArgList = throwError $ NumArgs 1 badArgList

logFunc :: [LispVal] -> ThrowsError LispVal
logFunc [n] = toDouble n >>= return . Float . log
logFunc badArgList = throwError $ NumArgs 1 badArgList

expFunc :: [LispVal] -> ThrowsError LispVal
expFunc [n] = toDouble n >>= return . Float . exp
expFunc badArgList = throwError $ NumArgs 1 badArgList

-- | リスト関数
listRefFunc :: [LispVal] -> ThrowsError LispVal
listRefFunc [List lst, Number idx] =
    let i = fromInteger idx
    in if i >= 0 && i < length lst
       then return $ lst !! i
       else throwError $ Default $ "list-ref: index out of bounds: " ++ show idx
listRefFunc [notList, _] = throwError $ TypeMismatch "list" notList
listRefFunc badArgList = throwError $ NumArgs 2 badArgList

listTailFunc :: [LispVal] -> ThrowsError LispVal
listTailFunc [List lst, Number n] =
    let i = fromInteger n
    in if i >= 0 && i <= length lst
       then return $ List (drop i lst)
       else throwError $ Default $ "list-tail: index out of bounds: " ++ show n
listTailFunc [notList, _] = throwError $ TypeMismatch "list" notList
listTailFunc badArgList = throwError $ NumArgs 2 badArgList

-- | 型変換関数
numberToString :: [LispVal] -> ThrowsError LispVal
numberToString [Number n] = return $ String (show n)
numberToString [Float f] = return $ String (show f)
numberToString [Rational n d] = return $ String (show n ++ "/" ++ show d)
numberToString [Complex r i] = return $ String (show r ++ "+" ++ show i ++ "i")
numberToString [notNum] = throwError $ TypeMismatch "number" notNum
numberToString badArgList = throwError $ NumArgs 1 badArgList

stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [String s] =
    case reads s :: [(Integer, String)] of
        [(n, "")] -> return $ Number n
        _ -> case reads s :: [(Double, String)] of
            [(f, "")] -> return $ Float f
            _ -> return $ Bool False  -- Schemeでは変換失敗時は#fを返す
stringToNumber [notString] = throwError $ TypeMismatch "string" notString
stringToNumber badArgList = throwError $ NumArgs 1 badArgList

-- | load関数: ファイルからSchemeコードを読み込んで実行
loadProc :: [LispVal] -> IOThrowsError LispVal
loadProc [String filename] = do
    env <- liftIO primitiveBindings
    contents <- liftIO $ readFile filename
    let exprs = lines contents
    results <- mapM (\line -> liftThrows (readExpr line) >>= eval env) (filter (not . null) exprs)
    return $ if null results then Unspecified else last results
loadProc [notString] = throwError $ TypeMismatch "string" notString
loadProc badArgList = throwError $ NumArgs 1 badArgList

