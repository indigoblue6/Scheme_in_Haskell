module Main (main) where

import LispVal
import Parser
import Eval
import System.Environment
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

-- | 1つの式を評価して文字列を返す
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

-- | 複数の式を評価して最後の結果を返す
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | Scheme関数名と特殊形式のリスト(タブ補完用)
schemeKeywords :: [String]
schemeKeywords = 
    -- 特殊形式
    [ "define", "lambda", "if", "quote", "set!", "let", "let*", "letrec"
    , "cond", "case", "and", "or", "begin"
    , "delay", "force"
    , "call/cc", "call-with-current-continuation"
    , "define-syntax", "syntax-rules"
    
    -- リスト操作
    , "car", "cdr", "cons", "list", "length", "reverse", "append"
    , "caar", "cadr", "cdar", "cddr"
    , "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr"
    , "null?", "pair?", "list?"
    , "member", "memq", "assoc", "assq"
    
    -- 可変ペア操作
    , "mcons", "mcar", "mcdr", "set-car!", "set-cdr!"
    
    -- 算術演算
    , "+", "-", "*", "/", "mod", "quotient", "remainder"
    , "abs", "max", "min"
    , "even?", "odd?", "zero?", "positive?", "negative?"
    
    -- 比較演算
    , "=", "<", ">", "<=", ">=", "/="
    , "eq?", "eqv?", "equal?"
    
    -- 文字列操作
    , "string=?", "string<?", "string>?", "string<=?", "string>=?"
    , "string-length", "string-append", "string-ref", "substring"
    , "string->list", "list->string", "string->symbol", "symbol->string"
    , "string?"
    , "make-mutable-string", "string-set!", "mutable-string-ref", "mutable-string->string"
    
    -- 文字操作
    , "char->integer", "integer->char", "char?"
    
    -- ベクター操作
    , "vector", "make-vector", "vector-ref", "vector-set!", "vector-length"
    , "vector?"
    
    -- 型判定
    , "number?", "symbol?", "procedure?", "boolean?"
    
    -- 高階関数
    , "apply", "map", "filter", "for-each"
    
    -- I/O
    , "display", "newline", "read", "load"
    , "open-input-file", "open-output-file"
    , "close-input-port", "close-output-port"
    , "read-char", "write-char", "write", "eof-object?"
    
    -- 数学関数
    , "sqrt", "expt", "sin", "cos", "tan", "log", "exp"
    
    -- リスト関数拡張
    , "list-ref", "list-tail"
    
    -- 型変換
    , "number->string", "string->number"
    
    -- メタプログラミング
    , "eval", "error"
    
    -- 定数
    , "#t", "#f"
    ]

-- | タブ補完の設定
schemeCompletion :: CompletionFunc IO
schemeCompletion = completeWord Nothing " \t()'" $ \str -> do
    let matches = filter (str `isPrefixOf`) schemeKeywords
    return $ map simpleCompletion matches
  where
    isPrefixOf prefix word = prefix == take (length prefix) word

-- | Haskellineの設定
replSettings :: Settings IO
replSettings = Settings
    { complete = schemeCompletion
    , historyFile = Just ".scheme_history"
    , autoAddHistory = True
    }

-- | 括弧のバランスをチェック
isBalanced :: String -> Bool
isBalanced = check 0
  where
    check :: Int -> String -> Bool
    check 0 [] = True
    check _ [] = False
    check n ('(':xs) = n >= 0 && check (n + 1) xs
    check n (')':xs) = n > 0 && check (n - 1) xs
    check n ('"':xs) = case checkString xs of
                         Nothing -> False
                         Just rest -> check n rest
    check n (_:xs) = check n xs
    
    checkString :: String -> Maybe String
    checkString [] = Nothing
    checkString ('\\':_:xs) = checkString xs
    checkString ('"':xs) = Just xs
    checkString (_:xs) = checkString xs

-- | 複数行入力を読み取る
readMultiLine :: String -> InputT IO (Maybe String)
readMultiLine prompt = do
    minput <- getInputLine prompt
    case minput of
        Nothing -> return Nothing
        Just line -> collectLines [line]
  where
    collectLines :: [String] -> InputT IO (Maybe String)
    collectLines lines' = 
        let combined = unlines (reverse lines')
        in if isBalanced combined
           then return $ Just combined
           else do
               mnext <- getInputLine "      ... "
               case mnext of
                   Nothing -> return $ Just combined  -- Ctrl-Dで現在の入力を返す
                   Just "" -> collectLines ("" : lines')  -- 空行も含める
                   Just next -> collectLines (next : lines')

-- | REPL(Read-Eval-Print Loop) - Haskeline版
runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    runInputT replSettings $ loop env
  where
    loop :: Env -> InputT IO ()
    loop env = do
        minput <- readMultiLine "Scheme>>> "
        case minput of
            Nothing -> return ()  -- Ctrl-D
            Just input 
                | all (`elem` " \t\n") input -> loop env  -- 空白のみは無視
                | trim input == "quit" -> return ()
                | otherwise -> do
                    result <- liftIO $ evalString env input
                    outputStrLn result
                    loop env
    
    trim = unwords . words  -- 前後の空白を削除

-- | 1つの式を評価
runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

-- | メイン関数
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runRepl
        [expr] -> runOne expr
        _ -> putStrLn "Usage: scheme-interpreter [expression]"
