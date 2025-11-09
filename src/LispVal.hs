module LispVal
    ( LispVal(..)
    , LispError(..)
    , PortType(..)
    , ThrowsError
    , IOThrowsError
    , Env
    , showVal
    , showError
    , trapError
    , extractValue
    , liftThrows
    , runIOThrows
    , nullEnv
    , isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    ) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (ParseError)
import qualified System.IO as IO

-- | Scheme値の型定義
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | MutablePair (IORef LispVal) (IORef LispVal)  -- 可変ペア
             | Number Integer
             | Float Double
             | Rational Integer Integer  -- 分子 分母
             | Complex Double Double      -- 実部 虚部
             | String String
             | MutableString (IORef [Char])  -- 可変文字列
             | Bool Bool
             | Char Char
             | Vector (IORef [LispVal])
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env
                    }
             | Continuation { cont :: LispVal -> IOThrowsError LispVal }
             | Promise { forced :: IORef Bool, value :: IORef LispVal, thunk :: LispVal }
             | Port { handle :: IO.Handle, portType :: PortType }
             | Macro { patterns :: [LispVal], templates :: [LispVal] }  -- マクロ
             | Unspecified  -- 未定義値（ifの2引数形式などで使用）

data PortType = InputPort | OutputPort deriving (Eq, Show)

-- | エラー型の定義（改善版：より詳細な情報を含む）
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               | Continue LispVal  -- 継続を呼び出すための特殊な例外
               | DivisionByZero
               | StackOverflow String  -- スタックトレース付き
               | RuntimeError String String  -- (コンテキスト, メッセージ)

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO
type Env = IORef (Map.Map String (IORef LispVal))

-- | Scheme値を文字列に変換
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (MutableString _) = "<mutable-string>"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational num denom) = show num ++ "/" ++ show denom
showVal (Complex real imag) = show real ++ "+" ++ show imag ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char c) = "#\\" ++ [c]
showVal (Vector _) = "<vector>"
showVal (MutablePair _ _) = "<mutable-pair>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<io-primitive>"
showVal (Continuation _) = "<continuation>"
showVal (Promise _ _ _) = "<promise>"
showVal (Port _ InputPort) = "<input-port>"
showVal (Port _ OutputPort) = "<output-port>"
showVal (Macro _ _) = "<macro>"
showVal Unspecified = "<unspecified>"
showVal Func {params = args, vararg = varargs, body = _, closure = _} =
    "(lambda (" ++ unwords args ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where
    show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | エラーメッセージを文字列に変換（改善版）
showError :: LispError -> String
showError (UnboundVar message varname) = "Error: " ++ message ++ ": `" ++ varname ++ "`"
showError (BadSpecialForm message form) = "Error: " ++ message ++ ": " ++ show form
showError (NotFunction message func) = "Error: " ++ message ++ ": " ++ show func
showError (NumArgs expected found) = "Error: Expected " ++ show expected
                                  ++ " argument(s), but got " ++ show (length found)
                                  ++ "\n  Arguments: " ++ unwordsList found
showError (TypeMismatch expected found) = "Error: Type mismatch\n"
                                       ++ "  Expected: " ++ expected ++ "\n"
                                       ++ "  Got: " ++ show found ++ " (type: " ++ typeOf found ++ ")"
showError (Parser parseErr) = "Parse error: " ++ show parseErr
showError (Default message) = "Error: " ++ message
showError (Continue _) = "Error: Continuation invoked outside of call/cc context"
showError DivisionByZero = "Error: Division by zero"
showError (StackOverflow trace) = "Error: Stack overflow\n" ++ trace
showError (RuntimeError context message) = "Runtime Error in " ++ context ++ ":\n  " ++ message

-- | 値の型名を取得
typeOf :: LispVal -> String
typeOf (Atom _) = "symbol"
typeOf (List _) = "list"
typeOf (DottedList _ _) = "dotted-list"
typeOf (Number _) = "integer"
typeOf (Float _) = "float"
typeOf (Rational _ _) = "rational"
typeOf (Complex _ _) = "complex"
typeOf (String _) = "string"
typeOf (MutableString _) = "mutable-string"
typeOf (Bool _) = "boolean"
typeOf (Char _) = "character"
typeOf (Vector _) = "vector"
typeOf (PrimitiveFunc _) = "primitive-function"
typeOf (Func _ _ _ _) = "function"
typeOf (IOFunc _) = "io-function"
typeOf (Port _ _) = "port"
typeOf (Promise _ _ _) = "promise"
typeOf (Continuation _) = "continuation"
typeOf (Macro _ _) = "macro"
typeOf (MutablePair _ _) = "mutable-pair"
typeOf Unspecified = "unspecified"

instance Show LispError where
    show = showError

-- | エラーハンドリング
trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Unexpected Left value"

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- | 環境の操作
nullEnv :: IO Env
nullEnv = newIORef Map.empty

isBound :: Env -> String -> IO Bool
isBound envRef var = do
    env <- readIORef envRef
    return $ Map.member var env

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Nothing -> throwError $ UnboundVar "Getting an unbound variable" var
        Just varRef -> liftIO $ readIORef varRef

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var newValue = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Nothing -> throwError $ UnboundVar "Setting an unbound variable" var
        Just varRef -> liftIO $ writeIORef varRef newValue
    return newValue

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var newValue = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var newValue
        else liftIO $ do
            varRef <- newIORef newValue
            env <- readIORef envRef
            writeIORef envRef (Map.insert var varRef env)
            return newValue

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv binds env = fmap (Map.union env . Map.fromList) (mapM addBinding binds)
        addBinding (var, bindValue) = do
            ref <- newIORef bindValue
            return (var, ref)
