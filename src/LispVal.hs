module LispVal
    ( LispVal(..)
    , LispError(..)
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

-- | Scheme値の型定義
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env
                    }

-- | エラー型の定義
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO
type Env = IORef (Map.Map String (IORef LispVal))

-- | Scheme値を文字列に変換
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = _, closure = _} =
    "(lambda (" ++ unwords args ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where
    show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | エラーメッセージを文字列に変換
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = "Error: " ++ message

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
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Nothing -> throwError $ UnboundVar "Setting an unbound variable" var
        Just varRef -> liftIO $ writeIORef varRef value
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef (Map.insert var valueRef env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv binds env = fmap (Map.union env . Map.fromList) (mapM addBinding binds)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)
