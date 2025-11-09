module Main (main) where

import LispVal
import Parser
import Eval
import System.Environment
import System.IO
import Control.Monad

-- | 1つの式を評価して文字列を返す
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

-- | 複数の式を評価して最後の結果を返す
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | 入力が終了するまでループ
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
    result <- prompt
    unless (pred' result) $ action result >> until_ pred' prompt action

-- | プロンプトを表示して入力を読み取る
readPrompt :: String -> IO String
readPrompt prompt = do
    putStr prompt
    hFlush stdout
    getLine

-- | REPL(Read-Eval-Print Loop)
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>> ") . evalAndPrint

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
