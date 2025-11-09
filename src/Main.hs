module Main (main) where

import LispVal
import Parser
import Eval
import System.Environment
import Control.Monad
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

-- | 1つの式を評価して文字列を返す
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

-- | 複数の式を評価して最後の結果を返す
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | Scheme関数名と特殊形式のリスト（タブ補完用）
schemeKeywords :: [String]
schemeKeywords = 
    [ "define", "lambda", "if", "quote", "set!", "car", "cdr", "cons"
    , "+", "-", "*", "/", "mod", "quotient", "remainder"
    , "=", "<", ">", "<=", ">=", "/="
    , "eq?", "eqv?", "equal?"
    , "string=?", "string<?", "string>?", "string<=?", "string>=?"
    , "&&", "||"
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
                | filter (/= '\n') input == "quit" -> return ()
                | otherwise -> do
                    result <- liftIO $ evalString env (filter (/= '\n') input)
                    outputStrLn result
                    loop env

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
