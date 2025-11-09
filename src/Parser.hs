module Parser
    ( readExpr
    , readExprList
    ) where

import LispVal
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

-- | スペースのパーサー
spaces :: Parser ()
spaces = skipMany1 space

-- | シンボルで使用可能な文字
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- | 文字列のパーサー
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"" <|> (char '\\' >> char '"'))
    _ <- char '"'
    return $ String x

-- | アトムのパーサー
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- | 数値のパーサー
parseNumber :: Parser LispVal
parseNumber = do
    digits <- many1 digit
    return $ Number (read digits)

-- | リストのパーサー
parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

-- | ドット記法のパーサー
parseDottedList :: Parser LispVal
parseDottedList = do
    hd <- endBy parseExpr spaces
    tl <- char '.' >> spaces >> parseExpr
    return $ DottedList hd tl

-- | クォートのパーサー
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- | Scheme式のパーサー
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
            _ <- char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'
            return x

-- | 文字列をScheme式にパース
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- | 複数のScheme式をパース
readExprList :: String -> ThrowsError [LispVal]
readExprList input = case parse (endBy parseExpr spaces) "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
