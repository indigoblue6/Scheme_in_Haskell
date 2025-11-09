module Parser
    ( readExpr
    , readExprList
    ) where

import LispVal
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

-- | スペースとコメントのパーサー（1つ以上）
spaces :: Parser ()
spaces = skipMany1 (space <|> comment)
  where
    comment = do
        _ <- char ';'
        _ <- manyTill anyChar (try newline <|> (eof >> return '\n'))
        return ' '

-- | スペースとコメントのパーサー（0個以上）
spaces0 :: Parser ()
spaces0 = skipMany (space <|> comment)
  where
    comment = do
        _ <- char ';'
        _ <- manyTill anyChar (try newline <|> (eof >> return '\n'))
        return ' '

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

-- | 真偽値のパーサー
parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False
        _ -> Bool False  -- unreachable

-- | アトムのパーサー
parseAtom :: Parser LispVal
parseAtom = try parseEllipsis <|> parseRegularAtom
  where
    parseEllipsis = do
        _ <- string "..."
        notFollowedBy (letter <|> digit <|> symbol)
        return $ Atom "..."
    parseRegularAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ Atom atom

-- | 数値のパーサー（整数、浮動小数点、有理数、複素数）
parseNumber :: Parser LispVal
parseNumber = try parseComplex <|> try parseRational <|> try parseFloat <|> parseInt
  where
    parseInt = do
        digits <- many1 digit
        return $ Number (read digits)
    
    parseFloat = do
        intPart <- many1 digit
        _ <- char '.'
        fracPart <- many1 digit
        return $ Float (read (intPart ++ "." ++ fracPart))
    
    parseRational = do
        num <- many1 digit
        _ <- char '/'
        denom <- many1 digit
        return $ Rational (read num) (read denom)
    
    parseComplex = do
        real <- (try parseFloatNum <|> parseIntNum)
        _ <- optional (char '+')
        imag <- (try parseFloatNum <|> parseIntNum)
        _ <- char 'i'
        return $ Complex real imag
    
    parseFloatNum = do
        intPart <- many1 digit
        _ <- char '.'
        fracPart <- many1 digit
        return $ read (intPart ++ "." ++ fracPart)
    
    parseIntNum = do
        digits <- many1 digit
        return $ fromIntegral (read digits :: Integer)

-- | 文字のパーサー
parseChar :: Parser LispVal
parseChar = do
    _ <- string "#\\"
    c <- parseCharName <|> anyChar
    return $ Char c
  where
    parseCharName = (string "space" >> return ' ')
                <|> (string "newline" >> return '\n')
                <|> (string "tab" >> return '\t')

-- | ベクターのパーサー（評価時にIORefを作成する必要があるため、リストとして解析）
parseVector :: Parser LispVal
parseVector = do
    _ <- string "#("
    spaces0
    elems <- sepEndBy parseExpr spaces
    spaces0
    _ <- char ')'
    return $ List [Atom "vector", List elems]

-- | リストまたはドット記法のパーサー
parseListOrDotted :: Parser LispVal
parseListOrDotted = do
    spaces0
    elems <- sepEndBy parseExpr spaces
    spaces0
    maybeDot <- optionMaybe (char '.')
    case maybeDot of
        Nothing -> return $ List elems
        Just _ -> do
            spaces
            tl <- parseExpr
            spaces0
            case elems of
                [] -> fail "Dotted list must have at least one element before the dot"
                _ -> return $ DottedList elems tl

-- | クォートのパーサー
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- | Scheme式のパーサー
parseExpr :: Parser LispVal
parseExpr = parseBool
        <|> parseAtom
        <|> parseString
        <|> parseChar
        <|> parseNumber
        <|> parseQuoted
        <|> parseVector
        <|> do
            _ <- char '('
            x <- parseListOrDotted
            _ <- char ')'
            return x

-- | 文字列をScheme式にパース
readExpr :: String -> ThrowsError LispVal
readExpr input = 
    let trimmed = dropWhile (`elem` " \t\n\r") input
    in if null trimmed || all isComment (lines trimmed)
       then throwError $ Default "Empty input"
       else case parse (skipWhitespace >> parseExpr) "lisp" input of
           Left err -> throwError $ Parser err
           Right val -> return val
  where
    skipWhitespace = skipMany ((space >> return ()) <|> comment)
    comment = do
        _ <- char ';'
        _ <- manyTill anyChar (try newline <|> (eof >> return '\n'))
        return ()
    isComment line = case dropWhile (`elem` " \t") line of
        (';':_) -> True
        "" -> True
        _ -> False

-- | 複数のScheme式をパース
readExprList :: String -> ThrowsError [LispVal]
readExprList input = case parse (endBy parseExpr spaces) "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
