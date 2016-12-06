module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio
import Data.Array

-- Value Definitions
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)

instance Show LispVal where show = showVal

-- Parser

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (parseBSSymbol <|> noneOf "\"\\")
    char '"'
    return $ String x

parseBSSymbol :: Parser Char
parseBSSymbol = do
    x<- char '\\' >> oneOf "nrt\"\\"
    return $ case x of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    string "#"
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber :: Parser LispVal
-- parseNumber = do
--     n <- many1 digit
--     return $ Number $ read n

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

parseDigital1 :: Parser LispVal
parseDigital1 = do
    x <- many1 digit
    (return . Number .read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do
    string "#d"
    x <- many1 digit
    (return .Number .read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "10")
    return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
    let old = 2 * digint + (if x == '0' then 0 else 1) in
        bin2dig' old xs

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do
            x <- anyChar
            notFollowedBy alphaNum
            return [x]
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseNumber)
    char '+'
    y <- (try parseFloat <|> parseNumber)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquate", x]

parseVector :: Parser LispVal
parseVector = do
    arrayValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseComplex
    <|> try parseQuoted
    <|> try parseQuasiQuoted
    <|> try parseUnQuote
    <|> try (do
        string "#("
        x <- parseVector
        char ')'
        return x)
    <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

readExpr :: String -> LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left  err -> String $ "No match: " ++ show err
        Right val -> val

-- Evaluators

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [("+", numericBinop (+))
    ,("-", numericBinop (-))
    ,("/", numericBinop div)
    ,("mod", numericBinop mod)
    ,("quotient", numericBinop quot)
    ,("remainder", numericBinop rem)
    ,("symbol?", unaryOp symbolp)
    ,("string?", unaryOp stringp)
    ,("number?", unaryOp numberp)
    ,("bool?", unaryOp boolp)
    ,("list?", unaryOp listp)
    ,("symbol->string", unaryOp symbol2string)
    ,("string->symbol", unaryOp string2symbol)
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- unpackNum (String n) =
--     let parsed = reads n
--     in if null parsed
--          then 0
--          else fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp fn [v] = fn v

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False
stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False
numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False
boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False
listp :: LispVal -> LispVal
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

symbol2string :: LispVal -> LispVal
symbol2string (Atom v) = String v
symbol2string _ = String ""
string2symbol :: LispVal -> LispVal
string2symbol (String v) = Atom v
string2symbol _ = Atom ""

main :: IO ()
main = do
    args <- getArgs
    print . eval . readExpr . head $ args


