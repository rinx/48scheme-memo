module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
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

instance Show LispVal
    where show = showVal

-- Error Difinitions
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError
    where show = showError
instance Error LispError
    where
        noMsg = Default "An error has occurred"
        strMsg = Default

type ThrowsError = Either LispError

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

readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left  err -> throwError $ Parser err
        Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
    ,("=", numBoolBinop (==))
    ,("<", numBoolBinop (<))
    ,(">", numBoolBinop (>))
    ,("/=", numBoolBinop (/=))
    ,(">=", numBoolBinop (>=))
    ,("<=", numBoolBinop (<=))
    ,("&&", boolBoolBinop (&&))
    ,("||", boolBoolBinop (||))
    ,("string=?", strBoolBinop (==))
    ,("string<?", strBoolBinop (<))
    ,("string>?", strBoolBinop (>))
    ,("string<=?", strBoolBinop (<=))
    ,("string>=?", strBoolBinop (>=))
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
        then throwError $ NumArgs 2 args
        else do
            left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n
    in if null parsed
         then throwError $ TypeMismatch "number" $ String n
         else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" $ notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp fn [v] = fn v

symbolp :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp _ = return $ Bool False
stringp :: LispVal -> ThrowsError LispVal
stringp (String _) = return $ Bool True
stringp _ = return $ Bool False
numberp :: LispVal -> ThrowsError LispVal
numberp (Number _) = return $ Bool True
numberp _ = return $ Bool False
boolp :: LispVal -> ThrowsError LispVal
boolp (Bool _) = return $ Bool True
boolp _ = return $ Bool False
listp :: LispVal -> ThrowsError LispVal
listp (List _) = return $ Bool True
listp (DottedList _ _) = return $ Bool True
listp _ = return $ Bool False

symbol2string :: LispVal -> ThrowsError LispVal
symbol2string (Atom v) = return $ String v
symbol2string x = throwError $ TypeMismatch "symbol" $ x
string2symbol :: LispVal -> ThrowsError LispVal
string2symbol (String v) = return $ Atom v
string2symbol x = throwError $ TypeMismatch "string" $ x

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x),(DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) &&
    (all eqvPair $ zip arg1 arg2)
    where
        eqvPair (x1, x2) =
            case eqv [x1, x2] of
                Left err -> False
                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- Error handling
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ ", found " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled


