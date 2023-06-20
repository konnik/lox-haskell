{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parser (
    parse,
    peek,
    match,
    consume,
    Parser (..),
) where

import Token (Token (..), TokenType (..))

data Expr
    = Binary Expr BinaryOp Expr
    | Grouping Expr
    | Literal LiteralValue
    | Unary UnaryOp Expr
    deriving (Show)

data LiteralValue
    = LiteralString String
    | LiteralNumber Float
    | LiteralTrue
    | LiteralFalse
    | LiteralNil
    deriving (Show)

data BinaryOp
    = Equal
    | NotEqual
    | LessThan
    | LessOrEqual
    | GreaterThan
    | GreaterOrEqual
    | Addition
    | Subtraction
    | Multiplication
    | Division
    deriving (Show)

data UnaryOp
    = Negate
    | Not
    deriving (Show)

newtype Parser a = Parser {runParser :: [Token] -> ([Token], Either String a)}

instance Functor Parser where
    fmap f p = Parser $ \tokens ->
        case runParser p tokens of
            (tokens', Right v) -> (tokens', Right (f v))
            (tokens', Left err) -> (tokens', Left err)

instance Applicative Parser where
    pure value = Parser $ \tokens -> (tokens, Right value)
    (<*>) fab fa = Parser $ \tokens ->
        case runParser fab tokens of
            (tokens', Right ab) -> runParser (fmap ab fa) tokens'
            (tokens', Left err) -> (tokens', Left err)

instance Monad Parser where
    return = pure
    (>>=) ma amb = Parser $ \tokens ->
        case runParser ma tokens of
            (tokens', Right a) -> runParser (amb a) tokens'
            (tokens', Left err) -> (tokens', Left err)

unexpectedEndOfFile :: ([Token], Either String a)
unexpectedEndOfFile = ([], Left "Unexpected end of file.")

{- |
Matches and consumes a specific token type.
-}
match :: TokenType -> Parser ()
match tokenType = Parser $ \tokens ->
    case tokens of
        [] -> unexpectedEndOfFile
        t : tokens' ->
            if t.type_ == tokenType
                then (tokens', Right ())
                else (tokens, Left $ "Expected " ++ show tokenType ++ " but was " ++ show t.type_)

{- |
Returns next token without consuming it.
-}
peek :: Parser Token
peek = Parser $ \tokens ->
    case tokens of
        [] -> unexpectedEndOfFile
        t : _ -> (tokens, Right t)

{- |
Skips next token without returning it.
-}
skip :: Parser ()
skip = const () <$> consume

{- |
Consume next token and return it.
-}
consume :: Parser Token
consume = Parser $ \tokens ->
    case tokens of
        [] -> unexpectedEndOfFile
        t : tokens' -> (tokens', Right t)

{-
Lox Grammar:

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
-}
parse :: [Token] -> Either String Expr
parse tokens = do
    snd $ runParser lox tokens

parseError :: String -> Parser a
parseError msg = Parser $ \tokens -> (tokens, Left $ msg)

lox :: Parser Expr
lox = do
    expr <- expression
    match EOF
    pure $ expr

expression :: Parser Expr
expression = do
    equality

equality :: Parser Expr
equality = do
    expr <- comparison
    equalityLoop expr

equalityLoop :: Expr -> Parser Expr
equalityLoop expr1 = do
    next <- peek
    case next.type_ of
        BANG_EQUAL -> do
            skip
            expr2 <- comparison
            equalityLoop $ Binary expr1 NotEqual expr2
        EQUAL_EQUAL -> do
            skip
            expr2 <- comparison
            equalityLoop $ Binary expr1 Equal expr2
        _ -> pure expr1

comparison :: Parser Expr
comparison = do
    expr <- term
    comparisonLoop expr

comparisonLoop :: Expr -> Parser Expr
comparisonLoop expr1 = do
    next <- peek
    case next.type_ of
        GREATER -> do
            skip
            expr2 <- term
            comparisonLoop (Binary expr1 GreaterThan expr2)
        GREATER_EQUAL -> do
            skip
            expr2 <- term
            comparisonLoop (Binary expr1 GreaterOrEqual expr2)
        LESS -> do
            skip
            expr2 <- term
            comparisonLoop (Binary expr1 LessThan expr2)
        LESS_EQUAL -> do
            skip
            expr2 <- term
            comparisonLoop (Binary expr1 LessOrEqual expr2)
        _ -> pure expr1

term :: Parser Expr
term = do
    expr <- factor
    termLoop expr

termLoop :: Expr -> Parser Expr
termLoop expr1 = do
    next <- peek
    case next.type_ of
        MINUS -> do
            skip
            expr2 <- factor
            termLoop (Binary expr1 Subtraction expr2)
        PLUS -> do
            skip
            expr2 <- factor
            termLoop (Binary expr1 Addition expr2)
        _ -> pure expr1

factor :: Parser Expr
factor = do
    expr <- unary
    factorLoop expr

factorLoop :: Expr -> Parser Expr
factorLoop expr1 = do
    next <- peek
    case next.type_ of
        SLASH -> do
            skip
            expr2 <- unary
            factorLoop (Binary expr1 Division expr2)
        STAR -> do
            skip
            expr2 <- unary
            factorLoop (Binary expr1 Multiplication expr2)
        _ -> pure expr1

unary :: Parser Expr
unary = do
    next <- peek
    case next.type_ of
        BANG -> do
            skip
            expr <- unary
            pure $ Unary Not expr
        MINUS -> do
            skip
            expr <- unary
            pure $ Unary Negate expr
        _ -> primary

primary :: Parser Expr
primary = do
    next <- consume
    case next.type_ of
        NUMBER value -> pure $ Literal (LiteralNumber value)
        STRING value -> pure $ Literal (LiteralString value)
        TRUE -> pure $ Literal LiteralTrue
        FALSE -> pure $ Literal LiteralFalse
        NIL -> pure $ Literal LiteralNil
        LEFT_PAREN -> do
            expr <- expression
            match RIGHT_PAREN
            pure $ Grouping expr
        _ -> parseError $ "An expression can not start with : '" ++ next.lexeme ++ "'"
