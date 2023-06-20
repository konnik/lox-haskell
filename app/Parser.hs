{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Parser (parse) where

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
parse :: [Token] -> Either String (Expr, [Token])
parse tokens = do
    expression tokens

expression :: [Token] -> Either String (Expr, [Token])
expression tokens =
    equality tokens

equality :: [Token] -> Either String (Expr, [Token])
equality tokens = do
    (expr, rest) <- comparison tokens
    equalityLoop expr rest

equalityLoop :: Expr -> [Token] -> Either String (Expr, [Token])
equalityLoop expr1 tokens =
    case tokens of
        [] -> Right (expr1, tokens)
        next : rest -> case next.type_ of
            BANG_EQUAL -> do
                (expr2, rest2) <- comparison rest
                equalityLoop (Binary expr1 NotEqual expr2) rest2
            EQUAL_EQUAL -> do
                (expr2, rest2) <- comparison rest
                equalityLoop (Binary expr1 Equal expr2) rest2
            _ -> Right (expr1, tokens)

comparison :: [Token] -> Either String (Expr, [Token])
comparison tokens = do
    (expr, rest) <- term tokens
    comparisonLoop expr rest

comparisonLoop :: Expr -> [Token] -> Either String (Expr, [Token])
comparisonLoop expr1 tokens = do
    case tokens of
        [] -> Right (expr1, tokens)
        next : rest -> case next.type_ of
            GREATER -> do
                (expr2, rest2) <- term rest
                comparisonLoop (Binary expr1 GreaterThan expr2) rest2
            GREATER_EQUAL -> do
                (expr2, rest2) <- term rest
                comparisonLoop (Binary expr1 GreaterOrEqual expr2) rest2
            LESS -> do
                (expr2, rest2) <- term rest
                comparisonLoop (Binary expr1 LessThan expr2) rest2
            LESS_EQUAL -> do
                (expr2, rest2) <- term rest
                comparisonLoop (Binary expr1 LessOrEqual expr2) rest2
            _ -> Right (expr1, tokens)

term :: [Token] -> Either String (Expr, [Token])
term tokens = do
    (expr, rest) <- factor tokens
    termLoop expr rest

termLoop :: Expr -> [Token] -> Either String (Expr, [Token])
termLoop expr1 tokens = do
    case tokens of
        [] -> Right (expr1, tokens)
        next : rest -> case next.type_ of
            MINUS -> do
                (expr2, rest2) <- factor rest
                termLoop (Binary expr1 Subtraction expr2) rest2
            PLUS -> do
                (expr2, rest2) <- factor rest
                termLoop (Binary expr1 Addition expr2) rest2
            _ -> Right (expr1, tokens)

factor :: [Token] -> Either String (Expr, [Token])
factor tokens = do
    (expr, rest) <- unary tokens
    factorLoop expr rest

factorLoop :: Expr -> [Token] -> Either String (Expr, [Token])
factorLoop expr1 tokens = do
    case tokens of
        [] -> Right (expr1, tokens)
        next : rest -> case next.type_ of
            SLASH -> do
                (expr2, rest2) <- unary rest
                factorLoop (Binary expr1 Division expr2) rest2
            STAR -> do
                (expr2, rest2) <- unary rest
                factorLoop (Binary expr1 Multiplication expr2) rest2
            _ -> Right (expr1, tokens)

unary :: [Token] -> Either String (Expr, [Token])
unary tokens = case tokens of
    [] -> Left "End of file."
    next : rest -> case next.type_ of
        BANG -> do
            (expr, rest2) <- unary rest
            Right (Unary Not expr, rest2)
        MINUS -> do
            (expr, rest2) <- unary rest
            Right (Unary Negate expr, rest2)
        _ -> primary tokens

primary :: [Token] -> Either String (Expr, [Token])
primary tokens = case tokens of
    [] -> Left "End of file."
    next : rest -> case next.type_ of
        NUMBER value -> Right $ (Literal (LiteralNumber value), rest)
        STRING value -> Right $ (Literal (LiteralString value), rest)
        TRUE -> Right $ (Literal LiteralTrue, rest)
        FALSE -> Right $ (Literal LiteralFalse, rest)
        NIL -> Right $ (Literal LiteralNil, rest)
        LEFT_PAREN -> do
            (expr, rest2) <- expression rest
            case rest2 of
                [] -> Left "Unexpected end of file."
                next2 : rest3 -> case next2.type_ of
                    RIGHT_PAREN -> Right $ (Grouping expr, rest3)
                    _ -> Left $ "Expected right paren."
        _ -> Left $ "An expression can not start with : '" ++ next.lexeme ++ "'"
