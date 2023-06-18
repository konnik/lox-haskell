module Parser where

import Token (Token)

data Expr
    = Binary Expr BinaryOp Expr
    | Grouping Expr
    | Literal LiteralValue
    | Unary UnaryOp Expr

data LiteralValue
    = LiteralString String
    | LiteralNumber Float
    | LiteralTrue
    | LiteralFalse
    | LiteralNil

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

data UnaryOp
    = Negate
    | Not

parse :: [Token] -> Either String Expr
parse _tokens = undefined
