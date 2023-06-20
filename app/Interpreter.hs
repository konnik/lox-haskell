module Interpreter where

import Ast

data Value
    = StrVal String
    | NumVal Float
    | BoolVal Bool
    | NilVal
    deriving (Show, Eq)

eval :: Expr -> Either String Value
eval expr =
    case expr of
        Literal litval -> Right $ evalLiteral litval
        Unary op expr2 -> evalUnary op expr2
        Grouping expr2 -> eval expr2
        Binary op lhs rhs -> evalBinary op lhs rhs

evalBinary :: BinaryOp -> Expr -> Expr -> Either String Value
evalBinary op lhs rhs = do
    leftVal <- eval lhs
    rightVal <- eval rhs

    case op of
        Addition -> evalAdditionOrStringConcatination leftVal rightVal
        Subtraction -> evalArithmetic (-) leftVal rightVal
        Multiplication -> evalArithmetic (*) leftVal rightVal
        Division -> evalArithmetic (/) leftVal rightVal
        LessOrEqual -> evalComparison (<=) leftVal rightVal
        LessThan -> evalComparison (<) leftVal rightVal
        GreaterOrEqual -> evalComparison (>=) leftVal rightVal
        GreaterThan -> evalComparison (>) leftVal rightVal
        Equal -> evalEqual id leftVal rightVal
        NotEqual -> evalEqual not leftVal rightVal

evalAdditionOrStringConcatination :: Value -> Value -> Either String Value
evalAdditionOrStringConcatination lhs rhs =
    case (lhs, rhs) of
        (NumVal _, NumVal _) -> evalArithmetic (+) lhs rhs
        (StrVal a, StrVal b) -> pure $ StrVal (a ++ b)
        _ -> Left $ "Plus operator can only be used on two numbers or two strings. " ++ show (lhs, rhs)

evalArithmetic :: (Float -> Float -> Float) -> Value -> Value -> Either String Value
evalArithmetic func lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ NumVal (func a b)
        _ -> Left $ "Arithmetic operations is only allowed betweeen two numbers. " ++ show (lhs, rhs)

evalComparison :: (Float -> Float -> Bool) -> Value -> Value -> Either String Value
evalComparison comparison lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ BoolVal (comparison a b)
        _ -> Left $ "Only two numbers can be compared." ++ show (lhs, rhs)

evalEqual :: (Bool -> Bool) -> Value -> Value -> Either String Value
evalEqual modifier lhs rhs =
    pure $ BoolVal $ modifier $ case (lhs, rhs) of
        (NilVal, NilVal) -> True
        (NilVal, _) -> False
        (_, NilVal) -> False
        (StrVal a, StrVal b) -> (a == b)
        (NumVal a, NumVal b) -> (a == b)
        (BoolVal a, BoolVal b) -> (a == b)
        _ -> False

evalUnary :: UnaryOp -> Expr -> Either String Value
evalUnary op expr = do
    val <- eval expr

    case (op, val) of
        (Negate, NumVal n) -> Right $ NumVal (-n)
        (Negate, _) -> Left $ "Only numbers kan be negated."
        (Not, _) -> Right $ BoolVal (not (isTruthy val))

evalLiteral :: LiteralValue -> Value
evalLiteral litval =
    case litval of
        LiteralString val -> StrVal val
        LiteralNumber val -> NumVal val
        LiteralTrue -> BoolVal True
        LiteralFalse -> BoolVal False
        LiteralNil -> NilVal

isTruthy :: Value -> Bool
isTruthy val =
    case val of
        BoolVal False -> False
        NilVal -> False
        _ -> True

isFalsy :: Value -> Bool
isFalsy = not . isTruthy
