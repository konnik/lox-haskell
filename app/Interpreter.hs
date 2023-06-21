module Interpreter where

import Ast
import Environment (Environment)
import Environment qualified
import Value (Value (..))
import Value qualified

run :: [Stmt] -> IO ()
run stmts = do
    _ <- runWithEnv Environment.newEnvironment stmts
    pure ()

runWithEnv :: Environment -> [Stmt] -> IO Environment
runWithEnv env stmts = do
    case stmts of
        [] -> pure env
        stmt : rest -> do
            result <- runStmt env stmt
            case result of
                Right modifiedEnv ->
                    runWithEnv modifiedEnv rest
                Left err -> do
                    putStrLn $ "Runtime error: " ++ err
                    pure env

runtimeError :: String -> IO (Either String a)
runtimeError msg = pure $ Left msg

runStmt :: Environment -> Stmt -> IO (Either String Environment)
runStmt env stmt = do
    case stmt of
        StmtPrint expr ->
            case runEvaluator (eval expr) env of
                (_, Left err) -> runtimeError err
                (env', Right value) -> do
                    putStrLn $ Value.toString value
                    pure $ pure env'
        StmtExpr expr ->
            case runEvaluator (eval expr) env of
                (_, Left err) -> runtimeError err
                (env', Right _) -> do
                    pure $ pure env'
        StmtVarDecl name initExpr ->
            case runEvaluator (eval initExpr) env of
                (_, Left err) -> runtimeError err
                (env', Right initalValue) -> do
                    pure $ pure (Environment.declareVar name initalValue env')
        StmtBlock stmts -> do
            let blockEnv = Environment.enterBlock env
            blockEnv' <- runWithEnv blockEnv stmts
            pure $ pure $ Environment.leaveBlock blockEnv'
        StmtIf cond thenStmt maybeElseStmt -> do
            case runEvaluator (eval cond) env of
                (_, Left err) -> runtimeError err
                (env', Right condValue) ->
                    if isTruthy condValue
                        then runStmt env' thenStmt
                        else case maybeElseStmt of
                            Nothing -> pure $ pure env
                            Just elseStmt -> runStmt env' elseStmt

newtype Evaluator a = Evaluator {runEvaluator :: Environment -> (Environment, Either String a)}

instance Functor Evaluator where
    fmap f fa = Evaluator $ \env ->
        case runEvaluator fa env of
            (env', Right a) -> (env', Right (f a))
            (env', Left err) -> (env', Left err)

instance Applicative Evaluator where
    pure value = Evaluator $ \env -> (env, Right value)
    (<*>) fab fa = Evaluator $ \env ->
        case runEvaluator fab env of
            (env', Right ab) -> runEvaluator (fmap ab fa) env'
            (env', Left err) -> (env', Left err)

instance Monad Evaluator where
    return = pure
    (>>=) ma f = Evaluator $ \env ->
        case runEvaluator ma env of
            (env', Right a) -> runEvaluator (f a) env'
            (env', Left err) -> (env', Left err)

environment :: Evaluator Environment
environment = Evaluator $ \env -> (env, pure env)

modifyEnv :: (Environment -> Either String Environment) -> Evaluator ()
modifyEnv f = Evaluator $ \env -> case f env of
    Right env' -> (env', pure ())
    Left err -> (env, Left err)

evalError :: String -> Evaluator a
evalError msg = Evaluator $ \env -> (env, Left msg)

eval :: Expr -> Evaluator Value
eval expr =
    case expr of
        Literal litval -> evalLiteral litval
        Unary op expr2 -> evalUnary op expr2
        Grouping expr2 -> eval expr2
        Binary op lhs rhs -> evalBinary op lhs rhs
        Variable name -> evalVariable name
        Assignment name valueExpr -> evalAssignment name valueExpr
        Logic op lhs rhs -> evalLogic op lhs rhs

evalLogic :: LogicOp -> Expr -> Expr -> Evaluator Value
evalLogic op lhs rhs =
    case op of
        LogicAnd -> do
            lhsVal <- eval lhs
            if isTruthy lhsVal
                then eval rhs
                else pure lhsVal
        LogicOr -> do
            lhsVal <- eval lhs
            if isTruthy lhsVal
                then pure $ lhsVal
                else eval rhs

evalAssignment :: String -> Expr -> Evaluator Value
evalAssignment name expr = do
    value <- eval expr
    modifyEnv $ Environment.setVar name value
    pure value

evalVariable :: String -> Evaluator Value
evalVariable name = do
    env <- environment
    case Environment.getVar name env of
        Right value -> pure value
        Left err -> evalError err

evalBinary :: BinaryOp -> Expr -> Expr -> Evaluator Value
evalBinary op lhs rhs = do
    leftVal <- eval lhs
    rightVal <- eval rhs
    case op of
        Addition -> evalAdditionOrStringConcatination leftVal rightVal
        Subtraction -> evalArithmetic (-) leftVal rightVal
        Multiplication -> evalArithmetic (*) leftVal rightVal
        Division -> safeDivision leftVal rightVal
        LessOrEqual -> evalComparison (<=) leftVal rightVal
        LessThan -> evalComparison (<) leftVal rightVal
        GreaterOrEqual -> evalComparison (>=) leftVal rightVal
        GreaterThan -> evalComparison (>) leftVal rightVal
        Equal -> evalEqual id leftVal rightVal
        NotEqual -> evalEqual not leftVal rightVal

safeDivision :: Value -> Value -> Evaluator Value
safeDivision lhs rhs = case rhs of
    NumVal 0 -> evalError "Division by zero."
    _ -> evalArithmetic ((/)) lhs rhs

evalAdditionOrStringConcatination :: Value -> Value -> Evaluator Value
evalAdditionOrStringConcatination lhs rhs =
    case (lhs, rhs) of
        (NumVal _, NumVal _) -> evalArithmetic (+) lhs rhs
        (StrVal a, StrVal b) -> pure $ StrVal (a ++ b)
        _ -> evalError $ "Plus operator can only be used on two numbers or two strings. " ++ show (lhs, rhs)

evalArithmetic :: (Float -> Float -> Float) -> Value -> Value -> Evaluator Value
evalArithmetic func lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ NumVal (func a b)
        _ -> evalError $ "Arithmetic operations is only allowed betweeen two numbers. " ++ show (lhs, rhs)

evalComparison :: (Float -> Float -> Bool) -> Value -> Value -> Evaluator Value
evalComparison comparison lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ BoolVal (comparison a b)
        _ -> evalError $ "Only two numbers can be compared." ++ show (lhs, rhs)

evalEqual :: (Bool -> Bool) -> Value -> Value -> Evaluator Value
evalEqual modifier lhs rhs =
    pure $ BoolVal $ modifier $ case (lhs, rhs) of
        (NilVal, NilVal) -> True
        (NilVal, _) -> False
        (_, NilVal) -> False
        (StrVal a, StrVal b) -> (a == b)
        (NumVal a, NumVal b) -> (a == b)
        (BoolVal a, BoolVal b) -> (a == b)
        _ -> False

evalUnary :: UnaryOp -> Expr -> Evaluator Value
evalUnary op expr = do
    val <- eval expr

    case (op, val) of
        (Negate, NumVal n) -> pure $ NumVal (-n)
        (Not, _) -> pure $ BoolVal (not (isTruthy val))
        (Show, v) -> pure $ StrVal (Value.toString v)
        (Negate, _) -> evalError $ "Only numbers kan be negated."

evalLiteral :: LiteralValue -> Evaluator Value
evalLiteral litval =
    case litval of
        LiteralString val -> pure $ StrVal val
        LiteralNumber val -> pure $ NumVal val
        LiteralTrue -> pure $ BoolVal True
        LiteralFalse -> pure $ BoolVal False
        LiteralNil -> pure $ NilVal

isTruthy :: Value -> Bool
isTruthy val =
    case val of
        BoolVal False -> False
        NilVal -> False
        _ -> True

isFalsy :: Value -> Bool
isFalsy = not . isTruthy
