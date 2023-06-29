module Interpreter (run) where

import Ast
import Environment (Environment)
import Environment qualified
import Value (Value (..))
import Value qualified

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, get, gets, modify, put, runStateT)

-- TODO Read this article again and decide if this monad stack (StateT / ExceptT over IO)
-- really is the best design.... :-)
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
type Interpreter m a = ExceptT String (StateT Environment m) a

run :: [Stmt] -> IO ()
run stmts = do
    (result, env) <- runStateT (runExceptT (runWithEnv stmts)) Environment.newEnvironment
    case result of
        Right _ -> pure ()
        Left err -> do
            putStrLn ""
            putStrLn ""
            putStrLn "RUNTIME ERROR!!!"
            putStrLn err
            putStrLn ""
            Environment.printEnv env
            putStrLn ""

runWithEnv :: (MonadIO m) => [Stmt] -> Interpreter m ()
runWithEnv stmts = do
    case stmts of
        [] -> pure ()
        stmt : rest -> do
            runStmt stmt
            runWithEnv rest

runtimeError :: (Monad m) => String -> Interpreter m a
runtimeError msg = throwE msg

runStmt :: (MonadIO m) => Stmt -> Interpreter m ()
runStmt stmt = do
    case stmt of
        StmtPrint expr -> runPrintStmt expr
        StmtExpr expr -> eval expr >> pure ()
        StmtVarDecl name initExpr -> runVarDeclStmt name initExpr
        StmtBlock stmts -> runBlock stmts
        StmtIf cond thenStmt maybeElseStmt -> runIfStmt cond thenStmt maybeElseStmt
        StmtWhile cond whileStmt -> runWhileStmt cond whileStmt

runVarDeclStmt :: (MonadIO m) => String -> Expr -> Interpreter m ()
runVarDeclStmt name initExpr = do
    value <- eval initExpr
    lift $ modify $ Environment.declareVar name value

runPrintStmt :: (MonadIO m) => Expr -> Interpreter m ()
runPrintStmt expr = do
    value <- eval expr
    liftIO $ putStrLn $ Value.toString value

runBlock :: (MonadIO m) => [Stmt] -> Interpreter m ()
runBlock stmts = do
    lift $ modify Environment.enterBlock
    runWithEnv stmts
    lift $ modify Environment.leaveBlock
    pure ()

runIfStmt :: (MonadIO m) => Expr -> Stmt -> Maybe Stmt -> Interpreter m ()
runIfStmt cond thenStmt maybeElseStmt = do
    condValue <- eval cond
    case (isTruthy condValue, maybeElseStmt) of
        (True, _) -> runStmt thenStmt
        (False, Just elseStmt) -> runStmt elseStmt
        (False, Nothing) -> pure ()

runWhileStmt :: (MonadIO m) => Expr -> Stmt -> Interpreter m ()
runWhileStmt cond whileStmt = do
    condValue <- eval cond
    if isTruthy condValue
        then do
            runStmt whileStmt
            runWhileStmt cond whileStmt
        else pure ()

eval :: (Monad m) => Expr -> Interpreter m Value
eval expr =
    case expr of
        Literal litval -> evalLiteral litval
        Unary op expr2 -> evalUnary op expr2
        Grouping expr2 -> eval expr2
        Binary op lhs rhs -> evalBinary op lhs rhs
        Variable name -> evalVariable name
        Assignment name valueExpr -> evalAssignment name valueExpr
        Logic op lhs rhs -> evalLogic op lhs rhs

evalLogic :: (Monad m) => LogicOp -> Expr -> Expr -> Interpreter m Value
evalLogic op lhs rhs =
    case op of
        LogicAnd -> do
            lhsVal <- eval lhs
            if isTruthy lhsVal
                then eval rhs
                else pure lhsVal
        LogicOr -> do
            lhsVal <- eval lhs
            if isFalsy lhsVal
                then eval rhs
                else pure lhsVal

evalAssignment :: (Monad m) => String -> Expr -> Interpreter m Value
evalAssignment name expr = do
    value <- eval expr
    env <- lift get
    case Environment.setVar name value env of
        Left err -> runtimeError err
        Right env' -> do
            lift $ put env'
            pure $ value

evalVariable :: (Monad m) => String -> Interpreter m Value
evalVariable name = do
    result <- lift $ gets $ Environment.getVar name
    case result of
        Right val -> pure val
        Left err -> runtimeError err

evalBinary :: (Monad m) => BinaryOp -> Expr -> Expr -> Interpreter m Value
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

safeDivision :: (Monad m) => Value -> Value -> Interpreter m Value
safeDivision lhs rhs = case rhs of
    NumVal 0 -> runtimeError "Division by zero."
    _ -> evalArithmetic ((/)) lhs rhs

evalAdditionOrStringConcatination :: (Monad m) => Value -> Value -> Interpreter m Value
evalAdditionOrStringConcatination lhs rhs =
    case (lhs, rhs) of
        (NumVal _, NumVal _) -> evalArithmetic (+) lhs rhs
        (StrVal a, StrVal b) -> pure $ StrVal (a ++ b)
        _ -> runtimeError $ "Plus operator can only be used on two numbers or two strings. " ++ show (lhs, rhs)

evalArithmetic :: (Monad m) => (Float -> Float -> Float) -> Value -> Value -> Interpreter m Value
evalArithmetic func lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ NumVal (func a b)
        _ -> runtimeError $ "Arithmetic operations is only allowed betweeen two numbers. " ++ show (lhs, rhs)

evalComparison :: (Monad m) => (Float -> Float -> Bool) -> Value -> Value -> Interpreter m Value
evalComparison comparison lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ BoolVal (comparison a b)
        _ -> runtimeError $ "Only two numbers can be compared." ++ show (lhs, rhs)

evalEqual :: (Monad m) => (Bool -> Bool) -> Value -> Value -> Interpreter m Value
evalEqual modifier lhs rhs =
    pure $ BoolVal $ modifier $ case (lhs, rhs) of
        (NilVal, NilVal) -> True
        (NilVal, _) -> False
        (_, NilVal) -> False
        (StrVal a, StrVal b) -> (a == b)
        (NumVal a, NumVal b) -> (a == b)
        (BoolVal a, BoolVal b) -> (a == b)
        _ -> False

evalUnary :: (Monad m) => UnaryOp -> Expr -> Interpreter m Value
evalUnary op expr = do
    val <- eval expr
    case (op, val) of
        (Negate, NumVal n) -> pure $ NumVal (-n)
        (Not, _) -> pure $ BoolVal (not (isTruthy val))
        (Show, v) -> pure $ StrVal (Value.toString v)
        (Negate, _) -> runtimeError $ "Only numbers kan be negated."

evalLiteral :: (Monad m) => LiteralValue -> Interpreter m Value
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
