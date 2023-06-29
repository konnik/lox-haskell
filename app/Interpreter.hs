module Interpreter where

import Ast
import Environment (Environment)
import Environment qualified
import Value (Value (..))
import Value qualified

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State

type Interpreter m a = StateT Environment m a

run :: [Stmt] -> IO ()
run stmts = do
    _ <- runStateT (runWithEnv stmts) Environment.newEnvironment
    pure ()

runWithEnv :: (MonadIO m) => [Stmt] -> Interpreter m ()
runWithEnv stmts = do
    case stmts of
        [] -> pure ()
        stmt : rest -> do
            result <- runStmt stmt
            case result of
                Right () ->
                    runWithEnv rest
                Left err -> do
                    liftIO (putStrLn $ "Runtime error: " ++ err)

runtimeError :: (Monad m) => String -> Interpreter m (Either String a)
runtimeError msg = pure $ Left msg

withValue :: (Monad m) => Expr -> (Value -> Interpreter m b) -> Interpreter m (Either String b)
withValue expr f = do
    result <- eval expr
    case result of
        Right a -> pure <$> f a
        Left err -> pure $ Left err

withValue2 :: (Monad m) => Expr -> (Value -> Interpreter m (Either String b)) -> Interpreter m (Either String b)
withValue2 expr f = do
    result <- eval expr
    case result of
        Right a -> f a
        Left err -> pure $ Left err

runStmt :: (MonadIO m) => Stmt -> Interpreter m (Either String ())
runStmt stmt = do
    case stmt of
        StmtPrint expr -> withValue expr $ \value ->
            liftIO $ putStrLn $ Value.toString value
        StmtExpr expr -> withValue expr $ \_ ->
            pure ()
        StmtVarDecl name initExpr -> withValue initExpr $ \value ->
            modify $ Environment.declareVar name value
        StmtBlock stmts -> do
            modify Environment.enterBlock
            runWithEnv stmts
            modify Environment.leaveBlock
            pure $ pure ()
        StmtIf cond thenStmt maybeElseStmt -> withValue2 cond $ \condValue ->
            if isTruthy condValue
                then runStmt thenStmt
                else case maybeElseStmt of
                    Just elseStmt -> runStmt elseStmt
                    Nothing -> pure $ pure ()
        StmtWhile cond whileStmt -> runWhileStmt cond whileStmt

runWhileStmt :: (MonadIO m) => Expr -> Stmt -> Interpreter m (Either String ())
runWhileStmt cond whileStmt = withValue2 cond $ \condValue ->
    if isTruthy condValue
        then do
            result <- runStmt whileStmt
            case result of
                Right () -> do
                    runWhileStmt cond whileStmt
                Left err -> runtimeError err
        else pure $ pure ()

eval :: (Monad m) => Expr -> Interpreter m (Either String Value)
eval expr =
    case expr of
        Literal litval -> evalLiteral litval
        Unary op expr2 -> evalUnary op expr2
        Grouping expr2 -> eval expr2
        Binary op lhs rhs -> evalBinary op lhs rhs
        Variable name -> evalVariable name
        Assignment name valueExpr -> evalAssignment name valueExpr
        Logic op lhs rhs -> evalLogic op lhs rhs

evalLogic :: (Monad m) => LogicOp -> Expr -> Expr -> Interpreter m (Either String Value)
evalLogic op lhs rhs =
    case op of
        LogicAnd -> withValue2 lhs $ \lhsVal ->
            if isTruthy lhsVal
                then eval rhs
                else pure $ pure lhsVal
        LogicOr -> withValue2 lhs $ \lhsVal ->
            if isTruthy lhsVal
                then pure $ pure lhsVal
                else eval rhs

evalAssignment :: (Monad m) => String -> Expr -> Interpreter m (Either String Value)
evalAssignment name expr = do
    result <- eval expr
    case result of
        Left err -> runtimeError err
        Right value -> do
            env <- get
            case Environment.setVar name value env of
                Left err -> runtimeError err
                Right env' -> do
                    put env'
                    pure $ pure value

evalVariable :: (Monad m) => String -> Interpreter m (Either String Value)
evalVariable name = gets $ Environment.getVar name

evalBinary :: (Monad m) => BinaryOp -> Expr -> Expr -> Interpreter m (Either String Value)
evalBinary op lhs rhs = do
    withValue2 lhs $ \leftVal ->
        withValue2 rhs $ \rightVal ->
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

safeDivision :: (Monad m) => Value -> Value -> Interpreter m (Either String Value)
safeDivision lhs rhs = case rhs of
    NumVal 0 -> runtimeError "Division by zero."
    _ -> evalArithmetic ((/)) lhs rhs

evalAdditionOrStringConcatination :: (Monad m) => Value -> Value -> Interpreter m (Either String Value)
evalAdditionOrStringConcatination lhs rhs =
    case (lhs, rhs) of
        (NumVal _, NumVal _) -> evalArithmetic (+) lhs rhs
        (StrVal a, StrVal b) -> pure $ pure $ StrVal (a ++ b)
        _ -> runtimeError $ "Plus operator can only be used on two numbers or two strings. " ++ show (lhs, rhs)

evalArithmetic :: (Monad m) => (Float -> Float -> Float) -> Value -> Value -> Interpreter m (Either String Value)
evalArithmetic func lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ pure $ NumVal (func a b)
        _ -> runtimeError $ "Arithmetic operations is only allowed betweeen two numbers. " ++ show (lhs, rhs)

evalComparison :: (Monad m) => (Float -> Float -> Bool) -> Value -> Value -> Interpreter m (Either String Value)
evalComparison comparison lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ pure $ BoolVal (comparison a b)
        _ -> runtimeError $ "Only two numbers can be compared." ++ show (lhs, rhs)

evalEqual :: (Monad m) => (Bool -> Bool) -> Value -> Value -> Interpreter m (Either String Value)
evalEqual modifier lhs rhs =
    pure $ pure $ BoolVal $ modifier $ case (lhs, rhs) of
        (NilVal, NilVal) -> True
        (NilVal, _) -> False
        (_, NilVal) -> False
        (StrVal a, StrVal b) -> (a == b)
        (NumVal a, NumVal b) -> (a == b)
        (BoolVal a, BoolVal b) -> (a == b)
        _ -> False

evalUnary :: (Monad m) => UnaryOp -> Expr -> Interpreter m (Either String Value)
evalUnary op expr = do
    withValue2 expr $ \val ->
        case (op, val) of
            (Negate, NumVal n) -> pure $ pure $ NumVal (-n)
            (Not, _) -> pure $ pure $ BoolVal (not (isTruthy val))
            (Show, v) -> pure $ pure $ StrVal (Value.toString v)
            (Negate, _) -> runtimeError $ "Only numbers kan be negated."

evalLiteral :: (Monad m) => LiteralValue -> Interpreter m (Either String Value)
evalLiteral litval =
    case litval of
        LiteralString val -> pure $ pure $ StrVal val
        LiteralNumber val -> pure $ pure $ NumVal val
        LiteralTrue -> pure $ pure $ BoolVal True
        LiteralFalse -> pure $ pure $ BoolVal False
        LiteralNil -> pure $ pure $ NilVal

isTruthy :: Value -> Bool
isTruthy val =
    case val of
        BoolVal False -> False
        NilVal -> False
        _ -> True

isFalsy :: Value -> Bool
isFalsy = not . isTruthy
