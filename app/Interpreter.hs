module Interpreter (run) where

import Ast
import Environment (Environment)
import Environment qualified
import Value (Value (..))
import Value qualified

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, local)
import Control.Monad.Trans.State (StateT, get, gets, modify, put, runStateT)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr)

-- TODO Read these articles again and decide if this monad stack (StateT / ExceptT over IO)
-- really is the best design.... :-)
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
-- https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/
-- https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/
type Interpreter m a = ReaderT Int (ExceptT Error (StateT Environment m)) a

data Error = Error
    { line :: Int
    , description :: String
    }
    deriving (Show)

run :: Bool -> [Stmt] -> IO ()
run _debug stmts = do
    (result, _env) <- runStateT (runExceptT (runReaderT (runWithEnv stmts) 1)) Environment.newEnvironment
    case result of
        Right _ -> pure ()
        Left err -> do
            reportError err
            exitWith $ ExitFailure 70

reportError :: Error -> IO ()
reportError err = do
    hPutStrLn stderr err.description
    hPutStrLn stderr $ "[line " ++ show err.line ++ "]"

-- if debug
--     then do
--         putStrLn ""
--         putStrLn ""
--         putStrLn "RUNTIME ERROR!!!"
--     else pure ()
-- if debug
--     then do
--         putStrLn ""
--         Environment.printEnv env
--         putStrLn ""
--     else pure ()
runWithEnv :: (MonadIO m) => [Stmt] -> Interpreter m ()
runWithEnv stmts = do
    case stmts of
        [] -> pure ()
        stmt : rest -> do
            runStmt stmt
            runWithEnv rest

getCurrentLine :: (Monad m) => Interpreter m Int
getCurrentLine = do
    line <- ask
    pure line

setCurrentLine :: (Monad m) => Int -> Interpreter m a -> Interpreter m a
setCurrentLine line action = do
    local (const line) action

runtimeError :: (Monad m) => String -> Interpreter m a
runtimeError description = do
    line <- getCurrentLine
    (lift . throwE)
        Error
            { line = line
            , description = description
            }

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
    lift $ lift $ modify $ Environment.declareVar name value

runPrintStmt :: (MonadIO m) => Expr -> Interpreter m ()
runPrintStmt expr = do
    value <- eval expr
    liftIO $ putStrLn $ Value.toString value

runBlock :: (MonadIO m) => [Stmt] -> Interpreter m ()
runBlock stmts = do
    lift $ lift $ modify Environment.enterBlock
    runWithEnv stmts
    lift $ lift $ modify Environment.leaveBlock
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
        Binary line op lhs rhs -> setCurrentLine line $ evalBinary op lhs rhs
        Variable line name -> setCurrentLine line $ evalVariable name
        Assignment line name valueExpr -> setCurrentLine line $ evalAssignment name valueExpr
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
    env <- lift $ lift get
    case Environment.setVar name value env of
        Left err -> runtimeError err
        Right env' -> do
            lift $ lift $ put env'
            pure $ value

evalVariable :: (Monad m) => String -> Interpreter m Value
evalVariable name = do
    result <- lift $ lift $ gets $ Environment.getVar name
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
        _ -> runtimeError $ "Operands must be two numbers or two strings."

evalArithmetic :: (Monad m) => (Double -> Double -> Double) -> Value -> Value -> Interpreter m Value
evalArithmetic func lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ NumVal (func a b)
        _ -> runtimeError $ "Operands must be numbers."

evalComparison :: (Monad m) => (Double -> Double -> Bool) -> Value -> Value -> Interpreter m Value
evalComparison comparison lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ BoolVal (comparison a b)
        _ -> runtimeError $ "Operands must be numbers."

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
        (Negate, _) -> runtimeError $ "Operand must be a number."

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
