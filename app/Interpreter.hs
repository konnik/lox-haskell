module Interpreter (run) where

import Ast
import Environment qualified
import Value (Value (..))
import Value qualified

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (catchE, finallyE, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, local)
import Control.Monad.Trans.State (get, gets, modify, put, runStateT)
import Data.Function ((&))
import GHC.IO.Exception (ExitCode (ExitFailure))
import Native qualified
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr)

import Control.Monad (when)
import Types (Error (..))
import Types qualified

type Interpreter a = Types.Interpreter Value a

run :: Bool -> [Stmt] -> IO ()
run _debug stmts = do
    let initialEnvironment =
            Environment.newEnvironment
                & Environment.declareVar "clock" Native.clock
                & Environment.declareVar "readStr" Native.readStr

    (result, _env) <- runStateT (runExceptT (runReaderT (runStatements stmts) 1)) initialEnvironment
    case result of
        Right _ -> pure ()
        Left err -> do
            reportError err
            exitWith $ ExitFailure 70

reportError :: Error Value -> IO ()
reportError err = do
    case err of
        ReturnValue{value} -> do
            hPutStrLn stderr "BUG: return-value not handled"
            hPutStrLn stderr $ "[line " ++ show value ++ "]"
        Error{line, description} -> do
            hPutStrLn stderr description
            hPutStrLn stderr $ "[line " ++ show line ++ "]"

runStatements :: [Stmt] -> Interpreter ()
runStatements stmts = do
    case stmts of
        [] -> pure ()
        stmt : rest -> do
            runStmt stmt
            runStatements rest

getCurrentLine :: Interpreter Int
getCurrentLine = do
    line <- ask
    pure line

setCurrentLine :: Int -> Interpreter a -> Interpreter a
setCurrentLine line action = do
    local (const line) action

runtimeError :: String -> Interpreter a
runtimeError description = do
    line <- getCurrentLine
    (lift . throwE)
        Error
            { line = line
            , description = description
            }

runStmt :: Stmt -> Interpreter ()
runStmt stmt = do
    case stmt of
        StmtPrint expr -> runPrintStmt expr
        StmtExpr expr -> eval expr >> pure ()
        StmtVarDecl name initExpr -> runVarDeclStmt name initExpr
        StmtBlock stmts -> runBlock stmts
        StmtIf cond thenStmt maybeElseStmt -> runIfStmt cond thenStmt maybeElseStmt
        StmtWhile cond whileStmt -> runWhileStmt cond whileStmt
        StmtFunctionDecl name params body -> runFunctionDeclStmt name params body
        StmtReturn line expr -> setCurrentLine line $ runReturnStmt expr

runReturnStmt :: Expr -> Interpreter ()
runReturnStmt expr =
    eval expr >>= returnFromFunction

returnFromFunction :: Value -> Interpreter a
returnFromFunction value = do
    (lift . throwE) ReturnValue{value}

enterBlock :: Interpreter ()
enterBlock = do
    lift $ lift $ modify Environment.enterBlock

leaveBlock :: Interpreter ()
leaveBlock = do
    lift $ lift $ modify Environment.leaveBlock

runInNewEnvironment :: Interpreter a -> Interpreter a
runInNewEnvironment action = do
    enterBlock
    line <- ask
    result <- lift $ finallyE (runReaderT action line) $ do
        lift $ modify Environment.leaveBlock
    pure result

_setVar :: String -> Value -> Interpreter ()
_setVar name value = do
    env <- lift $ lift get
    case Environment.setVar name value env of
        Right env' -> lift $ lift $ put env'
        Left err -> runtimeError err

declareVar :: String -> Value -> Interpreter ()
declareVar name value = do
    env <- lift $ lift get
    let env' = Environment.declareVar name value env
    lift $ lift $ put env'

runFunctionDeclStmt :: String -> [String] -> [Stmt] -> Interpreter ()
runFunctionDeclStmt name params block = do
    let callable = CallableVal (length params) name $ \args -> do
            sequence_ $ zipWith declareVar params args
            runStatements block
            pure NilVal

    lift $ lift $ modify $ Environment.declareVar name callable

runVarDeclStmt :: String -> Expr -> Interpreter ()
runVarDeclStmt name initExpr = do
    value <- eval initExpr
    lift $ lift $ modify $ Environment.declareVar name value

runPrintStmt :: Expr -> Interpreter ()
runPrintStmt expr = do
    value <- eval expr
    liftIO $ putStrLn $ Value.toString value

runBlock :: [Stmt] -> Interpreter ()
runBlock stmts = runInNewEnvironment $ do
    runStatements stmts

runIfStmt :: Expr -> Stmt -> Maybe Stmt -> Interpreter ()
runIfStmt cond thenStmt maybeElseStmt = do
    condValue <- eval cond
    case (isTruthy condValue, maybeElseStmt) of
        (True, _) -> runStmt thenStmt
        (False, Just elseStmt) -> runStmt elseStmt
        (False, Nothing) -> pure ()

runWhileStmt :: Expr -> Stmt -> Interpreter ()
runWhileStmt cond whileStmt = do
    condValue <- eval cond
    when (isTruthy condValue) $ do
        runStmt whileStmt
        runWhileStmt cond whileStmt

eval :: Expr -> Interpreter Value
eval expr =
    case expr of
        Literal litval -> evalLiteral litval
        Unary op expr2 -> evalUnary op expr2
        Grouping expr2 -> eval expr2
        Binary line op lhs rhs -> setCurrentLine line $ evalBinary op lhs rhs
        Variable line name -> setCurrentLine line $ evalVariable name
        Assignment line name valueExpr -> setCurrentLine line $ evalAssignment name valueExpr
        Logic op lhs rhs -> evalLogic op lhs rhs
        Call line callee args -> setCurrentLine line $ evalCall callee args

evalCall :: Expr -> [Expr] -> Interpreter Value
evalCall calleeExpr arguments = do
    callee <- eval calleeExpr
    case callee of
        CallableVal arity _ func ->
            if arity /= length arguments
                then runtimeError $ "Expected " <> show arity <> " arguments but got " <> show (length arguments) <> "."
                else do
                    argValues <- mapM eval arguments
                    result <- runInNewEnvironment $ handleReturn $ func argValues
                    pure result
        _ -> runtimeError "Can only call functions and classes."

debugPrintEnv :: Interpreter ()
debugPrintEnv = do
    env <- lift $ lift get
    liftIO $ Environment.printEnv env

handleReturn :: Interpreter Value -> Interpreter Value
handleReturn action = do
    line <- ask
    lift $ catchE (runReaderT action line) $ \err -> do
        case err of
            ReturnValue{value} -> pure value
            _ -> throwE err

evalLogic :: LogicOp -> Expr -> Expr -> Interpreter Value
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

evalAssignment :: String -> Expr -> Interpreter Value
evalAssignment name expr = do
    value <- eval expr
    env <- lift $ lift get
    case Environment.setVar name value env of
        Left err -> runtimeError err
        Right env' -> do
            lift $ lift $ put env'
            pure $ value

evalVariable :: String -> Interpreter Value
evalVariable name = do
    result <- lift $ lift $ gets $ Environment.getVar name
    case result of
        Right val -> pure val
        Left err -> runtimeError err

evalBinary :: BinaryOp -> Expr -> Expr -> Interpreter Value
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

safeDivision :: Value -> Value -> Interpreter Value
safeDivision lhs rhs = case rhs of
    NumVal 0 -> runtimeError "Division by zero."
    _ -> evalArithmetic ((/)) lhs rhs

evalAdditionOrStringConcatination :: Value -> Value -> Interpreter Value
evalAdditionOrStringConcatination lhs rhs =
    case (lhs, rhs) of
        (NumVal _, NumVal _) -> evalArithmetic (+) lhs rhs
        (StrVal a, StrVal b) -> pure $ StrVal (a ++ b)
        _ -> runtimeError $ "Operands must be two numbers or two strings."

evalArithmetic :: (Double -> Double -> Double) -> Value -> Value -> Interpreter Value
evalArithmetic func lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ NumVal (func a b)
        _ -> runtimeError $ "Operands must be numbers."

evalComparison :: (Double -> Double -> Bool) -> Value -> Value -> Interpreter Value
evalComparison comparison lhs rhs =
    case (lhs, rhs) of
        (NumVal a, NumVal b) -> pure $ BoolVal (comparison a b)
        _ -> runtimeError $ "Operands must be numbers."

evalEqual :: (Bool -> Bool) -> Value -> Value -> Interpreter Value
evalEqual modifier lhs rhs =
    pure $ BoolVal $ modifier $ case (lhs, rhs) of
        (NilVal, NilVal) -> True
        (NilVal, _) -> False
        (_, NilVal) -> False
        (StrVal a, StrVal b) -> (a == b)
        (NumVal a, NumVal b) -> (a == b)
        (BoolVal a, BoolVal b) -> (a == b)
        _ -> False

evalUnary :: UnaryOp -> Expr -> Interpreter Value
evalUnary op expr = do
    val <- eval expr
    case (op, val) of
        (Negate, NumVal n) -> pure $ NumVal (-n)
        (Not, _) -> pure $ BoolVal (not (isTruthy val))
        (Show, v) -> pure $ StrVal (Value.toString v)
        (Negate, _) -> runtimeError $ "Operand must be a number."

evalLiteral :: LiteralValue -> Interpreter Value
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
