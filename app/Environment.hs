module Environment (
    Environment,
    newEnvironment,
    declareVar,
    getVar,
    setVar,
    enterBlock,
    leaveBlock,
    printEnv,
)
where

import Data.Map (Map)
import Data.Map qualified

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)

data Environment a
    = GlobalScope (IORef (Map String a))
    | LocalScope (Map String (IORef a)) (Environment a)

declareVar :: (MonadIO m) => String -> a -> Environment a -> m (Environment a)
declareVar name val env = do
    case env of
        GlobalScope variablesRef -> do
            variables <- liftIO $ readIORef variablesRef
            liftIO $ writeIORef variablesRef $ Data.Map.insert name val variables
            pure env
        LocalScope variables _ -> do
            ioRefValue <- liftIO $ newIORef val
            pure $ LocalScope (Data.Map.insert name ioRefValue variables) env

getVar :: (MonadIO m) => String -> Environment a -> m (Either String a)
getVar name env =
    case env of
        GlobalScope variablesRef -> do
            variables <- liftIO $ readIORef variablesRef
            case Data.Map.lookup name variables of
                Just val -> do
                    pure $ Right val
                Nothing ->
                    pure $ Left $ "Undefined variable '" ++ name ++ "'."
        LocalScope variables parent ->
            case Data.Map.lookup name variables of
                Just val -> do
                    val' <- liftIO $ readIORef val
                    pure $ Right val'
                Nothing ->
                    getVar name parent

setVar :: (MonadIO m) => String -> a -> Environment a -> m (Either String ())
setVar name val env =
    case env of
        GlobalScope variablesRef -> do
            variables <- liftIO $ readIORef variablesRef
            case Data.Map.lookup name variables of
                Just _ -> do
                    liftIO $ writeIORef variablesRef $ Data.Map.insert name val variables
                    pure $ pure ()
                Nothing ->
                    pure $ Left $ "Undefined variable '" ++ name ++ "'."
        LocalScope variables parent ->
            case Data.Map.lookup name variables of
                Just ref -> do
                    liftIO $ writeIORef ref val
                    pure $ pure ()
                Nothing ->
                    setVar name val parent

enterBlock :: Environment a -> Environment a
enterBlock parent = LocalScope Data.Map.empty parent

leaveBlock :: Environment a -> Environment a
leaveBlock env = case env of
    GlobalScope _ -> error "BUG: Cannot leave global scope."
    LocalScope _ parent -> parent

newEnvironment :: (MonadIO m) => m (Environment a)
newEnvironment = do
    vars <- liftIO $ newIORef Data.Map.empty
    pure $ GlobalScope vars

printEnv :: (Show a) => Environment a -> IO ()
printEnv currEnv = do
    go 1 currEnv
    putStrLn "--- End ---"
  where
    go :: (Show a) => Int -> Environment a -> IO ()
    go n env = do
        case env of
            GlobalScope variablesRef -> do
                putStrLn "--- Global Scope ---"
                variables <- readIORef variablesRef
                mapM_ printVarGlobal $ Data.Map.toList variables
            LocalScope variables parent -> do
                putStrLn $ "--- Local Scope " ++ show n ++ " ---"
                mapM_ printVar $ Data.Map.toList variables
                go (n + 1) parent

    printVar :: (Show a) => (String, IORef a) -> IO ()
    printVar (name, ref) = do
        value <- readIORef ref
        putStrLn $ mconcat [name, ": ", replicate (10 - length name) ' ', show value]

    printVarGlobal :: (Show a) => (String, a) -> IO ()
    printVarGlobal (name, value) = do
        putStrLn $ mconcat [name, ": ", replicate (10 - length name) ' ', show value]
