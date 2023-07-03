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

data Environment a = Environment
    { variables :: Map String (IORef a)
    , parent :: Maybe (Environment a)
    }

declareVar :: (MonadIO m) => String -> a -> Environment a -> m (Environment a)
declareVar name val env = do
    ioRefValue <- liftIO $ newIORef val
    pure
        env
            { variables =
                Data.Map.insert name ioRefValue env.variables
            }

getVar :: (MonadIO m) => String -> Environment a -> m (Either String a)
getVar name env =
    case Data.Map.lookup name env.variables of
        Just val -> do
            val' <- liftIO $ readIORef val
            pure $ Right val'
        Nothing -> case env.parent of
            Just parent ->
                getVar name parent
            Nothing ->
                pure $ Left $ "Undefined variable '" ++ name ++ "'."

setVar :: (MonadIO m) => String -> a -> Environment a -> m (Either String ())
setVar name val env =
    case Data.Map.lookup name env.variables of
        Just ref -> do
            liftIO $ writeIORef ref val
            pure $ pure ()
        Nothing -> case env.parent of
            Just parent -> do
                setVar name val parent
            Nothing ->
                pure $ Left $ "Undefined variable '" ++ name ++ "'."

enterBlock :: Environment a -> Environment a
enterBlock parent = newEnvironment{parent = Just parent}

leaveBlock :: Environment a -> Environment a
leaveBlock env = fromJust env.parent

newEnvironment :: Environment a
newEnvironment =
    Environment
        { variables = Data.Map.empty
        , parent = Nothing
        }

printEnv :: (Show a) => Environment a -> IO ()
printEnv currEnv = do
    go 1 currEnv
    putStrLn "--- End ---"
  where
    go :: (Show a) => Int -> Environment a -> IO ()
    go n env = do
        putStrLn $ "--- Frame " ++ show n ++ " ---"
        mapM_ printVar $ Data.Map.toList env.variables
        case env.parent of
            Just parent -> go (n + 1) parent
            Nothing -> pure ()

    printVar :: (Show a) => (String, IORef a) -> IO ()
    printVar (name, ref) = do
        value <- readIORef ref
        putStrLn $ mconcat [name, ": ", replicate (10 - length name) ' ', show value]
