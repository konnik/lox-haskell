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

import Data.Maybe (fromJust)

data Environment a = Environment
    { variables :: Map String a
    , parent :: Maybe (Environment a)
    }
    deriving (Show)

declareVar :: String -> a -> Environment a -> Environment a
declareVar name val env =
    env
        { variables =
            Data.Map.insert name val env.variables
        }

getVar :: String -> Environment a -> Either String a
getVar name env =
    case Data.Map.lookup name env.variables of
        Just val -> Right val
        Nothing -> case env.parent of
            Just parent ->
                getVar name parent
            Nothing ->
                Left $ "Undefined variable '" ++ name ++ "'."

setVar :: String -> a -> Environment a -> Either String (Environment a)
setVar name val env =
    if Data.Map.member name env.variables
        then pure $ env{variables = Data.Map.insert name val env.variables}
        else case env.parent of
            Just parent -> do
                parent' <- setVar name val parent
                pure $ env{parent = Just parent'}
            Nothing ->
                Left $ "Undefined variable '" ++ name ++ "'."

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

    printVar :: (Show a) => (String, a) -> IO ()
    printVar (name, value) = putStrLn $ mconcat [name, ": ", replicate (10 - length name) ' ', show value]
