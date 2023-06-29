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
import Value (Value, toString)

data Environment = Environment
    { variables :: Map String Value
    , parent :: Maybe Environment
    }
    deriving (Show)

declareVar :: String -> Value -> Environment -> Environment
declareVar name val env =
    env
        { variables =
            Data.Map.insert name val env.variables
        }

getVar :: String -> Environment -> Either String Value
getVar name env =
    case Data.Map.lookup name env.variables of
        Just val -> Right val
        Nothing -> case env.parent of
            Just parent ->
                getVar name parent
            Nothing ->
                Left $ "Undefined variable: '" ++ name ++ "'"

setVar :: String -> Value -> Environment -> Either String Environment
setVar name val env =
    if Data.Map.member name env.variables
        then pure $ env{variables = Data.Map.insert name val env.variables}
        else case env.parent of
            Just parent -> do
                parent' <- setVar name val parent
                pure $ env{parent = Just parent'}
            Nothing ->
                Left $ "Variable not declared: '" ++ name ++ "'"

enterBlock :: Environment -> Environment
enterBlock parent = newEnvironment{parent = Just parent}

leaveBlock :: Environment -> Environment
leaveBlock env = fromJust env.parent

newEnvironment :: Environment
newEnvironment =
    Environment
        { variables = Data.Map.empty
        , parent = Nothing
        }

printEnv :: Environment -> IO ()
printEnv currEnv = do
    go 1 currEnv
    putStrLn "--- End ---"
  where
    go :: Int -> Environment -> IO ()
    go n env = do
        putStrLn $ "--- Frame " ++ show n ++ " ---"
        mapM_ printVar $ Data.Map.toList env.variables
        case env.parent of
            Just parent -> go (n + 1) parent
            Nothing -> pure ()

    printVar :: (String, Value) -> IO ()
    printVar (name, value) = putStrLn $ mconcat [name, ": ", replicate (10 - length name) ' ', Value.toString value]
