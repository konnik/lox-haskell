module Environment (
    Environment,
    newEnvironment,
    declareVar,
    getVar,
    setVar,
)
where

import Data.Map (Map)
import Data.Map qualified

import Value (Value)

data Environment = Environment
    { decls :: Map String Value
    }

declareVar :: String -> Value -> Environment -> Environment
declareVar name val env =
    env
        { decls =
            Data.Map.insert name val env.decls
        }

getVar :: String -> Environment -> Either String Value
getVar name env =
    case Data.Map.lookup name env.decls of
        Just val -> Right val
        Nothing -> Left $ "Undefined variable: '" ++ name ++ "'"

setVar :: String -> Value -> Environment -> Either String Environment
setVar name val env =
    if Data.Map.member name env.decls
        then pure $ env{decls = Data.Map.insert name val env.decls}
        else Left $ "Varable not declared: '" ++ name ++ "'"

newEnvironment :: Environment
newEnvironment =
    Environment
        { decls = Data.Map.empty
        }
