module Value (
    Value (..),
    FuncType (..),
    toString,
) where

import Numeric (showFFloat)
import Types (Interpreter)

data Value
    = StrVal String
    | NumVal Double
    | BoolVal Bool
    | NilVal
    | CallableVal Int FuncType String ([Value] -> Interpreter Value Value)

data FuncType
    = Native
    | UserDefined

instance Show Value where
    show (StrVal str) = show str
    show x = toString x

toString :: Value -> String
toString value =
    case value of
        StrVal str -> str
        NumVal n -> normalizeNum (showFFloat Nothing n "")
        BoolVal True -> "true"
        BoolVal False -> "false"
        NilVal -> "nil"
        CallableVal _ Native _ _ -> "<native fn>"
        CallableVal _ UserDefined name _ -> "<fn " ++ name ++ ">"
  where
    normalizeNum str =
        case reverse str of
            '0' : '.' : rest -> reverse rest
            _ -> str
