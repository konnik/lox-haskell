module Value (
    Value (..),
    toString,
) where

data Value
    = StrVal String
    | NumVal Float
    | BoolVal Bool
    | NilVal
    deriving (Show, Eq)

toString :: Value -> String
toString value =
    case value of
        StrVal str -> str
        NumVal n -> normalizeNum (show n)
        BoolVal True -> "true"
        BoolVal False -> "false"
        NilVal -> "nil"
  where
    normalizeNum str =
        case reverse str of
            '0' : '.' : rest -> reverse rest
            _ -> str
