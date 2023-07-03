module Result where

data Error = Error
    { line :: Int
    , where_ :: String
    , message :: String
    }
    deriving (Show)
