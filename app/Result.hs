module Result where

data Error = Error
    { line :: Int
    , where_ :: String
    , message :: String
    }
type Result a = Either Error a

okResult :: a -> Result a
okResult value = Right value

errorResult :: Int -> String -> Result a
errorResult line message = Left $ Error line "" message
