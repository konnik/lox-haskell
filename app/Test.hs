{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test where

import Parser
import Scanner
import Token

code :: String
code = "!1+2==3"

-- code = "\"string\""

myParser :: Parser ()
myParser = do
    x <- peek
    if x.type_ == BANG
        then do
            _ <- consume
            match (NUMBER 1)
            match PLUS
            match (NUMBER 2)
            match EQUAL_EQUAL
            match (NUMBER 3)
        else do
            match (STRING "string")
    match EOF

test :: IO ()
test =
    case Scanner.scanTokens code of
        Left err -> putStrLn $ "Scan error : " ++ show err
        Right tokens ->
            case Parser.runParser myParser tokens of
                (tokens', Right v) -> do
                    putStrLn $ "Result: " ++ show v
                    putStrLn $ "Unconsumed tokens: "
                    reportTokenTypes tokens'
                (tokens', Left err) -> do
                    putStrLn $ "Parse error: " ++ err
                    putStrLn $ "Unconsumed tokens: "
                    reportTokenTypes tokens'

reportTokenTypes :: [Token] -> IO ()
reportTokenTypes tokens = do
    mapM_ putStrLn $ fmap (show . (\t -> t.type_)) tokens
