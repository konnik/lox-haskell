{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List (intercalate, intersperse)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdin, stdout)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runPrompt
        [fileName] -> runFile (fileName)
        _ -> putStrLn "Usage: hlox <filename>"

runPrompt :: IO ()
runPrompt = do
    putStr "hlox> " >> hFlush stdout
    input <- getLine
    case input of
        "" -> runPrompt
        _ -> do
            run (pure ()) input
            runPrompt

runFile :: String -> IO ()
runFile fileName = do
    fileContent <- readFile fileName
    run (exitWith $ ExitFailure 65) fileContent

run :: IO () -> String -> IO ()
run onError source = do
    case scanTokens source of
        Right tokens -> do
            putStrLn $ "Tokens: " <> mconcat (intersperse ", " (fmap show tokens))
        Left err -> do
            reportError err
            onError

data Token
    = TokenA
    | TokenB
    deriving (Show)

scanTokens :: String -> Result [Token]
scanTokens source =
    okResult [TokenA, TokenB]

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

reportError :: Error -> IO ()
reportError err =
    hPutStrLn stderr errorMessage
  where
    errorMessage = mconcat ["[line ", show err.line, "] Error", err.where_, ": ", err.message]