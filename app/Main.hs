{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Result
import Scanner
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Token (Token (..))

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
            reportTokens tokens
        Left err -> do
            reportError err
            onError

reportTokens :: [Token] -> IO ()
reportTokens tokens = do
    putStrLn $ unlines $ fmap (\t -> mconcat [show t.line, ": ", show t.type_]) tokens

reportError :: Error -> IO ()
reportError err =
    hPutStrLn stderr errorMessage
  where
    errorMessage = mconcat ["[line ", show err.line, "] Error", err.where_, ": ", err.message]