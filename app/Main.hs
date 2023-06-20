{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import qualified Interpreter
import qualified Parser
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
    putStrLn $ "Input: "
    putStrLn source
    putStrLn ""

    case scanTokens source of
        Right tokens -> do
            putStrLn "Tokens: "
            reportTokens tokens
            case Parser.parse tokens of
                Left str -> putStrLn ("Parse error: " ++ str)
                Right expr -> do
                    putStrLn "Parsed expresson tree: "
                    putStrLn $ show expr
                    putStrLn ""
                    putStrLn "Evaluated expression:"
                    case Interpreter.eval expr of
                        Left err -> putStrLn $ "Eval error: " ++ err
                        Right value -> putStrLn $ show value
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