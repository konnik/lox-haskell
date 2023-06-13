module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdin, stdout)

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
            run input
            runPrompt

runFile :: String -> IO ()
runFile fileName = do
    fileContent <- readFile fileName
    run fileContent

run :: String -> IO ()
run input = do
    putStrLn "TODO: implement run"
    putStrLn "Input: "
    putStrLn input
