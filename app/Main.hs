module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runPrompt
        [fileName] -> runFile (fileName)
        _ -> putStrLn "Usage: hlox <filename>"

runPrompt :: IO ()
runPrompt = do
    putStrLn "repl not implemented!!!"

runFile :: String -> IO ()
runFile fileName = do
    fileContent <- readFile fileName
    run fileContent

run :: String -> IO ()
run input = do
    putStrLn "TODO implement run"
