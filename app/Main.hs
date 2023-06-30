module Main where

import Ast (Stmt)
import Interpreter qualified
import Parser qualified
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
    -- putStrLn $ "Input: "
    -- putStrLn source
    -- putStrLn ""

    case scanTokens source of
        Right tokens -> do
            -- putStrLn "Tokens: "
            -- reportTokens tokens
            case Parser.parse tokens of
                Left str -> do
                    hPutStrLn stderr str
                    onError
                Right program -> do
                    -- putStrLn "Parsed statements: "
                    -- reportStatements program
                    -- putStrLn ""
                    -- putStrLn "Program output:"
                    Interpreter.run False program
        -- putStrLn "Done."
        Left err -> do
            reportError err
            onError

reportTokens :: [Token] -> IO ()
reportTokens tokens = do
    putStrLn $ unlines $ fmap (\t -> mconcat [show t.line, ": ", show t.type_]) tokens

reportStatements :: [Stmt] -> IO ()
reportStatements stmts =
    mapM_ putStrLn (fmap show stmts)

reportError :: Error -> IO ()
reportError err =
    hPutStrLn stderr errorMessage
  where
    errorMessage = mconcat ["[line ", show err.line, "] Error", err.where_, ": ", err.message]