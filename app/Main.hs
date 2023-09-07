module Main where

import Ast (Stmt)
import Control.Monad (when)
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
        [fileName] -> runFile fileName
        _ -> putStrLn "Usage: hlox <filename>"

runPrompt :: IO ()
runPrompt = do
    putStr "hlox> " >> hFlush stdout
    input <- getLine
    _ <- run input
    runPrompt

runFile :: String -> IO ()
runFile fileName = do
    fileContent <- readFile fileName
    (hadErrors, hadRuntimeErrors) <- run fileContent
    when hadErrors $ exitWith $ ExitFailure 65
    when hadRuntimeErrors $ exitWith $ ExitFailure 70

run :: String -> IO (Bool, Bool)
run source = do
    -- putStrLn $ "Input: "
    -- putStrLn source
    -- putStrLn ""
    let (scanErrors, tokens) = scanTokens source
    mapM_ reportError scanErrors
    -- putStrLn "Tokens: "
    -- reportTokens tokens
    case Parser.parse tokens of
        Left parseErrors -> do
            mapM_ reportError parseErrors
            pure (True, False)
        Right program -> do
            -- putStrLn "Parsed statements: "
            -- reportStatements program
            -- putStrLn ""
            -- putStrLn "Program output:"
            hadRuntimeErrors <- Interpreter.run False program
            pure (not (null scanErrors), hadRuntimeErrors)

-- putStrLn "Done."

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