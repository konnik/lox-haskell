{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Scanner (scanTokens) where

import Result
import Token

scanTokens :: String -> Result [Token]
scanTokens source =
    scanTokens' $ Scanner source 1

scanTokens' :: Scanner -> Result [Token]
scanTokens' s =
    case nextToken s of
        Right (t, s') ->
            if t.type_ == EOF
                then okResult [t]
                else fmap ((:) t) $ scanTokens' s'
        Left err -> Left err

data Scanner = Scanner {source :: String, line :: Int}

nextToken :: Scanner -> Result (Token, Scanner)
nextToken s =
    case s.source of
        "" -> emit EOF 0
        '(' : _ -> emit LEFT_PAREN 1
        ')' : _ -> emit RIGHT_PAREN 1
        '{' : _ -> emit LEFT_BRACE 1
        '}' : _ -> emit RIGHT_BRACE 1
        ',' : _ -> emit COMMA 1
        '.' : _ -> emit DOT 1
        '-' : _ -> emit MINUS 1
        '+' : _ -> emit PLUS 1
        ';' : _ -> emit SEMICOLON 1
        '/' : _ -> emit SLASH 1
        '*' : _ -> emit STAR 1
        '\r' : '\n' : _ -> nextToken $ skipWindowsNewLine
        '\n' : _ -> nextToken $ skipLinuxNewLine
        ' ' : _ -> nextToken $ skipWs
        '\t' : _ -> nextToken $ skipWs
        x : _ -> errorResult s.line ("Unexpected character: " ++ show x)
  where
    skipWindowsNewLine :: Scanner
    skipWindowsNewLine = s{source = drop 2 s.source, line = s.line + 1}

    skipLinuxNewLine :: Scanner
    skipLinuxNewLine = s{source = drop 1 s.source, line = s.line + 1}

    skipWs :: Scanner
    skipWs = s{source = drop 1 s.source}

    emit :: TokenType -> Int -> Result (Token, Scanner)
    emit toktyp n =
        okResult
            ( Token
                { type_ = toktyp
                , lexeme = take n s.source
                , literal = ()
                , line = s.line
                }
            , s{source = drop n s.source}
            )