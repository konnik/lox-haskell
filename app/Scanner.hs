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
        '"' : _ -> string
        '(' : _ -> emit LEFT_PAREN 1
        ')' : _ -> emit RIGHT_PAREN 1
        '{' : _ -> emit LEFT_BRACE 1
        '}' : _ -> emit RIGHT_BRACE 1
        ',' : _ -> emit COMMA 1
        '.' : _ -> emit DOT 1
        '-' : _ -> emit MINUS 1
        '+' : _ -> emit PLUS 1
        ';' : _ -> emit SEMICOLON 1
        '*' : _ -> emit STAR 1
        '!' : '=' : _ -> emit BANG_EQUAL 2
        '!' : _ -> emit BANG 1
        '=' : '=' : _ -> emit EQUAL_EQUAL 2
        '=' : _ -> emit EQUAL 1
        '<' : '=' : _ -> emit LESS_EQUAL 2
        '<' : _ -> emit LESS 1
        '>' : '=' : _ -> emit GREATER_EQUAL 2
        '>' : _ -> emit GREATER 1
        '/' : '/' : _ -> nextToken $ ignoreRestOfLine
        '/' : _ -> emit SLASH 1
        ' ' : _ -> nextToken $ skipWs
        '\r' : _ -> nextToken $ skipWs
        '\t' : _ -> nextToken $ skipWs
        '\n' : _ -> nextToken $ skipLinuxNewLine
        x : _ -> errorResult s.line ("Unexpected character: " ++ show x)
  where
    string :: Result (Token, Scanner)
    string =
        let
            str = "\"" ++ takeWhile ((/=) '"') (drop 1 s.source) ++ "\""
         in
            if length str > length s.source
                then errorResult s.line "Unterminated string."
                else emitString str

    emitString :: String -> Result (Token, Scanner)
    emitString str =
        let
            newlines = length $ filter ((==) '\n') str
         in
            okResult $
                ( Token
                    { type_ = STRING (trimQuotes str)
                    , lexeme = str
                    , line = s.line
                    }
                , s
                    { source = drop (length str) s.source
                    , line = s.line + newlines
                    }
                )
    skipLinuxNewLine :: Scanner
    skipLinuxNewLine = s{source = drop 1 s.source, line = s.line + 1}

    ignoreRestOfLine :: Scanner
    ignoreRestOfLine = s{source = dropWhile ((/=) '\n') s.source}

    skipWs :: Scanner
    skipWs = s{source = drop 1 s.source}

    emit :: TokenType -> Int -> Result (Token, Scanner)
    emit toktyp n =
        okResult
            ( Token
                { type_ = toktyp
                , lexeme = take n s.source
                , line = s.line
                }
            , s{source = drop n s.source}
            )

trimQuotes :: String -> String
trimQuotes str = reverse $ drop 1 $ reverse $ drop 1 str