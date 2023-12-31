module Scanner (scanTokens) where

import Data.Char (isAlpha, isDigit)
import Data.Map qualified as Map

import Result
import Token

type Result = Either (Error, Scanner) (Token, Scanner)

okResult :: (Token, Scanner) -> Result
okResult value = Right value

errorResult :: Int -> String -> Scanner -> Result
errorResult line message s = Left $ (Error line "" message, s)

scanTokens :: String -> ([Error], [Token])
scanTokens source =
    scanTokens' [] [] $ Scanner source 1

scanTokens' :: [Error] -> [Token] -> Scanner -> ([Error], [Token])
scanTokens' errors tokens s =
    case nextToken s of
        Right (t, s') ->
            if t.type_ == EOF
                then (errors, tokens ++ [t])
                else scanTokens' errors (tokens ++ [t]) s'
        Left (err, s') -> scanTokens' (errors ++ [err]) tokens s'

data Scanner = Scanner {source :: String, line :: Int}

nextToken :: Scanner -> Result
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
        '0' : _ -> digit
        '1' : _ -> digit
        '2' : _ -> digit
        '3' : _ -> digit
        '4' : _ -> digit
        '5' : _ -> digit
        '6' : _ -> digit
        '7' : _ -> digit
        '8' : _ -> digit
        '9' : _ -> digit
        '\r' : _ -> nextToken $ skipWs
        '\t' : _ -> nextToken $ skipWs
        '\n' : _ -> nextToken $ skipLinuxNewLine
        x : _ ->
            if isAlphaUnderscore x
                then identifier
                else errorResult s.line ("Unexpected character.") (skip 1)
  where
    identifier :: Result
    identifier =
        let
            word = takeWhile isAlpahNumUnderscore s.source
            keywords =
                Map.fromList
                    [ ("and", AND)
                    , ("class", CLASS)
                    , ("else", ELSE)
                    , ("false", FALSE)
                    , ("for", FOR)
                    , ("fun", FUN)
                    , ("if", IF)
                    , ("nil", NIL)
                    , ("or", OR)
                    , ("print", PRINT)
                    , ("show", SHOW)
                    , ("return", RETURN)
                    , ("super", SUPER)
                    , ("this", THIS)
                    , ("true", TRUE)
                    , ("var", VAR)
                    , ("while", WHILE)
                    ]
         in
            case Map.lookup word keywords of
                Just typ -> emit typ (length word)
                Nothing -> emit IDENTIFIER (length word)

    digit :: Result
    digit =
        let
            digits = takeWhile isDigit s.source
            rest = drop (length digits) s.source
         in
            case rest of
                [] -> emitNumber digits
                '.' : [] -> emitNumber digits
                '.' : x : rest2 ->
                    if not (isDigit x)
                        then emitNumber digits
                        else
                            let digits2 = x : takeWhile isDigit rest2
                             in emitNumber (digits ++ "." ++ digits2)
                _ -> emitNumber digits

    emitNumber :: String -> Result
    emitNumber str =
        okResult $
            ( Token
                { type_ = NUMBER (read str)
                , lexeme = str
                , line = s.line
                }
            , s
                { source = drop (length str) s.source
                }
            )

    string :: Result
    string =
        let
            str = takeWhile ((/=) '"') (drop 1 s.source)
         in
            if length str >= (length s.source - 1)
                then errorResult s.line "Unterminated string." (skip (length s.source))
                else emitString str

    emitString :: String -> Result
    emitString str =
        let
            newlines = length $ filter ((==) '\n') str
         in
            okResult $
                ( Token
                    { type_ = STRING str
                    , lexeme = '"' : str ++ "\""
                    , line = s.line
                    }
                , s
                    { source = drop (length str + 2) s.source
                    , line = s.line + newlines
                    }
                )
    skipLinuxNewLine :: Scanner
    skipLinuxNewLine = s{source = drop 1 s.source, line = s.line + 1}

    ignoreRestOfLine :: Scanner
    ignoreRestOfLine = s{source = dropWhile ((/=) '\n') s.source}

    skipWs :: Scanner
    skipWs = s{source = drop 1 s.source}

    skip :: Int -> Scanner
    skip n = s{source = drop n s.source}

    emit :: TokenType -> Int -> Result
    emit toktyp n =
        okResult
            ( Token
                { type_ = toktyp
                , lexeme = take n s.source
                , line = s.line
                }
            , s{source = drop n s.source}
            )

isAlphaUnderscore :: Char -> Bool
isAlphaUnderscore ch = isAlpha ch || ch == '_'

isAlpahNumUnderscore :: Char -> Bool
isAlpahNumUnderscore ch = isAlphaUnderscore ch || isDigit ch
