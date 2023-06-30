module Parser (
    parse,
    Parser,
) where

import Ast
import Data.Maybe (catMaybes, fromMaybe)
import Result qualified
import Token (Token (..), TokenType (..))

newtype Parser a = Parser {runParser :: [Token] -> ([Token], [Result.Error], Maybe a)}

instance Functor Parser where
    fmap f p = Parser $ \tokens ->
        case runParser p tokens of
            (tokens', errors, Just v) -> (tokens', errors, Just (f v))
            (tokens', errors, Nothing) -> (tokens', errors, Nothing)

instance Applicative Parser where
    pure value = Parser $ \tokens -> (tokens, [], Just value)
    (<*>) fab fa = Parser $ \tokens ->
        case runParser fab tokens of
            (tokens', errors', Just ab) -> case runParser (fmap ab fa) tokens' of
                (tokens'', errors'', Just b) -> (tokens'', errors' ++ errors'', Just b)
                (tokens'', errors'', Nothing) -> (tokens'', errors' ++ errors'', Nothing)
            (tokens', errors', Nothing) -> (tokens', errors', Nothing)

instance Monad Parser where
    return = pure
    (>>=) ma f = Parser $ \tokens ->
        case runParser ma tokens of
            (tokens', errors', Just a) -> case runParser (f a) tokens' of
                (tokens'', errors'', Just b) -> (tokens'', errors' ++ errors'', Just b)
                (tokens'', errors'', Nothing) -> (tokens'', errors' ++ errors'', Nothing)
            (tokens', errors', Nothing) -> (tokens', errors', Nothing)

unexpectedEndOfFile :: ([Token], [Result.Error], Maybe a)
unexpectedEndOfFile =
    ( []
    ,
        [ Result.Error
            { line = 99
            , where_ = ""
            , message = "Unexpected end of file."
            }
        ]
    , Nothing
    )

-- parsing primitives

-- match(type) - checks to see if the current token has any of the given types. If so, it consumes the token and returns true. Otherwise, it returns false and leaves the current token alone.
-- check(type) - returns true if the current token is of the given type. Unlike match(), it never consumes the token, it only looks at it.
-- advance() - consumes the current token and returns it,

-- isAtEnd() - checks if current token is EOF
-- peek() - returns current token without consuming it
-- previous() - returns previous token

{- |
    Checks if the current token matches the given type. If so, consumes the token and returns True.
    Otherwise returns False without consuming the token.
-}
match :: TokenType -> Parser Bool
match tokenType = do
    tokenMatches <- check tokenType
    if tokenMatches
        then skip >> pure True
        else pure False

{- |
    Returns true if the current token is of the given type.
    Unlike match, it never consumes the token, it only looks at it.
-}
check :: TokenType -> Parser Bool
check tokenType = do
    next <- peek
    if next.type_ == tokenType
        then pure True
        else pure False

{- |
Matches and consumes a specific token without returning it.
Results in an error of topken does not match.
-}
expect :: TokenType -> String -> Parser Token
expect tokenType description = do
    next <- peek
    if next.type_ == tokenType
        then skip >> pure next
        else parseError next $ description

expect_ :: TokenType -> String -> Parser ()
expect_ tokenType description = const () <$> expect tokenType description

{- |
Returns next token without consuming it.
-}
peek :: Parser Token
peek = Parser $ \tokens ->
    case tokens of
        [] -> unexpectedEndOfFile
        t : _ -> (tokens, [], Just t)

{- |
Skips current token without returning it.
-}
skip :: Parser ()
skip = const () <$> advance

{- |
Consume current token and return it.
-}
advance :: Parser Token
advance = Parser $ \tokens ->
    case tokens of
        [] -> unexpectedEndOfFile
        t : tokens' -> (tokens', [], Just t)

parse :: [Token] -> Either [Result.Error] [Stmt]
parse tokens = do
    case runParser program tokens of
        (_, [], Just stmts) -> Right stmts
        (_, errors, _) -> Left errors

{- |
    Selects one of many possible parsers based on the next token (matched token is not consumed).
    If no choice matches then the fallback parser is used.
-}
choice :: [(TokenType, Parser a)] -> Parser a -> Parser a
choice choices fallback = peek >>= loop choices
  where
    loop [] _ = fallback
    loop ((tokenType, currChoice) : rest) nextToken =
        if tokenType == nextToken.type_
            then currChoice
            else loop rest nextToken

{- |
    Selects one of many possible parsers based on the next token (matched token IS consumed).
    If no choice matches then the fallback parser is used.
-}
choiceMatch :: [(TokenType, Parser a)] -> Parser a -> Parser a
choiceMatch choices fallback = peek >>= loop choices
  where
    loop [] _ = fallback
    loop ((tokenType, currChoice) : rest) nextToken =
        if tokenType == nextToken.type_
            then skip >> currChoice
            else loop rest nextToken

{- |
    Selects one of many possible parsers based on the next token (matched token IS consumed).
    If no choice matches then the fallback parser is used.

    Provides the line number of the token to the matched choice.
-}
choiceMatchLine :: [(TokenType, (Int -> Parser a))] -> Parser a -> Parser a
choiceMatchLine choices fallback = peek >>= loop choices
  where
    loop [] _ = fallback
    loop ((tokenType, currChoice) : rest) nextToken =
        if tokenType == nextToken.type_
            then skip >> (currChoice nextToken.line)
            else loop rest nextToken

{- |
Fails the parsing with an error message.
-}
parseError :: Token -> String -> Parser a
parseError token msg = Parser $ \tokens ->
    ( tokens
    , [err]
    , Nothing
    )
  where
    err =
        Result.Error
            { line = token.line
            , where_ = " at '" ++ token.lexeme ++ "'"
            , message = msg
            }

_parseErrorAndContinue :: Token -> String -> Parser ()
_parseErrorAndContinue token msg = Parser $ \tokens ->
    ( tokens
    , [err]
    , Just ()
    )
  where
    err =
        Result.Error
            { line = token.line
            , where_ = " at '" ++ token.lexeme ++ "'"
            , message = msg
            }

program :: Parser [Stmt]
program = do
    isEof <- check EOF
    if isEof
        then pure []
        else do
            stmt <- declaration
            fmap ((:) stmt) program

declaration :: Parser Stmt
declaration = do
    isEof <- check EOF
    if isEof
        then exitParser
        else do
            isVar <- check VAR
            recoverParseError synchronize $ do
                if isVar
                    then varDeclaration
                    else statement

exitParser :: Parser a
exitParser = Parser $ \tokens ->
    (tokens, [], Nothing)

synchronize :: Parser Stmt
synchronize = do
    next <- peek
    if next.type_ == SEMICOLON
        then do
            skip
            declaration
        else case next.type_ of
            EOF -> exitParser
            CLASS -> declaration
            FUN -> declaration
            VAR -> declaration
            FOR -> declaration
            IF -> declaration
            WHILE -> declaration
            PRINT -> declaration
            RETURN -> declaration
            _ -> skip >> synchronize

recoverParseError :: Parser a -> Parser a -> Parser a
recoverParseError fallback parser = Parser $ \tokens ->
    case runParser parser tokens of
        (tokens', errors', Just a) -> (tokens', errors', Just a)
        ([], errors', Nothing) -> ([], errors', Nothing)
        (t : tokens', errors', Nothing) ->
            if t.type_ == EOF
                then ([t], errors', Nothing)
                else case runParser fallback (t : tokens') of
                    (tokens'', errors'', Just a'') -> (tokens'', errors' ++ errors'', Just a'')
                    (tokens'', errors'', Nothing) -> (tokens'', errors' ++ errors'', Nothing)

varDeclaration :: Parser Stmt
varDeclaration = do
    expect_ VAR "Expect 'var'"
    identifier <- expect IDENTIFIER "Expect variable name."
    hasInitialiser <- match EQUAL
    if hasInitialiser
        then do
            initExpr <- expression
            expect_ SEMICOLON "Expect semicolon."
            pure $ StmtVarDecl identifier.lexeme initExpr
        else do
            expect_ SEMICOLON "Expect semicolon."
            pure $ StmtVarDecl identifier.lexeme (Literal LiteralNil)

statement :: Parser Stmt
statement = do
    choice
        [ (IF, ifStatement)
        , (WHILE, whileStatement)
        , (FOR, forStatement)
        , (PRINT, printStatement)
        , (LEFT_BRACE, blockStatement)
        ]
        expressionStatement

forStatement :: Parser Stmt
forStatement = do
    expect_ FOR "Expect for."
    expect_ LEFT_PAREN "Expect ("

    maybeInit <- forInitalizer
    condition <- forCondition
    expect_ SEMICOLON "Expect semi"
    maybeIncr <- forIncrement
    expect_ RIGHT_PAREN "Expect right paren"
    forBody <- statement

    let desugaredWhileBody =
            StmtBlock $
                catMaybes
                    [ Just forBody
                    , StmtExpr <$> maybeIncr
                    ]
    let desugaredWhile =
            StmtBlock $
                catMaybes
                    [ maybeInit
                    , Just $ StmtWhile condition desugaredWhileBody
                    ]
    pure desugaredWhile
  where
    forInitalizer :: Parser (Maybe Stmt)
    forInitalizer =
        choice
            [ (SEMICOLON, skip >> pure Nothing)
            , (VAR, Just <$> varDeclaration)
            ]
            $ Just <$> expressionStatement

    forCondition :: Parser Expr
    forCondition = fromMaybe (Literal LiteralTrue) <$> maybeCond
      where
        maybeCond =
            choice
                [ (SEMICOLON, pure Nothing)
                ]
                $ Just <$> expression

    forIncrement :: Parser (Maybe Expr)
    forIncrement =
        choice
            [ (RIGHT_PAREN, pure Nothing)
            ]
            $ Just <$> expression

whileStatement :: Parser Stmt
whileStatement =
    -- Experimenting with applicatiove syntax, don't know if I like it... :-)
    pure StmtWhile
        <* expect WHILE "Expect while"
        <* expect LEFT_PAREN "Expect left pren"
        <*> expression
        <* expect RIGHT_PAREN "Expect right paren"
        <*> statement

ifStatement :: Parser Stmt
ifStatement = do
    expect_ IF "Expect if keyword"
    expect_ LEFT_PAREN "Expect left pren"
    condExpr <- expression
    expect_ RIGHT_PAREN "Expect roght pren"
    thenStmt <- statement
    choiceMatch
        [ (ELSE, statement >>= pure . StmtIf condExpr thenStmt . Just)
        ]
        $ pure (StmtIf condExpr thenStmt Nothing)

blockStatement :: Parser Stmt
blockStatement = expect_ LEFT_BRACE "expect begin block" >> zeroOrMoreStmts []
  where
    zeroOrMoreStmts :: [Stmt] -> Parser Stmt
    zeroOrMoreStmts stmts = do
        endOfBlock <- check RIGHT_BRACE
        if not endOfBlock
            then do
                innerStmt <- declaration
                zeroOrMoreStmts (stmts ++ [innerStmt])
            else do
                skip
                pure $ StmtBlock stmts

printStatement :: Parser Stmt
printStatement = do
    expect_ PRINT "expect print"
    expr <- expression
    expect_ SEMICOLON "expect semi"
    pure $ StmtPrint expr

expressionStatement :: Parser Stmt
expressionStatement = do
    expr <- expression
    expect_ SEMICOLON "Expect ';' after expression."
    pure $ StmtExpr expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = do
    targetExpr <- logicOr

    prevToken <- peek
    isAssignment <- match EQUAL
    if isAssignment
        then do
            valueExpr <- logicOr
            case targetExpr of
                Variable line varname -> pure $ Assignment line varname valueExpr
                _ -> parseError prevToken "Invalid assignment target."
        else pure targetExpr

logicOr :: Parser Expr
logicOr = logicAnd >>= logicOrLoop
  where
    logicOrLoop :: Expr -> Parser Expr
    logicOrLoop expr =
        choiceMatch
            [ (OR, logicAnd >>= logicOrLoop . (Logic LogicOr expr))
            ]
            $ pure expr

logicAnd :: Parser Expr
logicAnd = equality >>= logicAndLoop
  where
    logicAndLoop :: Expr -> Parser Expr
    logicAndLoop expr = do
        choiceMatch
            [ (AND, equality >>= logicAndLoop . Logic LogicAnd expr)
            ]
            $ pure expr

equality :: Parser Expr
equality = comparison >>= equalityLoop
  where
    equalityLoop :: Expr -> Parser Expr
    equalityLoop lhs =
        choiceMatchLine
            [ (BANG_EQUAL, \line -> comparison >>= equalityLoop . Binary line NotEqual lhs)
            , (EQUAL_EQUAL, \line -> comparison >>= equalityLoop . Binary line Equal lhs)
            ]
            $ pure lhs

comparison :: Parser Expr
comparison = term >>= comparisonLoop

comparisonLoop :: Expr -> Parser Expr
comparisonLoop lhs =
    choiceMatchLine
        [ (GREATER, \line -> term >>= comparisonLoop . Binary line GreaterThan lhs)
        , (GREATER_EQUAL, \line -> term >>= comparisonLoop . Binary line GreaterOrEqual lhs)
        , (LESS, \line -> term >>= comparisonLoop . Binary line LessThan lhs)
        , (LESS_EQUAL, \line -> term >>= comparisonLoop . Binary line LessOrEqual lhs)
        ]
        $ pure lhs

term :: Parser Expr
term = factor >>= termLoop
  where
    termLoop :: Expr -> Parser Expr
    termLoop lhs =
        choiceMatchLine
            [ (MINUS, \line -> factor >>= termLoop . Binary line Subtraction lhs)
            , (PLUS, \line -> factor >>= termLoop . Binary line Addition lhs)
            ]
            $ pure lhs

factor :: Parser Expr
factor = unary >>= factorLoop
  where
    factorLoop :: Expr -> Parser Expr
    factorLoop lhs =
        choiceMatchLine
            [ (SLASH, \line -> unary >>= factorLoop . Binary line Division lhs)
            , (STAR, \line -> unary >>= factorLoop . Binary line Multiplication lhs)
            ]
            $ pure lhs

unary :: Parser Expr
unary = do
    choiceMatch
        [ (BANG, Unary Not <$> unary)
        , (MINUS, Unary Negate <$> unary)
        , (SHOW, Unary Show <$> unary)
        ]
        primary

primary :: Parser Expr
primary = do
    next <- advance
    case next.type_ of
        NUMBER value -> pure $ Literal (LiteralNumber value)
        STRING value -> pure $ Literal (LiteralString value)
        TRUE -> pure $ Literal LiteralTrue
        FALSE -> pure $ Literal LiteralFalse
        NIL -> pure $ Literal LiteralNil
        LEFT_PAREN -> do
            expr <- expression
            expect_ RIGHT_PAREN "expect roight paren"
            pure $ Grouping expr
        IDENTIFIER -> pure $ Variable next.line next.lexeme
        _ -> parseError next $ "Expect expression."
