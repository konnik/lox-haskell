{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parser (
    parse,
    Parser,
) where

import Ast
import Data.Maybe (catMaybes, fromMaybe)
import Token (Token (..), TokenType (..))

newtype Parser a = Parser {runParser :: [Token] -> ([Token], Either String a)}

instance Functor Parser where
    fmap f p = Parser $ \tokens ->
        case runParser p tokens of
            (tokens', Right v) -> (tokens', Right (f v))
            (tokens', Left err) -> (tokens', Left err)

instance Applicative Parser where
    pure value = Parser $ \tokens -> (tokens, Right value)
    (<*>) fab fa = Parser $ \tokens ->
        case runParser fab tokens of
            (tokens', Right ab) -> runParser (fmap ab fa) tokens'
            (tokens', Left err) -> (tokens', Left err)

instance Monad Parser where
    return = pure
    (>>=) ma f = Parser $ \tokens ->
        case runParser ma tokens of
            (tokens', Right a) -> runParser (f a) tokens'
            (tokens', Left err) -> (tokens', Left err)

unexpectedEndOfFile :: ([Token], Either String a)
unexpectedEndOfFile = ([], Left "Unexpected end of file.")

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
expect :: TokenType -> Parser Token
expect tokenType = do
    next <- advance
    if next.type_ == tokenType
        then pure next
        else parseError next $ "Expected " ++ show tokenType ++ " but was " ++ show next.type_

expect_ :: TokenType -> Parser ()
expect_ tokenType = const () <$> expect tokenType

{- |
Returns next token without consuming it.
-}
peek :: Parser Token
peek = Parser $ \tokens ->
    case tokens of
        [] -> unexpectedEndOfFile
        t : _ -> (tokens, Right t)

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
        t : tokens' -> (tokens', Right t)

parse :: [Token] -> Either String [Stmt]
parse tokens = do
    snd $ runParser program tokens

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
Fails the parsing with an error message.
-}
parseError :: Token -> String -> Parser a
parseError token msg = Parser $ \tokens ->
    ( tokens
    , Left $ "line " ++ show token.line ++ ": " ++ msg
    )

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
    isVar <- check VAR
    if isVar
        then varDeclaration
        else statement

varDeclaration :: Parser Stmt
varDeclaration = do
    expect_ VAR
    identifier <- expect IDENTIFIER
    hasInitialiser <- match EQUAL
    if hasInitialiser
        then do
            initExpr <- expression
            expect_ SEMICOLON
            pure $ StmtVarDecl identifier.lexeme initExpr
        else do
            expect_ SEMICOLON
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
    expect_ FOR
    expect_ LEFT_PAREN

    maybeInit <- forInitalizer
    condition <- forCondition
    expect_ SEMICOLON
    maybeIncr <- forIncrement
    expect_ RIGHT_PAREN
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
whileStatement = do
    expect_ WHILE
    expect_ LEFT_PAREN
    condExpr <- expression
    expect_ RIGHT_PAREN
    stmt <- statement

    pure $ StmtWhile condExpr stmt

ifStatement :: Parser Stmt
ifStatement = do
    expect_ IF
    expect_ LEFT_PAREN
    condExpr <- expression
    expect_ RIGHT_PAREN
    thenStmt <- statement
    choiceMatch
        [ (ELSE, statement >>= pure . StmtIf condExpr thenStmt . Just)
        ]
        $ pure (StmtIf condExpr thenStmt Nothing)

blockStatement :: Parser Stmt
blockStatement = expect_ LEFT_BRACE >> zeroOrMoreStmts []
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
    expect_ PRINT
    expr <- expression
    expect_ SEMICOLON
    pure $ StmtPrint expr

expressionStatement :: Parser Stmt
expressionStatement = do
    expr <- expression
    expect_ SEMICOLON
    pure $ StmtExpr expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = do
    prevToken <- peek
    targetExpr <- logicOr

    isAssignment <- match EQUAL
    if isAssignment
        then do
            valueExpr <- logicOr
            case targetExpr of
                Variable varname -> pure $ Assignment varname valueExpr
                _ -> parseError prevToken "Illegal target for assignment."
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
        choiceMatch
            [ (BANG_EQUAL, comparison >>= equalityLoop . Binary NotEqual lhs)
            , (EQUAL_EQUAL, comparison >>= equalityLoop . Binary Equal lhs)
            ]
            $ pure lhs

comparison :: Parser Expr
comparison = term >>= comparisonLoop

comparisonLoop :: Expr -> Parser Expr
comparisonLoop lhs =
    choiceMatch
        [ (GREATER, term >>= comparisonLoop . Binary GreaterThan lhs)
        , (GREATER_EQUAL, term >>= comparisonLoop . Binary GreaterOrEqual lhs)
        , (LESS, term >>= comparisonLoop . Binary LessThan lhs)
        , (LESS_EQUAL, term >>= comparisonLoop . Binary LessOrEqual lhs)
        ]
        $ pure lhs

term :: Parser Expr
term = factor >>= termLoop
  where
    termLoop :: Expr -> Parser Expr
    termLoop lhs =
        choiceMatch
            [ (MINUS, factor >>= termLoop . Binary Subtraction lhs)
            , (PLUS, factor >>= termLoop . Binary Addition lhs)
            ]
            $ pure lhs

factor :: Parser Expr
factor = unary >>= factorLoop
  where
    factorLoop :: Expr -> Parser Expr
    factorLoop lhs =
        choiceMatch
            [ (SLASH, unary >>= factorLoop . Binary Division lhs)
            , (STAR, unary >>= factorLoop . Binary Multiplication lhs)
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
            expect_ RIGHT_PAREN
            pure $ Grouping expr
        IDENTIFIER -> pure $ Variable next.lexeme
        _ -> parseError next $ "An expression can not start with " ++ show next.type_ ++ ""
