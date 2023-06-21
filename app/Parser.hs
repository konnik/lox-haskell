{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parser (
    parse,
    Parser (..),
) where

import Ast
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

{-
Lox Grammar:

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
-}
parse :: [Token] -> Either String [Stmt]
parse tokens = do
    snd $ runParser program tokens

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
    identifer <- expect IDENTIFIER
    hasInitialiser <- match EQUAL
    if hasInitialiser
        then do
            initExpr <- expression
            expect_ SEMICOLON
            pure $ StmtVarDecl identifer.lexeme initExpr
        else do
            expect_ SEMICOLON
            pure $ StmtVarDecl identifer.lexeme (Literal LiteralNil)

statement :: Parser Stmt
statement = do
    next <- peek
    case next.type_ of
        PRINT -> skip >> printStatement
        LEFT_BRACE -> skip >> blockStatement []
        _ -> expressionStatement

blockStatement :: [Stmt] -> Parser Stmt
blockStatement stmts = do
    endOfBlock <- check RIGHT_BRACE
    if not endOfBlock
        then do
            innerStmt <- declaration
            blockStatement (stmts ++ [innerStmt])
        else do
            skip
            pure $ StmtBlock stmts

printStatement :: Parser Stmt
printStatement = do
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
    targetExpr <- equality

    isAssignment <- match EQUAL
    if isAssignment
        then do
            valueExpr <- equality
            case targetExpr of
                Variable varname -> pure $ Assignment varname valueExpr
                _ -> parseError prevToken "Illegal target for assignment."
        else pure targetExpr

equality :: Parser Expr
equality = do
    expr <- comparison
    equalityLoop expr

equalityLoop :: Expr -> Parser Expr
equalityLoop lhs = do
    next <- peek
    case next.type_ of
        BANG_EQUAL -> do
            skip
            rhs <- comparison
            equalityLoop $ Binary NotEqual lhs rhs
        EQUAL_EQUAL -> do
            skip
            rhs <- comparison
            equalityLoop $ Binary Equal lhs rhs
        _ -> pure lhs

comparison :: Parser Expr
comparison = do
    expr <- term
    comparisonLoop expr

comparisonLoop :: Expr -> Parser Expr
comparisonLoop lhs = do
    next <- peek
    case next.type_ of
        GREATER -> do
            skip
            rhs <- term
            comparisonLoop $ Binary GreaterThan lhs rhs
        GREATER_EQUAL -> do
            skip
            rhs <- term
            comparisonLoop $ Binary GreaterOrEqual lhs rhs
        LESS -> do
            skip
            rhs <- term
            comparisonLoop $ Binary LessThan lhs rhs
        LESS_EQUAL -> do
            skip
            rhs <- term
            comparisonLoop $ Binary LessOrEqual lhs rhs
        _ -> pure lhs

term :: Parser Expr
term = do
    expr <- factor
    termLoop expr

termLoop :: Expr -> Parser Expr
termLoop lhs = do
    next <- peek
    case next.type_ of
        MINUS -> do
            skip
            rhs <- factor
            termLoop $ Binary Subtraction lhs rhs
        PLUS -> do
            skip
            rhs <- factor
            termLoop $ Binary Addition lhs rhs
        _ -> pure lhs

factor :: Parser Expr
factor = do
    expr <- unary
    factorLoop expr

factorLoop :: Expr -> Parser Expr
factorLoop lhs = do
    next <- peek
    case next.type_ of
        SLASH -> do
            skip
            rhs <- unary
            factorLoop $ Binary Division lhs rhs
        STAR -> do
            skip
            rhs <- unary
            factorLoop $ Binary Multiplication lhs rhs
        _ -> pure lhs

unary :: Parser Expr
unary = do
    next <- peek
    case next.type_ of
        BANG -> Unary Not <$> (skip >> unary)
        MINUS -> Unary Negate <$> (skip >> unary)
        SHOW -> Unary Show <$> (skip >> unary)
        _ -> primary

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
