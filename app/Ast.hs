module Ast where

data Stmt
    = StmtPrint Expr
    | StmtExpr Expr
    | StmtVarDecl String Expr
    | StmtBlock [Stmt]
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtWhile Expr Stmt
    | StmtFunctionDecl String [String] [Stmt]
    | StmtReturn Int Expr
    deriving (Show)

data Expr
    = Binary Int BinaryOp Expr Expr
    | Grouping Expr
    | Literal LiteralValue
    | Unary UnaryOp Expr
    | Variable Int String
    | Assignment Int String Expr
    | Logic LogicOp Expr Expr
    | Call Int Expr [Expr]
    deriving (Show)

data LiteralValue
    = LiteralString String
    | LiteralNumber Double
    | LiteralTrue
    | LiteralFalse
    | LiteralNil
    deriving (Show)

data BinaryOp
    = Equal
    | NotEqual
    | LessThan
    | LessOrEqual
    | GreaterThan
    | GreaterOrEqual
    | Addition
    | Subtraction
    | Multiplication
    | Division
    deriving (Show)

data UnaryOp
    = Negate
    | Not
    | Show
    deriving (Show)

data LogicOp
    = LogicOr
    | LogicAnd
    deriving (Show)
