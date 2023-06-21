module Ast where

data Stmt
    = StmtPrint Expr
    | StmtExpr Expr
    | StmtVarDecl String Expr
    | StmtBlock [Stmt]
    | StmtIf Expr Stmt (Maybe Stmt)
    deriving (Show)

data Expr
    = Binary BinaryOp Expr Expr
    | Grouping Expr
    | Literal LiteralValue
    | Unary UnaryOp Expr
    | Variable String
    | Assignment String Expr
    | Logic LogicOp Expr Expr
    deriving (Show)

data LiteralValue
    = LiteralString String
    | LiteralNumber Float
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
