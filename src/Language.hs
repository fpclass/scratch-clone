--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

-- | This module contains the types for the abstract syntax tree.
module Language where

--------------------------------------------------------------------------------

-- | A program consists of a sequence of statements.
type Program = [Stmt]

-- | A program is a sequence of statements.
data Stmt
    = AssignStmt {
        assignVar  :: String,
        assignExpr :: Expr
    }
    | IfStmt {
        ifCond   :: Expr,
        ifBody   :: [Stmt],
        ifElseIf :: [(Expr,[Stmt])],
        ifElse   :: [Stmt]
    }
    | RepeatStmt {
        repeatTimesExpr :: Expr,
        repeatBody      :: [Stmt]
    }
    deriving Show

--------------------------------------------------------------------------------

-- | Operators.
data Op
    = Add                               -- ^ The + operator.
    | Sub                               -- ^ The - operator.
    | Mul                               -- ^ The * operator.
    | Div                               -- ^ The / operator.
    | Pow                               -- ^ The power of operator.
    | Equal                             -- ^ The == operator.
    | Neq                               -- ^ The /= operator.
    | LessThan                          -- ^ The < operator.
    | LessOrEqual                       -- ^ The <= operator.
    | GreaterThan                       -- ^ The > operator.
    | GreaterOrEqual                    -- ^ The >= operator.
    deriving (Eq, Enum, Bounded, Show)

-- | Expressions.
data Expr
    = ValE Int
    | VarE String
    | BinOpE Op Expr Expr
    deriving Show

--------------------------------------------------------------------------------
