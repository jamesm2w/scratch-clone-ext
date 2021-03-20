--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

-- | This module contains the types for the abstract syntax tree.
module Language where

--------------------------------------------------------------------------------

-- | A program consists of a sequence of statements.
type Program = [Stmt]

-- | A memory cell is an integer value or a subroutine program to run
data MemCell = Val Int | SubProgram Stmt
    deriving (Eq, Show)


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
    | DefSubroutine {
        routineName    :: String,
        routineProgram :: Program
    }
    | CallSubroutine {
        subName  :: String,
        subInput :: [(String, Expr)]
    }
    | SubroutineReturn {
        returnValue :: Expr
    }
    | ReturnIf {
        returnPredicate :: Expr
    }
    | ReturnIfValue {
        returnPredicate :: Expr,
        returnValue :: Expr
    }
    | RepeatUntilStmt {
        repeatPredicate :: Expr,
        repeatBody :: [Stmt]
    }
    | RepeatWhileStmt {
        repeatPredicate :: Expr,
        repeatBody :: [Stmt]
    }
    | CountStmt {
        countInitial :: Expr,
        countLimit :: Expr,
        countIncrement :: Expr,
        countBody :: [Stmt]
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Operators.
data Op
    = Add                               -- ^ The + operator.
    | Sub                               -- ^ The - operator.
    | Mul                               -- ^ The * operator.
    | Div                               -- ^ The / operator.
    | Mod                               -- ^ The % (modulo) operator.
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
    | CallFunction String [(String, Expr)]
    deriving (Eq, Show)

--------------------------------------------------------------------------------
