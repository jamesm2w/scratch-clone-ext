--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone : Extended Edition                             --
--------------------------------------------------------------------------------
-- Extensions in this file: language extensions defined in src/Language.hs
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
    | DefSubroutine { -- Defines a new subroutine in memory
        routineName    :: String,
        routineProgram :: Program
    }
    | CallSubroutine { -- Calls a subroutine in memory with list of input params
        subName  :: String,
        subInput :: [(String, Expr)]
    }
    | SubroutineReturn { -- Returns a value from a subroutine
        returnValue :: Expr
    }
    | ReturnIf { -- Exits from a subroutine if a predicate is truthy
        returnPredicate :: Expr
    }
    | ReturnIfValue { -- Exits from a subroutine with a value if predicate is truthy
        returnPredicate :: Expr,
        returnValue :: Expr
    }
    | RepeatUntilStmt { -- Repeats a program until predicate is truthy
        repeatPredicate :: Expr,
        repeatBody :: [Stmt]
    }
    | RepeatWhileStmt { -- Repeats a program while a predicate is truthy
        repeatPredicate :: Expr,
        repeatBody :: [Stmt]
    }
    | CountStmt { -- Counts with a variable from an initial limit, to an upper limit with increment
        countVar :: String,
        countInitial :: Expr,
        countLimit :: Expr,
        countIncrement :: Expr,
        countBody :: [Stmt]
    }
    | ControlStmt { -- Stops execution of a loop. If the break value is False then 
                    --  it just continues to the next iteration
        controlBreak :: Bool
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
    | CallFunction String [(String, Expr)] -- Call function with a return value with list of inputs.
    deriving (Eq, Show)

--------------------------------------------------------------------------------
