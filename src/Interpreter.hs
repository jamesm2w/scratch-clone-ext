--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Data.Either

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] m = Right m
interpret (cell:cells) mem = if isError then result else interpret cells newMem
    where 
        isError = isLeft result
        result = exec cell mem
        newMem = fromRight [] result

        -- Apply binary operator to two values integers 
        apply :: Op -> Int -> Int -> Either Err Int
        apply Add x y = Right $ x + y
        apply Sub x y = Right $ x - y
        apply Mul x y = Right $ x * y
        apply Div _ 0 = Left DivByZeroError
        apply Div x y = Right $ x `div` y
        apply Pow x y = if y < 0 then Left NegativeExponentError else Right $ x ^ y
        apply Equal x y = if x == y then Right 1 else Right 0
        apply Neq x y = if x /= y then Right 1 else Right 0
        apply LessThan x y = if x < y then Right 1 else Right 0
        apply LessOrEqual x y = if x <= y then Right 1 else Right 0
        apply GreaterThan x y = if x > y then Right 1 else Right 0
        apply GreaterOrEqual  x y = if x >= y then Right 1 else Right 0

        -- Evaluate an expression into an integer
        eval :: Expr -> Memory -> Either Err Int
        eval (ValE i) _ = Right i 
        eval (VarE x) [] = Left $ UninitialisedMemory x
        eval expr@(VarE x) ((n, v):ms) = if x == n then Right v else eval expr ms
        eval (BinOpE op x y) ms = if isEvalError then Left evalError else apply op valueX valueY
            where
                isEvalError = isXError || isYError
                evalError = if isXError then fromLeft DivByZeroError evalX else fromLeft DivByZeroError evalY 
                isXError = isLeft evalX
                isYError = isLeft evalY
                valueX = fromRight 0 evalX
                valueY = fromRight 0 evalY
                evalX = eval x ms
                evalY = eval y ms

        -- Executes a statement
        exec :: Stmt -> Memory -> Either Err Memory
        exec (AssignStmt n e) m = if isExprError then Left exprError else Right $ assign' n exprValue m
            where
                isExprError = isLeft evalExpr
                evalExpr = eval e m
                exprError = fromLeft DivByZeroError evalExpr
                exprValue = fromRight 0 evalExpr
                
                assign' :: String -> Int -> Memory -> Memory
                assign' name i [] = [(name, i)]
                assign' name i ((cellName, cellValue):cs) = if name == cellName then (name, i):cs else (cellName, cellValue):assign' name i cs
        -- Executing an IF statement with a predicate, true program, case predicate + program list and false program
        exec (IfStmt predicate t cs f) m = if isPredError then Left predError else control isPredTruthy t cs f m
            where
                isPredError  = isLeft predResult
                predError    = fromLeft DivByZeroError predResult
                predResult   = eval predicate m
                isPredTruthy = 0 /= fromRight 0 predResult

                -- Control takes an expression which reduces to a truthy value, then a program to run if true, a list of programs to run and a program to run if false then memory 
                control :: Bool -> Program -> [(Expr, Program)] -> Program -> Memory -> Either Err Memory
                control True onTrue _ _    progMem = interpret onTrue progMem
                control False _ [] onFalse progMem = interpret onFalse progMem
                control False onTrue ((expr, onCaseTrue):cases) onFalse progMem
                    | isVError   = Left vError
                    | value /= 0 = interpret onCaseTrue progMem 
                    | otherwise  = control False onTrue cases onFalse progMem
                    where
                        isVError = isLeft v
                        vError = fromLeft DivByZeroError v
                        v = eval expr progMem
                        value = fromRight 0 v
        -- Executing a FOR statement. Repeats a program amount times.
        exec (RepeatStmt amountExpr program) m = if isAmountError then Left amountError else loop amount program m
            where 
                isAmountError = isLeft amountResult
                amountResult = eval amountExpr m
                amountError = fromLeft DivByZeroError amountResult
                amount = fromRight 0 amountResult

                loop :: Int -> Program -> Memory -> Either Err Memory
                loop 1 p progMem = interpret p progMem
                loop n p progMem = if n > 0 then loop (n - 1) p nextMem else Right progMem
                    where
                        nextMem = fromRight [] res 
                        res = interpret p progMem

--------------------------------------------------------------------------------
