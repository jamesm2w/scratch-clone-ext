--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Control.Monad

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
-- If there are no statements left to evaluate it just returns the memory.
-- Otherwise, it first evaluates the result of the first memory cell, then 
-- sequentially binds that to a recursive call on the tail of the list of statements.
-- Runs in O(n) time since it linearly recurses through the list of statements
interpret :: Program -> Memory -> Either Err Memory
interpret []     m = Right m
interpret (p:ps) m = exec p m >>= interpret ps

-- | Evaluates an expression with the current memory. Returns either 
-- an Int value when successful or an Error if there was an error in
-- computation.
eval :: Expr -> Memory -> Either Err Int

-- ValE is just an explicit value. Runs in O(1) constant time.
eval (ValE i) _ = Right i

-- VarE is a reference to a variable. Runs in O(n) time as it uses 
-- a linear search to find the value in memory
eval (VarE x) m = memGet x m

-- BinOpE represents the application of an operator with two arguments.
-- It first evaluates the left and right arguments and then uses the apply
-- helper function to calculate the result.
-- Runs in worst case O(l + r) time as both left and right need to be evaluated. 
eval (BinOpE o l r) m = do vl <- eval l m
                           vr <- eval r m
                           apply o vl vr

-- | Execute a statement with memory context. Returns either the new state of memory
-- or an error from evaluatation.
exec :: Stmt -> Memory -> Either Err Memory

-- Assign statement: Assigns a value in memory. This evaluates an expression then sets memory.
-- Runs in O(n) time as it uses a linear search to set memory contents.
exec AssignStmt{..} m = do x <- eval assignExpr m
                           return $ memSet assignVar x m

-- If Statement: Evaluates an if condition. If == 0 then interprets the associated body statements.
-- however, if it's zero it procedes to check the else if conditions, until there are none left
-- at which point it just interprets the final else list of statements. 
-- Runs in O(n) time as it uses a linear search through case statements.
exec IfStmt{..} m = do 
                        x <- eval ifCond m
                        if x == 0
                            then case ifElseIf of -- If we have a non-empty list of cast statements pattern match
                                (c, b) : cs -> exec (IfStmt c b cs ifElse) m -- on the first for a recursive call.
                                []          -> interpret ifElse m -- otherwise, we can just move onto the else block.
                            else interpret ifBody m -- If it's true then just "run" the body.

-- Repeat Statement: Repeats a list of statement a given (fixed) amount of times.
-- Unlike a while loop the upper bound on repetitions doesn't change.
-- However, memory state is passed between iterations. 
-- Runs in O(n) time as it uses a linear fold to repeatedly interpret statement list
exec RepeatStmt{..} m = do i <- eval repeatTimesExpr m 
                           foldM (flip interpret) m (replicate i repeatBody)

---------------------------------
-- Auxilliary Helper functions --
---------------------------------
-- For the memory setter & getter functions
-- I was torn between using the Prelude functions
-- or implementing something myself, but running benchmarks
-- I found no significant time saving on a custom recursive
-- function against using `lookup` and `filter`

-- | A helper function to set a value in memory
-- Returning the new memory list including (n, v), with the rest
-- of the list intact.
-- Uses filter to remove all elements which have the same first element.
-- meaning only one instance of the variable key will be in the memory list
-- at one time.
-- Runs in O(n) since it uses filter, which is a linear search through
-- the list.
memSet :: String -> Int -> Memory -> Memory
memSet n v m = (n, v) : filter ((n /=) . fst) m

-- | A helper function to lookup a value from memory, wrapped like this
-- to make sure if it's not in memory an UninitialisedMemory error is returned.
-- Works in O(n) time by a linear search through the memory list
memGet :: String -> Memory -> Either Err Int
memGet n m = case lookup n m of
                Nothing -> Left $ UninitialisedMemory n
                Just v  -> Right v


-- | `pureEnum` just defines the compostion of pure
-- and fromEnum to give a shorthand for use in the 
--`apply` function.
pureEnum :: Bool -> Either Err Int
pureEnum = pure . fromEnum

-- | The apply function takes the operator and two integer parameters, and returns either
-- the result of the operator applied to the parameters or an error value because of an incorrect value.
--
-- For the boolean operators (==, /=, <, >, <=, >=) I used `fromEnum` which returns 0 when false 
-- and 1 for true.
apply :: Op -> Int -> Int -> Either Err Int
apply Add x y = pure $ x + y
apply Sub x y = pure $ x - y
apply Mul x y = pure $ x * y
apply Div _ 0 = Left DivByZeroError
apply Div x y = Right $ x `div` y
apply Pow x y | y < 0     = Left NegativeExponentError 
              | otherwise = Right $ x ^ y
apply Equal           x y = pureEnum $ x == y
apply Neq             x y = pureEnum $ x /= y
apply LessThan        x y = pureEnum $ x  < y
apply LessOrEqual     x y = pureEnum $ x <= y
apply GreaterThan     x y = pureEnum $ x  > y
apply GreaterOrEqual  x y = pureEnum $ x >= y

--------------------------------------------------------------------------------