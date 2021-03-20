--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Control.Monad

import Debug.Trace

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, MemCell)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError String                    -- ^ Division by zero was attempted.
    | NegativeExponentError String             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    | WrongMemoryType String            -- Wrong Memory type: variable : expected : got
    | ExecutionTerminated Memory Bool   -- Execution of the program has terminated prematurely with this memory.
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
eval (VarE x) m = memGetVal x m

-- BinOpE represents the application of an operator with two arguments.
-- It first evaluates the left and right arguments and then uses the apply
-- helper function to calculate the result.
-- Runs in worst case O(l + r) time as both left and right need to be evaluated. 
eval (BinOpE o l r) m = do vl <- eval l m
                           vr <- eval r m
                           apply o vl vr

-- Evaluate a call to a subroutine
eval (CallFunction n args) m = do 
    subProg <- memGetProg n m
    --let argStmts = [AssignStmt an ae | (an, ae) <- args]
    --memory <- interpret argStmts m
    argExprs <- mapM (\x -> eval (snd x) m) args
    let argsNorm = zipWith (\a b -> (a, Val b)) (map fst args) argExprs
    let mem = setAll argsNorm m 
    
    traceM $ show args ++ " in " ++ show mem
    --traceM $ "results in: " ++ show mem
    
    memory' <- exec subProg mem
    eval (VarE "subroutine_return") memory'

setAll :: [(String, MemCell)] -> [(String, MemCell)] -> [(String, MemCell)]
setAll cs fs = foldl (flip $ uncurry memSet) fs cs


-- | Execute a statement with memory context. Returns either the new state of memory
-- or an error from evaluatation.
exec :: Stmt -> Memory -> Either Err Memory

-- Assign statement: Assigns a value in memory. This evaluates an expression then sets memory.
-- Runs in O(n) time as it uses a linear search to set memory contents.
exec AssignStmt{..} m = do x <- eval assignExpr m
                           return $ memSet assignVar (Val x) m

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

-- | Runs a subroutine
exec DefSubroutine{..} m = case interpret routineProgram m of
                                Left (ExecutionTerminated mem _) -> Right mem
                                Left err -> Left $ appendError err $ " in subroutine " ++ routineName
                                Right mem -> Right mem 

-- | Calls a subroutine by name with given input
exec CallSubroutine{..} m = 
    do  
        subProg <- memGetProg subName m
        let argStmts = [AssignStmt an ae | (an, ae) <- subInput]
        memory <- interpret argStmts m
        exec subProg memory


-- | Sets an expression to be the return value for the subroutine.
exec SubroutineReturn{..} m = exec (AssignStmt "subroutine_return" returnValue) m

-- | Returns a value from a subroutine if a given predicate is truthy (continues otherwise)
-- Uses the ExceptionTerminated error value, which is not strictly an error, but forces the
-- interpreter to stop and break out to the next level up.
exec ReturnIfValue{..} m  = do  cond <- eval returnPredicate m
                                value <- eval returnValue m
                                if cond == 0 then
                                    pure m
                                else 
                                    Left $ ExecutionTerminated (("subroutine_return", Val value):m) False

-- | Returns from a subprocedure if a predicate is truthy (continues execution otherwise)
-- Similar to return with a value, this uses ExecutionTerminated to break the interpreter out to the
-- next level up.
exec ReturnIf{..} m = do cond <- eval returnPredicate m
                         if cond == 0 then
                             pure m
                         else Left $ ExecutionTerminated m False

-- | Repeats a given code block until a given predicate is truthy (if false then it interprets the statements again)
exec RepeatUntilStmt{..} m = do memory' <- interpret repeatBody m
                                loop <- eval repeatPredicate memory'
                                if loop == 0 then
                                    case exec (RepeatUntilStmt repeatPredicate repeatBody) memory' of
                                            Left (ExecutionTerminated mem True) -> return mem
                                            Left (ExecutionTerminated mem False) -> exec (RepeatUntilStmt repeatPredicate repeatBody) mem
                                            Left err -> Left err
                                            Right mem -> return mem
                                else
                                    return memory'
-- | Repeats a given code block while a given predicate is truthy
-- If false, it stops looping.
exec RepeatWhileStmt{..} m = do loop <- eval repeatPredicate m
                                if loop == 0 
                                then return m
                                else do
                                        memory' <- interpret repeatBody m
                                        case exec (RepeatWhileStmt repeatPredicate repeatBody) memory' of
                                            Left (ExecutionTerminated mem True) -> return mem
                                            Left (ExecutionTerminated mem False) -> exec (RepeatWhileStmt repeatPredicate repeatBody) mem
                                            Left err -> Left err
                                            Right mem -> return mem

-- | Counts a variable from an initial value to a limit, incrementing by `increment` each time
-- N.B. count variable can be directly manipulated in memory unlike normal repeat.
-- Similar to a for loop, but enforces the "<" stopping condition.
-- Works by recursively calling itself, moving the lower limit up by the increment each time
-- until the lower limit is not < upper limit, when it exits with the new state of memory.
exec CountStmt{..} m = do from <- eval countInitial m
                          let memory' = memSet countVar (Val from) m
                          limit <- eval countLimit memory'
                          by <- eval countIncrement memory'
                          if from < limit then
                              do 
                                  memory'' <- interpret countBody memory'
                                  count <- memGetVal countVar memory''
                                  case exec (CountStmt countVar (ValE $ count + by) countLimit countIncrement countBody) 
                                    memory'' of
                                            Left (ExecutionTerminated mem True) -> return mem
                                            Left (ExecutionTerminated mem False) -> exec (CountStmt countVar (ValE $ count + by) countLimit countIncrement countBody) 
                                                 mem
                                            Left err -> Left err
                                            Right mem -> return mem
                          else
                              return memory'

exec ControlStmt{..} m = Left $ ExecutionTerminated m controlBreak


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
memSet :: String -> MemCell -> Memory -> Memory
memSet n v m = (n, v) : filter ((n /=) . fst) m

-- | A helper function to lookup a value from memory, wrapped like this
-- to make sure if it's not in memory an UninitialisedMemory error is returned.
-- Works in O(n) time by a linear search through the memory list
memGet :: String -> Memory -> Either Err MemCell
memGet n m = case lookup n m of
                Nothing -> Left $ UninitialisedMemory n
                Just v  -> Right v

-- | Gets an integer value from memory, or raises an appropriate type error.
memGetVal :: String -> Memory -> Either Err Int
memGetVal n m = memGet n m >>= value
    where value (Val i) = return i
          value (SubProgram _) = Left $ WrongMemoryType $ show n ++ " expected Value got SubProgram"

-- | Gets a program type value from memory, or raises an appropriate type error.
memGetProg :: String -> Memory -> Either Err Stmt
memGetProg n m = memGet n m >>= program
    where program (Val _) = Left $ WrongMemoryType $ show n ++ " expected SubProgram got Value"
          program (SubProgram e) = return e

-- | Helper function for appending an error message to an error (e.g. if the error occured in a fucntion
-- alerting the user in which function the error occured in!)
appendError :: Err -> String -> Err
appendError (UninitialisedMemory x)   xs = UninitialisedMemory $ x ++ xs
appendError (WrongMemoryType x)       xs = WrongMemoryType $ x ++ xs
appendError (DivByZeroError x)        xs = DivByZeroError $ x ++ xs
appendError (NegativeExponentError x) xs = NegativeExponentError $ x ++ xs
appendError err _ = err

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
apply Div _ 0 = Left $ DivByZeroError ""
apply Div x y = Right $ x `div` y
apply Mod _ 0 = Left $ DivByZeroError ""
apply Mod x y = pure $ x `mod` y
apply Pow x y | y < 0     = Left $ NegativeExponentError "" 
              | otherwise = Right $ x ^ y
apply Equal           x y = pureEnum $ x == y
apply Neq             x y = pureEnum $ x /= y
apply LessThan        x y = pureEnum $ x  < y
apply LessOrEqual     x y = pureEnum $ x <= y
apply GreaterThan     x y = pureEnum $ x  > y
apply GreaterOrEqual  x y = pureEnum $ x >= y

--------------------------------------------------------------------------------