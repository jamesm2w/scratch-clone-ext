--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone : Extended Edition                             --
--------------------------------------------------------------------------------
-- Language Extensions Available:
--      Functions:
--          Define Functions and Procedures with optional parameters
--          Call functions and procudures by name with parameters
--          Return with a value from function or just return from procedures
--      Loops:
--          For loops   - Loop with a variable from 0 to a max limit
--          While loops - Continue execution of a block while a predicate is true
--          Until loops - Continue execution of a block until the predicate is true
--          Break statement    - Break out of loop execution
--          Continue statement - Stop execution and continue to the next iteration of the loop
--      Misc:
--          Modulo operator - Work out the integer remainder of division
--          Shadow blocks   - Don't want to spend hours dragging math blocks for arithmetic?
--                            don't worry! You can just edit the shadow blocks :)
--
-- Program memory has also been extended. Instead of just supporting an "Int" value
-- Memory now supports a MemCell type, which can be an integer value or a sub program.
--
-- All language extensions are also defined in src/Language.hs
--
--
-- All extensions are fully functional in the scratch clone front-end with custom parsers
--  in exe/Convert.hs
--
-- Subprograms are correctly loaded into memory in exe/Main.hs
--
-- All existing tests have been updated to work with new types of memory & new operators
--  in test/Tests.hs
--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Interpreter where

--------------------------------------------------------------------------------

import Language

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, MemCell)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError String             -- ^ Division by zero was attempted.
    | NegativeExponentError String      -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    | WrongMemoryType String            -- ^ Wrong Memory type for variable
    | ExecutionTerminated Memory        -- ^ Execution of the program has terminated prematurely with this memory.
    | LoopTerminated Bool Memory        -- ^ A loop as terminated (True for break/False for continue)
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
-- If there are no statements left to evaluate it just returns the memory.
-- Otherwise, it first evaluates the result of the first memory cell, then 
-- sequentially binds that to a recursive call on the tail of the list of statements.
-- Performance:
--  Best Case O(1) - An empty program
--  Average/Worst Case - It's impossible to tell whether or not the program terminates
--      Due to the nature of loops in the program, and arbitrarily deep
--      expression trees with function application, it's possible to construct
--      an infinite depth recursion.
--      This is not ideal, and would cause the server to hang indefinitely
--      however, this is an example of the halting problem, and there is no way to
--      verify that a program can terminated (as proven by Turing[1])
interpret :: Program -> Memory -> Either Err Memory
interpret []     m = Right m
interpret (p:ps) m = exec p m >>= interpret ps

-- | Evaluates an expression with the current memory. Returns either 
-- an Int value when successful or an Error if there was an error in
-- computation.
eval :: Expr -> Memory -> Either Err Int

-- ValE is just an explicit value. 
-- Performace: O(1) - Only ever returns the integer value
eval (ValE i) _ = Right i

-- VarE is a reference to a variable. Performance: O(n) time as it uses 
-- a linear search to find the value in memory
eval (VarE x) m = memGetVal x m

-- BinOpE represents the application of an operator with two arguments.
-- It first evaluates the left and right arguments and then uses the apply
-- helper function to calculate the result.
-- Performance: O(l + r) time as both left and right expression trees need to be evaluated. 
eval (BinOpE o l r) m = do vl <- eval l m
                           vr <- eval r m
                           apply o vl vr

-- CallFunction evaluates a call to a subroutine with a return value
-- with optional argument expressions passed in as well.
-- Performance: 
--  Best Case: O(n^2) since there's a nested list to evaluate all arguments into values.
--  Average/Wost Case: Impossible to tell if it terminates - Since the subroutine could
--      contain an infinite loop.
eval (CallFunction n args) m = do 
    subProg <- memGetProg n m -- Returns the sub program at the memory cell (or exits on error)
    argExprs <- mapM (\x -> eval (snd x) m) args -- This evaluates all arguments into basic values
    let argsNorm = zipWith (\a b -> (a, Val b)) (map fst args) argExprs -- Create a list of memory from arguments
    let mem = setAll argsNorm m -- Merge the arguments with the existing memory
    memory' <- exec subProg mem -- Actually run the subroutine with the new memory
    eval (VarE "subroutine_return") memory' -- Extracts the return value of the subroutine as the value.

-- | Execute a statement with memory context. Returns either the new state of memory
-- or an error from evaluatation.
exec :: Stmt -> Memory -> Either Err Memory

-- Assign statement: Assigns a value in memory. This evaluates an expression then sets memory.
-- Performance: 
--  Best Case: O(n) - linear loops to evaluate the expression, and set memory
--  Average/Worst Case: Impossible to determine if the program terminates - Since the 
--      expression tree can contain a subroutine which does not terminate.
exec AssignStmt{..} m = do x <- eval assignExpr m
                           return $ memSet assignVar (Val x) m

-- If Statement: Evaluates an if condition. If == 0 then interprets the associated body statements.
-- however, if it's zero it procedes to check the else if conditions, until there are none left
-- at which point it just interprets the final else list of statements. 
-- Performance:
--  Best Case: O(n) - A linear recursion through case statements, and the body is the trivial program case
--  Average/Worst Case: Impossible to determine if this terminates. Since any of the body programs could
--      contain an infinite loop which isn't guaranteed to terminate.
exec IfStmt{..} m = do  x <- eval ifCond m
                        if x == 0
                            then case ifElseIf of -- If we have a non-empty list of cast statements pattern match
                                (c, b) : cs -> exec (IfStmt c b cs ifElse) m -- on the first for a recursive call.
                                []          -> interpret ifElse m -- otherwise, we can just move onto the else block.
                            else interpret ifBody m -- If it's true then just "run" the body.

-- Repeat Statement: Repeats a list of statement a given (fixed) amount of times.
-- Unlike a while loop the upper bound on repetitions doesn't change.
-- However, memory state is passed between iterations. 
-- Performance:
--  Best Case: O(n) for n iteractions where the body is the trivial program
--  Average/Worst Case: Program may not terminate since there could be infinite loops in the body.
exec RepeatStmt{..} m = do i <- eval repeatTimesExpr m
                           if i < 0 then -- Don't attempt to loop if the amount is < 0
                               pure m
                           else
                               doMany i m
                        where doMany 0 mem = pure mem -- recursive function to interpret the loop body a number of times
                              doMany i mem = 
                                  case interpret repeatBody mem of
                                    Left (LoopTerminated True m')  -> pure m' -- if break, exit with memory
                                    Left (LoopTerminated False m') -> doMany (i - 1) m' -- if continue, continue
                                    Right m' -> doMany (i - 1) m'
                                    Left err -> Left err
                                              

-- | Represents a sequence of statements which can be run as a subroutine.
-- Executing this statement runs the subroutine with the given memory
-- Performance:
--  Best Case: O(1) when the program is of the trivial case
--  Otherwise, it can not be guaranteed that the subroutine will terminate.
--  since it could contain an infinite loop.
exec DefSubroutine{..} m = case interpret routineProgram m of
                                Left (ExecutionTerminated mem) -> Right mem
                                Left err -> Left $ appendError err $ " in subroutine " ++ routineName
                                Right mem -> Right mem 

-- | Calls a subroutine by name with given input
-- This calls a procedure, which takes the input arguments and reduces them to values,
-- puts them into memory, and interprets the procedure, with the memory after the procedure
-- being passed on as the next state of memory.
-- Performance:
--  Best Case: O(n^2) because of nested loops to set memory; 
--  Average/Worst Case: impossible to tell if program terminates since
--      procedure could use an infinite loop.
exec CallSubroutine{..} m = 
    do  
        subProg <- memGetProg subName m
        argExprs <- mapM (\x -> eval (snd x) m) subInput -- This evaluates all arguments into basic values
        let argsNorm = zipWith (\a b -> (a, Val b)) (map fst subInput) argExprs -- Create a list of memory from arguments
        let mem = setAll argsNorm m
        exec subProg mem


-- | Sets an expression to be the return value for the subroutine.
-- This just calls the exec with AssignStmt to reduce code duplication, as it essentially does the same thing
-- Performance is equivalent to that function.
exec SubroutineReturn{..} m = exec (AssignStmt "subroutine_return" returnValue) m

-- | Returns a value from a subroutine if a given predicate is truthy (continues otherwise)
-- Uses the ExceptionTerminated error value, which is not strictly an error, but forces the
-- interpreter to stop and break out to the next level up.
-- Performance:
--  Best Case: O(1) when expressions are trivial cases
--  Average/Worst Case: Impossible to tell if the program would actually terminate, since
--      expressions could contain function calls with infinite loops.
exec ReturnIfValue{..} m  = do  cond <- eval returnPredicate m
                                value <- eval returnValue m
                                if cond == 0 then
                                    pure m
                                else 
                                    Left $ ExecutionTerminated (("subroutine_return", Val value):m)

-- | Returns from a subprocedure if a predicate is truthy (continues execution otherwise)
-- Similar to return with a value, this uses ExecutionTerminated to break the interpreter out to the
-- next level up.
-- Performance:
--  Best Case: O(1): when expression is a trivial case
--  Otherwise, it's impossible to tell if this function will terminate, Since the 
--      expression could contain a function call with an infinite loop.
exec ReturnIf{..} m = do cond <- eval returnPredicate m
                         if cond == 0 then
                             pure m
                         else Left $ ExecutionTerminated m

-- | Repeats a given code block until a given predicate is truthy 
-- (if false then it interprets the statements again)
-- Performance:
--  Best Case: O(1) loop body is an empty program and the predicate is trivial and true.
--  Average/Worst case: impossible to tell if the program terminates, since the loop body
--      could contain a nested infinite loop
exec RepeatUntilStmt{..} m = do case interpret repeatBody m of
                                    Left (LoopTerminated True m')  -> pure m'
                                    Left (LoopTerminated False m') -> nextStep m'
                                    Right m' -> nextStep m'
                                    Left e   -> Left e
                             where nextStep mem = do 
                                    loop <- eval repeatPredicate mem
                                    if loop == 0 then
                                        exec (RepeatUntilStmt repeatPredicate repeatBody) mem
                                    else
                                        return mem
-- | Repeats a given code block while a given predicate is truthy
-- If false, it stops looping.
-- Performance:
--  Best Case: O(1) - predicate is the trivial case and false
--  Otherwise, it's not guaranteed that the function would finish execution
--      due to infinite loops in a subroutine called in an expression.
exec RepeatWhileStmt{..} m = do loop <- eval repeatPredicate m
                                if loop == 0 
                                then return m
                                else case interpret repeatBody m of
                                    Left (LoopTerminated True m')  -> pure m'
                                    Left (LoopTerminated False m') -> nextStep m'
                                    Right m' -> nextStep m'
                                    Left e -> Left e
                             where nextStep mem = exec (RepeatWhileStmt repeatPredicate repeatBody) mem

-- | Counts a variable from an initial value to a limit, incrementing by `increment` each time
-- N.B. count variable can be directly manipulated in memory unlike normal repeat.
-- Similar to a for loop, but enforces the "<" stopping condition.
-- Works by recursively calling itself, moving the lower limit up by the increment each time
-- until the lower limit is not < upper limit, when it exits with the new state of memory.
-- Performance:
--  Best Case: O(1) no iterations are performed
--  Otherwise: it's impossible to determine whether the loop body would actually terminate
--      for aforementioned reasons.
exec CountStmt{..} m = do from <- eval countInitial m
                          let m' = memSet countVar (Val from) m
                          limit <- eval countLimit m'
                          if from < limit then -- If the counter is still under the limit
                              case interpret countBody m' of -- execute loop body, and depening on the result:
                                      Left (LoopTerminated True m'')  -> pure m'' -- if it's a break, return memory
                                      Left (LoopTerminated False m'') -> nextStep m'' -- if its a continue, move to next step
                                      Right m'' -> nextStep m'' -- if it's successful, continue to next iteration
                                      Left e    -> Left e -- if it's an error just forward that error again
                          else
                              return m' -- If the counter is over the limit, then just end the loop execution.
                       where nextStep mem = do -- Defining the next step, 
                                count <- memGetVal countVar mem -- get the counting value
                                by <- eval countIncrement mem -- get the increment amount
                                exec (CountStmt countVar (ValE $ count + by) countLimit countIncrement countBody) mem
                                -- execute a recursive call on the loop with the lower limit increased by the increment.

-- | ControlStmt represents either a continue or break statement
-- This returns an error value, which will terminate the monadic sequencing
-- which can be caught by the calling function, and the appropriate behaviour can
-- be implemented there.
-- Performance: O(1) as it returns a singular value.
exec ControlStmt{..} m = Left $ LoopTerminated controlBreak m

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

-- | `setAll` is a helper function merges two memory values into one 
-- memory values this reduces space usage, and ensures that each variable
-- appears once in memory.
-- cs = new memory
-- fs = old memory
-- Works by linearly recursing through the 1st memory list, and using uncurry
-- to apply the tuple to memSet (which loops through the second memory set and sets the name to the new value)
-- and then since it's a fold, it uses the resulting value of memory as the second memory list 
-- in the next recursive call.
-- Performance: O(n^2) - since the inner function `memSet` is an O(n) function, and it is applied
--      n times by foldl.
setAll :: Memory -> Memory -> Memory
setAll cs fs = foldl (flip $ uncurry memSet) fs cs


-- | A helper function to lookup a value from memory, wrapped like this
-- to make sure if it's not in memory an UninitialisedMemory error is returned.
-- Works in O(n) time by a linear search through the memory list
memGet :: String -> Memory -> Either Err MemCell
memGet n m = case lookup n m of
                Nothing -> Left $ UninitialisedMemory n
                Just v  -> Right v

-- | Gets an integer value from memory, or raises an appropriate type error.
-- Performance: O(n) - equivalent to memGet
memGetVal :: String -> Memory -> Either Err Int
memGetVal n m = memGet n m >>= value
    where value (Val i) = return i
          value (SubProgram _) = Left $ WrongMemoryType $ show n ++ " expected Value got SubProgram"

-- | Gets a program type value from memory, or raises an appropriate type error.
-- Performance: O(n) - equivalent to memGet
memGetProg :: String -> Memory -> Either Err Stmt
memGetProg n m = memGet n m >>= program
    where program (Val _) = Left $ WrongMemoryType $ show n ++ " expected SubProgram got Value"
          program (SubProgram e) = return e

-- | Helper function for appending an error message to an error (e.g. if the error occured in a fucntion
-- alerting the user in which function the error occured in!)
-- Performance: O(n) for length of string xs, since append is O(n)
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

-- | The apply function takes the operator and two integer arguments, and returns either
-- the result of the operator applied to the arguments or an error value because of an incorrect value.
--
-- For the boolean operators (==, /=, <, >, <=, >=) I used `fromEnum` which returns 0 when false 
-- and 1 for true.
-- Performance: O(1) - singular function application for any input Op, x and y
apply :: Op -> Int -> Int -> Either Err Int
apply Add x y = pure $ x + y
apply Sub x y = pure $ x - y
apply Mul x y = pure $ x * y
apply Div _ 0 = Left $ DivByZeroError ""
apply Div x y = pure $ x `div` y
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
-- References:
--
-- [1] Alan Turing, "On computable numbers, with an application to the Entscheidungsproblem", Proceedings of the London Mathematical Society, Series 2, Volume 42 (1937)
--------------------------------------------------------------------------------