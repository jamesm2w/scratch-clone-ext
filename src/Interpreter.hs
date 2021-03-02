--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
{-------------------------------------------------------------------------------

Implemented by James Macer-Wright (u2003041) 2/2021

My first approach to the interpret function was to define a subfunction which executed
an instruction with a given list of memory cells. Called `exec` this function simply
pattern matched on the type constructors for statements and did one of three functions
accordingly. The first statement was the assign statement. This statement had two parts
the name of the variable to assign to and the expression to assign to the variable.

### EVAL ###
In order to fully implement this functionality I needed another function which was capable
of evaluating expressions into the raw integer format required for memory cells. Which I 
called `eval`. Similarly, `eval` pattern matched over the limited number of type constructors
for expressions. If it was the case that the expression was just an integer value or "ValE" 
then `eval` just returns the integer value associated. This can be thought somewhat of a base
case for `eval`. The next type constructor was when an expression was a reference to a variable.
Denoted by "VarE". This is the reason why `eval` also requires a parameter for a Memory value.
By pattern matching on the head of memory (and the first and second values since it's a tuple)
I can check if the memory cell matches the variable being referenced. If it does, then I can 
return the value associated at that memory cell. However, if it does not, I recursively call
`eval` with the same expression, instead with the tail of the memory, and repeat. Until, of course, 
the list is empty, in which I define a base case that has to return an error because the value was not
defined in memory. By the specification of the language, this is the "UninitialisedMemory String" error type.
Incidentally, this is where I first had to grapple with the "Either" type. In this case it is fairly trivial, 
to wrap the outputs from the functions in a call to "Right" when it was successful, and to wrap in a "Left"
here when I need to return an error.

The next section of the `eval` function was the "BinaryOpE" representing a binary (arithmetic or
logic) operation. This has a fairly similar structure to something like a tree, with two sub-expressions
which need to be evaluated first before being combined with the binary operator to return the value.
Which is what I did, definining a statement which simply put the two recursive calls into a function call to 
the `apply` function which would just "apply" the operator. However, there was an issue with the "Either"
context around the two values. Simply put if either of these values was an error then the `apply` function
wouldn't work, furthermore, the `apply` function could also return an error. My first way to deal with these
error values with by looking up on Hoogle functions for either. What I found was the `fromLeft`, `fromRight`
which would return the left/right value respectively or a default value passed in as well as `isLeft` and `isRight`
which returned bool depending on whether the value was left or right. I then used an if statement to check 
    => if isLeft value then value else ... fromRight 0 value
or similar with fromLeft if the error type was needed. I simply made several helper declarations for each 
of the evaluated sub-expressions x and y, and returned the error if one was Left. 

### APPLY ###
Then I moved onto the `apply` function. The purpose of this was to take the operator type, and apply it to 
the two operands passed in. Simply enough I pattern matched all possible constructors of the type and simply
wrote many times x op y replacing op with the operator. However, I had to take in some special considerations
and my naive approach didn't handle error values. To do this, I then had to go back and wrap all the statements
in Right applications. I used the $ operator to remove some tedious brackets here, and in the special cases where
division was called with a 0 divisor it returns a Left with the error, and if pow was called with a negative exponent,
it's similar. However, what I ended up with was a long list of ugly statements. Especially with the logical operators,
which needed inline if-statements to return 1 or 0 which seemed like a really bad idea. However, I didn't have any
simple fixed on hand, so I decided to re-focus my attention on other parts of the program for the time being.

### ASSIGNMENT STATEMENT ###
Moving back to the assignment statment, I now had a function which would evaluate the expression
passed in and give a value which could be entered into memory. However, I still had the small issue
of error handling. To solve this again I used the `ifLeft` and `fromLeft`, `fromRight` to return the correct
Either type. Since the `eval` function returned an "Either Err Int" type and not "Either Err Program" I had to
get the error values out and re-wrap them in another "Either". To do the actual assignment, I called a helper function
which similar to getting the value in the memory list, recurses from the head down the tail looking for the cell with
a matching name. If there is one, then the memory returned is a new cell with the name and new value consd onto the rest
of the memory. If the cell does not match, then the current cell unaltered is consd to the recursive call on the tail.
The base case for this recursion is when the memory list is empty and there are no values left to check, in this case 
the new variable can be added to the list by returning the list with only the one cell inside. This is then passed up the 
recursive call, being consd with all the other cells returning the new value of memory with the new variable.
This all returns the new value of memory, and is wrapped in a "Right" constructor for finally being returned from `exec`.
I think on balance this is probably the most efficient way to write this function, it only requires 1 traversal of the whole
memory in the worst case. However, I would say that this interpreter suffers from using linked lists to represent program memory,
as in computers memory is supposed to be instant access. Having to traverse a list to find/update the value of something is inefficient,
and some other structure like a map should be used instead -- which would allow constant access to memory, and the program wouldn't 
encounter performance issues with long programs.
What I do not think is particularly elegant is the error handling with Either used. I think my solution isn't very elegant, relies on
too many auxilliary statements and probably could and should be simplified. However, I want to first create a full implementation, so
next I will implement the control statement.

### IF (CONTROL) STATEMENT ###
For the if statement, I first began with a similar `ifLeft` `fromLeft` construction
which then checks the result is not equal to 0. This indicates true. In this case it
recursively calls the interpret function with the list of statements - Program - to 
run. However, if the result of evaluating the expression was 0 - indicative of false
it calls the internal function  `control` which just takes the list of cases, the on
false program and the value of memory. It then recurses through the list evaluating
the "else if" case expressions which is the first element of the tuple. And if the 
value is truthy then it again recursively calls the main interpret function to exec
-ute the sub-function, if its false it continues the recursive call. Finally in the
case that there are no more "else if" cases it finally executes the "else" condition
program. I had to take special care here, that the value of memory in the recursive
call was correct. Since only one block of code should ever be executed the memory
passed should be the same that was passed in and not be changed based upon any execution.

### REPEAT (LOOP) STATEMENT ###
For the repeat statement I have to first evaluate the statement which gives the
amount of times to loop. But since that could return an Left value, I used the
same `isLeft` check for the error value, if there wasn't an error I just called
the `loop` function which is defined to recursively execute on an amount, taking away one
until it reaches 1 where it just executes the program once by calling the interpret
function. However, there was a small issue with if a negative number was entered. 
It would enter an unbounded iteration taking one away from -1 would never get to 
one. So instead I added an if statement on the general case for n being positive
if n was negative then it just returns the memory with nothing changed. 
To evaluate the value of memory for each recursive call it gets the value from
a where-bound definition `newMem` which is a call to interpret with the sub-program.

### Improving Either handling. Case Statements & Applicative ###
The next step I took to further streamline was replacing if statements with case expressions
which could pattern match for either Left and an error value, and just return that. 
Or for Right value and then use the value for the next function call. However, this had it's own
problems. Mainly creating very verbose code which hurt my head to look at. 
To improve this I decided to look for functions which could abstract some verbosity away by
applying the value with a context to a function, to get another value in the same context.

I found in some places I could simply use fmap to apply the function to the results. However
in others I would end up with nesteded contexts (Either Err (Either Err ...)). To fix this I 
needed to use the `bind` functionality from Monads with the `>>=` operator.

This reduced
    case exec stmt mem of 
       Left err     -> Left err                   
       Right newMem -> interpret program newMem 
into just
    exec stmt mem >>= interpret program
which reads much nicer as "execute statement with memory then inerpret program" than
"case of executing statement with memory is Left then return Left, if it's Right with value of 
new memory then repreturn interpret program with new memory."


-}------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Interpreter where

--------------------------------------------------------------------------------

import Language

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, MemCell)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    | WrongMemoryType String            -- ^ Tried to perform an operation on
                                        -- a memory cell which had the wrong type
    | FinishedExecutionError            -- ^ The program has finished execution 
                                        -- and shouldn't do anything further.
    deriving (Eq, Show)

appendError :: Err -> String -> Err
appendError (UninitialisedMemory x) xs = UninitialisedMemory $ x ++ xs
appendError (WrongMemoryType x) xs = WrongMemoryType $ x ++ xs
appendError err _ = err

--------------------------------------------------------------------------------

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] m = Right m                            -- Base case for the recursion. If the program is empty, we can just successfully return the
                                                    -- current value of the memory, no further changes needed.
interpret (stmt:program) mem   = exec stmt mem >>= interpret program
    --case exec stmt mem of 
    --   Left err     -> Left err                   -- If the `exec` function returned an error value, stop execution here with that error.
    --   Right newMem -> interpret program newMem   -- Else if the new value of memory was returned, recursively move onto executing the next
                                                    -- instruction with the new value of memory.

                                                    -- Call the `exec` function with the current statement and value of memory. This returns
                                                    -- Either an error because of an invalid expression or a new value for memory which has 
                                                    -- been changed as a result of the statement.
        
-- | Given a binary operator type, and two operands, applies the operator the
-- operands returning Either an error value because of an invalid operation
-- or a integer value resulting from the calculation.
-- For the purposes of boolean operators, only a zero value indicates false.
apply :: Op -> Int -> Int -> Either Err Int
apply Add             x y = Right $ x + y
apply Sub             x y = Right $ x - y
apply Mul             x y = Right $ x * y
apply Div             _ 0 = Left DivByZeroError
apply Div             x y = Right $ x `div` y
apply Pow             x y = if y <  0 then Left NegativeExponentError else Right $ x ^ y
apply Equal           x y = Right $ fromEnum $ x == y
apply Neq             x y = Right $ fromEnum $ x /= y
apply LessThan        x y = Right $ fromEnum $ x  < y
apply LessOrEqual     x y = Right $ fromEnum $ x <= y
apply GreaterThan     x y = Right $ fromEnum $ x  > y
apply GreaterOrEqual  x y = Right $ fromEnum $ x >= y

-- | Given an expression type, and a value of memory this will evaluate the expression
-- returning either an error from an invalid operation or invalid variable expression (e.g.
-- trying to read a value which does not exist), or it returns an integer value resulting
-- from the expression. 
-- For the purposes of boolean operations, only a zero value indicated false.
eval :: Expr -> Memory -> Either Err Int
eval   (ValE i)  _ = Right i                      -- In the case that 

-- | Evaluate a variable expression (fetches a variable from memory)
eval   (VarE x) []                     = Left $ UninitialisedMemory x
eval e@(VarE x) ((n, Val v) : ms)      | x == n    = Right v
                                       | otherwise = eval e ms
eval e@(VarE x) ((n, SubProgram _):ms) | x == n    = Left $ WrongMemoryType $ n ++ " is of type SubProgram not Value"
                                       | otherwise = eval e ms

-- | Evaluate a binary operator (+, -, /, *, etc.)
eval   (BinOpE op x y) ms   = do valX <- eval x ms
                                 valY <- eval y ms
                                 apply op valX valY

-- | Evaluate a call to a subroutine function which returns a value. Basically loops through memory trying to find
-- the subroutine cell. If found it calls interpret on the subroutine and then inspects the memory for the return variable.
-- the subroutine call should be accompanied by some memory which will be the memory for the subroutine's local working.
eval (CallFunction name inputs) progMem = 
    do 
        let inputStatements = [AssignStmt n e | (n, e) <- inputs]
        subprog <- findProgramCell ((name ==) . fst) progMem
        memory  <- interpret inputStatements progMem
        memory' <- exec subprog memory
        eval (VarE "subroutine_return") memory'
    where
        findProgramCell :: ((String, MemCell) -> Bool) -> Memory -> Either Err Stmt
        findProgramCell _ [] = Left $ UninitialisedMemory name
        findProgramCell p ((_, Val _):xs) = findProgramCell p xs
        findProgramCell p (n@(_, SubProgram r):xs) = if p n then Right r else findProgramCell p xs

-- Executes a statement
exec :: Stmt -> Memory -> Either Err Memory
exec (AssignStmt n e) m = 
    do 
        x <- eval e m
        return $ assign n x m
    where
        assign :: String -> Int -> Memory -> Memory
        assign vn i []              = [(vn, Val i)]                   
        assign vn i ((cn, cv) : cs) | vn == cn  = (vn, Val i)  : cs 
                                    | otherwise = (cn, cv) : assign vn i cs

-- Executing an IF statement with a predicate, true program, case predicate + program list and false program
exec (IfStmt predicate t cs f) m = control ((predicate, t):cs) f m
    where
        -- Control takes a list of programs to run and a program to run if false then memory 
        control :: [(Expr, Program)] -> Program -> Memory -> Either Err Memory
        control [] onFalse progMem = interpret onFalse progMem
        control ((expr, onCaseTrue):cases) onFalse progMem = eval expr progMem >>= (\x -> 
            if x == 0 
                then 
                    control cases onFalse progMem 
                else 
                    interpret onCaseTrue progMem
            )

-- Executing a REPEAT statement. Repeats a program amount times.
exec (RepeatStmt amountExpr prog) m = 
    do
        x <- eval amountExpr m 
        --traceM ("repeat " ++ show x)
        loop x prog m
    where
        loop :: Int -> Program -> Memory -> Either Err Memory
        loop 0 _ progMem = Right progMem
        loop n p progMem = 
            if n < 0 then
                Right progMem 
            else do
                nMem <- interpret p progMem
                loop (n - 1) p nMem

-- | Runs subroutine with given name, program and memory
exec (DefSubroutine name prog) m = case interpret prog m of
    Left  err -> Left $ appendError err $ " in subroutine " ++ name
    Right mem -> Right mem

-- | Executes a call to a subroutine which doesn't return a value... wait whats the point of this?
-- Perhaps have this be a procedure which uses global memory?
exec (CallSubroutine name inputs) progMem = 
    do 
        let inputStatements = [AssignStmt n e | (n, e) <- inputs]
        subprog <- findProgramCell ((name ==) . fst) progMem
        
        memory <- interpret inputStatements progMem
        exec subprog memory
    where
        findProgramCell :: ((String, MemCell) -> Bool) -> Memory -> Either Err Stmt
        findProgramCell _ [] = Left $ UninitialisedMemory name
        findProgramCell p ((_, Val _):xs) = findProgramCell p xs
        findProgramCell p (n@(_, SubProgram r):xs) = if p n then Right r else findProgramCell p xs

-- | Returns from a subroutine execution with an expression
exec (SubroutineReturn e) mem = exec (AssignStmt "subroutine_return" e) mem

--------------------------------------------------------------------------------
