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

### IF (CONTROL) STATEMENT

-}------------------------------------------------------------------------------

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
interpret [] m = Right m                            -- Base case for the recursion. If the program is empty, we can just successfully return the
                                                    -- current value of the memory, no further changes needed.
interpret (stmt:program) mem = 
    case exec stmt mem of 
        Left err     -> Left err                    -- If the `exec` function returned an error value, stop execution here with that error.
        Right newMem -> interpret program newMem    -- Else if the new value of memory was returned, recursively move onto executing the next
                                                    -- instruction with the new value of memory.
    where
                                                    -- Call the `exec` function with the current statement and value of memory. This returns
                                                    -- Either an error because of an invalid expression or a new value for memory which has 
                                                    -- been changed as a result of the statement.
        
        -- | Given a binary operator type, and two operands, applies the operator the
        -- operands returning Either an error value because of an invalid operation
        -- or a integer value resulting from the calculation.
        -- For the purposes of boolean operators, only a zero value indicates false.
        apply :: Op -> Int -> Int -> Either Err Int
        apply Add x y = Right $ (+) x y
        apply Sub x y = Right $ (-) x y
        apply Mul x y = Right $ (*) x y
        apply Div _ 0 = Left DivByZeroError
        apply Div x y = Right $ x `div` y
        apply Pow x y = if y < 0 then Left NegativeExponentError else Right $ x ^ y
        apply Equal x y = if x == y then Right 1 else Right 0
        apply Neq x y = if x /= y then Right 1 else Right 0
        apply LessThan x y = if x < y then Right 1 else Right 0
        apply LessOrEqual x y = if x <= y then Right 1 else Right 0
        apply GreaterThan x y = if x > y then Right 1 else Right 0
        apply GreaterOrEqual  x y = if x >= y then Right 1 else Right 0

        -- | Given an expression type, and a value of memory this will evaluate the expression
        -- returning either an error from an invalid operation or invalid variable expression (e.g.
        -- trying to read a value which does not exist), or it returns an integer value resulting
        -- from the expression. 
        -- For the purposes of boolean operations, only a zero value indicated false.
        eval :: Expr -> Memory -> Either Err Int
        eval   (ValE i)  _ = Right i                      -- In the case that 
        eval   (VarE x) []          = Left $ UninitialisedMemory x
        eval e@(VarE x) ((n, v):ms) | x == n    = Right v
                                    | otherwise = eval e ms
        eval   (BinOpE op x y) ms   = 
            case eval x ms of
                Left evalError -> Left evalError
                Right valueX   -> 
                    case eval y ms of
                        Left evalError -> Left evalError
                        Right valueY   -> apply op valueX valueY --if isEvalError then Left evalError else apply op valueX valueY

        -- Executes a statement
        exec :: Stmt -> Memory -> Either Err Memory
        exec (AssignStmt n e) m = 
            case eval e m of
                Left  evalError  -> Left    evalError
                Right value      -> Right $ assign n value m -- if isExprError then Left exprError else Right $ assign' n exprValue m
            where                
                assign :: String -> Int -> Memory -> Memory
                assign vn i []            = [(vn, i)]
                assign vn i ((cn, cv):cs) | vn == cn  = (vn, i)  : cs 
                                          | otherwise = (cn, cv) : assign vn i cs

        -- Executing an IF statement with a predicate, true program, case predicate + program list and false program
        exec (IfStmt predicate t cs f) m = 
            case eval predicate m of
                Left  e -> Left e
                Right 0 -> control cs f m --if isPredError then Left predError else control isPredTruthy t cs f m
                Right _ -> interpret t m
            where
                -- Control takes a list of programs to run and a program to run if false then memory 
                control :: [(Expr, Program)] -> Program -> Memory -> Either Err Memory
                control [] onFalse progMem = interpret onFalse progMem
                control ((expr, onCaseTrue):cases) onFalse progMem = 
                    case eval expr progMem of
                        Left  e -> Left e
                        Right 0 -> control cases onFalse progMem
                        Right _ -> interpret onCaseTrue progMem

        -- Executing a FOR statement. Repeats a program amount times.
        exec (RepeatStmt amountExpr prog) m = 
            case eval amountExpr m of 
                Left e -> Left e
                Right amount -> loop amount prog m --if isAmountError then Left amountError else loop amount prog m
            where
                loop :: Int -> Program -> Memory -> Either Err Memory
                loop 1 p progMem = interpret p progMem
                loop n p progMem = if n > 0 then loop (n - 1) p nextMem else Right progMem
                    where
                        nextMem = fromRight [] res 
                        res = interpret p progMem

--------------------------------------------------------------------------------
