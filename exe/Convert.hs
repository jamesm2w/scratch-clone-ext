--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone : Extended Edition                             --
--------------------------------------------------------------------------------
-- Extensions in this file:
--      Lots of parsers for extended functionality defined in src/Interpreter.hs 
--          & src/Language.hs
--      Parsing only from start blocks. Also parsing shadow blocks correctly.
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains functions to convert XML generated by
-- Google Blockly into an AST suitable for interpretation.
module Convert where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Trans.Except

import qualified Data.ByteString.Lazy as BS
import Data.Text hiding (concat)
import qualified Data.Map as M
import Data.Maybe

import Text.XML hiding (parseLBS)
import Text.HTML.DOM

import Language

--------------------------------------------------------------------------------

-- | Represents a Google Blockly XML document.
data Doc = Doc {
    docVars        :: [String],           -- ^ A list of variable names.
    docProgram     :: Program,            -- ^ The program.
    docSubroutines :: [(String, Stmt)]    -- ^ Any attached subroutines to the program
} deriving Show

--------------------------------------------------------------------------------

-- | Parsers from values of type r to those of type a.
type Parser r a = r -> Except String a

-- | If the node is an element, return it.
nodeToElement :: Node -> Maybe Element
nodeToElement (NodeElement e) = Just e
nodeToElement _               = Nothing

-- | If the node represents the content of an element, return it.
nodeToContent :: Node -> Maybe Text
nodeToContent (NodeContent t) = Just t
nodeToContent _               = Nothing

-- | Parses the content of an XML element.
content :: Parser [Node] Text
content = require "Missing content!" . listToMaybe . mapMaybe nodeToContent

-- | Returns all elements which satisfy the predicate.
elements :: (Element -> Maybe Element) -> [Node] -> [Element]
elements p = mapMaybe (nodeToElement >=> p)

-- | Returns all elements with the specified name.
elementsByName :: Name -> [Node] -> [Element]
elementsByName n = elements p
    where p e | elementName e == n = Just e
              | otherwise          = Nothing

-- | Returns all blocks/shadows from given nodes.
-- Shadows are default blocks which can be included in statements. This function
-- just makes sure the parser includes them as well as the actual blocks.
blocksAndShadows :: [Node] -> [Element]
blocksAndShadows ns = elementsByName "block" ns ++ elementsByName "shadow" ns

-- | Returns all the start elements. That is all blocks with a type=entry_point
startElements :: [Node] -> [Element]
startElements ns = Prelude.filter 
    (\Element{..} -> case M.lookup "type" elementAttributes of 
        Just "entry_point" -> True 
        _ -> False )
    (elementsByName "block" ns)

-- | Returns all blocks which define proceudres.
procedureElements :: [Node] -> [Element]
procedureElements ns = Prelude.filter 
    (\Element{..} -> case M.lookup "type" elementAttributes of
        Just "procedures_defnoreturn" -> True
        Just "procedures_defreturn" -> True
        _ -> False )
    (elementsByName "block" ns)

-- | Returns the first element with the specified name.
element :: Name -> [Node] -> Maybe Element
element n = listToMaybe . elementsByName n

-- | Returns the first element which has the specified name
-- and name attribute.
node :: Name -> Text -> [Node] -> Maybe Element
node elemName attrName = listToMaybe . elements p
    where p e | elementName e == elemName = do
                    name <- M.lookup "name" (elementAttributes e)
                    if name == attrName then Just e else Nothing
              | otherwise = Nothing

-- | Returns the first field with the specified name.
field :: Text -> Parser [Node] Text
field n ns = case node "field" n ns of
    Nothing -> throwE $ "Missing field: " ++ unpack n
    Just e  -> content (elementNodes e)

-- | Returns all values with the specified name.
values :: Text -> Parser [Node] [Expr]
values n ns = case node "value" n ns of
    Nothing -> return []
    Just e  -> mapM parseExpr (blocksAndShadows (elementNodes e))

-- | Returns the first value with the specified name.
value :: Text -> Parser [Node] (Maybe Expr)
value t ns = listToMaybe `fmap` values t ns

statement :: Text -> Parser Element Program
statement n e = case node "statement" n (elementNodes e) of
    Nothing -> return []
    Just e' -> Prelude.concat <$>
        mapM parseStmt (blocksAndShadows (elementNodes e'))

mutations :: Parser Element (M.Map Name Text)
mutations e = case element "mutation" (elementNodes e) of
    Nothing -> return M.empty
    Just e' -> return (elementAttributes e')

-- | Requires an optional parser to succeed.
require :: String -> Parser (Maybe a) a
require ex Nothing  = throwE ex
require _  (Just x) = return x

-- | Runs an optional parser and requires it to succeed.
force :: String -> Parser (Except String (Maybe a)) a
force ex m = m >>= require ex

--------------------------------------------------------------------------------

-- | Parses a single global variable.
parseVar :: Parser Element String
parseVar Element {..} = do
        v <- content elementNodes
        return (unpack v)

-- | Parses the global variables section.
parseVars :: Parser Element [String]
parseVars Element {..} =
    mapM parseVar (elementsByName "variable" elementNodes)

parseNext :: Parser Element Program
parseNext e = case element "next" (elementNodes e) of
    Nothing -> return []
    Just e' -> Prelude.concat <$>
        mapM parseStmt (blocksAndShadows (elementNodes e'))

parseVarGet :: Parser Element Expr
parseVarGet e = do
    var <- field "VAR" (elementNodes e)
    return $ VarE (unpack var)

parseMathNumber :: Parser Element Expr
parseMathNumber e = do
    v <- field "NUM" (elementNodes e)
    return $ ValE (read $ unpack v)

parseBinOp :: Parser Element Expr
parseBinOp e = do
    op <- field "OP" (elementNodes e)
    l <- force "Left sub-expression is required" $ value "A" (elementNodes e)
    r <- force "Right sub-expression is required" $ value "B" (elementNodes e)
    return $ BinOpE (toOp op) l r
    where
        toOp :: Text -> Op
        toOp "ADD"      = Add
        toOp "MINUS"    = Sub
        toOp "MULTIPLY" = Mul
        toOp "DIVIDE"   = Div
        toOp "POWER"    = Pow
        toOp "EQ"       = Equal
        toOp "NEQ"      = Neq
        toOp "LT"       = LessThan
        toOp "LTE"      = LessOrEqual
        toOp "GT"       = GreaterThan
        toOp "GTE"      = GreaterOrEqual
        toOp _          = error "(toOp) Unknown operator"

parseEntryPoint :: Parser Element Program
parseEntryPoint = parseNext

parseVarSet :: Parser Element Program
parseVarSet e = do
    var <- field "VAR" (elementNodes e)
    val <- force "Value is required." $ value "VALUE" (elementNodes e)
    n <- parseNext e
    return $ AssignStmt (unpack var) val : n

parseRepeat :: Parser Element Program
parseRepeat e = do
    val <- force "Number of times is required." $
                value "TIMES" (elementNodes e)
    blk <- statement "DO" e
    n <- parseNext e
    return $ RepeatStmt val blk : n

parseIf :: Int -> Parser Element (Expr, Program)
parseIf i e = let n = pack (show i) in do
    cond <- force "Condition is required." $
                value ("IF" `append` n) (elementNodes e)
    stmt <- statement ("DO" `append` n) e
    return (cond, stmt)

parseControlIf :: Parser Element Program
parseControlIf e = do
    muts <- mutations e
    let elseif = fromMaybe 0 (read . unpack <$> M.lookup "elseif" muts)
    (cond,blk) <- parseIf 0 e
    alts <- mapM (flip parseIf e) [1..elseif]
    els <- statement "ELSE" e
    p <- parseNext e
    return $ IfStmt cond blk alts els : p

-- | Parses a call to a function which returns a value
-- requires the name, and any arguments to equal number of arguments on function.
parseCallReturn :: Parser Element Expr
parseCallReturn e = do
    let mutators = elementsByName "mutation" (elementNodes e)
    let subname = mapM (\x -> unpack <$> M.lookup "name" (elementAttributes x)) mutators -- finds name
    name <- case subname of
        Nothing    -> throwE "Subroutine name is undefined."
        Just []    -> throwE "Subroutine name is undefined."
        Just (x:_) -> return x

    let args = elementsByName "arg" (elementNodes $ Prelude.head mutators)
    let argNames = fromMaybe [] $ mapM (\x -> unpack <$> M.lookup "name" (elementAttributes x)) args
    let argValues = Prelude.concat $
         mapM (elementsByName "block" . elementNodes) $ elementsByName "value" (elementNodes e)

    argExprs <- mapM parseExpr argValues -- parse arguments into expressions

    when (Prelude.length args /= Prelude.length argNames) $ -- if lengths don't match then something's gone wrong
        throwE $ "Number of arguments don't match parameters in " ++ name
        
    let xs = Prelude.zip argNames argExprs -- join argument names with their expressions.
    return $ CallFunction name xs

-- | Defines a new subroutine. This is agnostic of whether there is a return value or not
-- Requires a name, as well as a stack value for the actuall function "code"
parseDefSubroutine :: Parser Element Stmt
parseDefSubroutine e = do
    name <- field "NAME" (elementNodes e)
    let stack = filterM (\x -> M.lookup "name" (elementAttributes x) >>= (\y -> Just $ unpack y == "STACK")) (elementsByName "statement" (elementNodes e))
    
    stmts <- case stack of
        Nothing -> pure []
        Just [] -> pure []
        Just (x:_) -> mapM parseStmt (blocksAndShadows (elementNodes x)) -- parse the stack into the stmt list.

    retE <- value "RETURN" (elementNodes e)
    ret <- case retE of 
        Nothing -> pure [] -- If we have no return value, no extra operations need to happen
        Just expr -> pure [SubroutineReturn expr] -- but if there is a value, the final stmt is to return that value

    return $ DefSubroutine (unpack name) $ Prelude.concat stmts ++ ret

-- | Defines a new call to a function which does not have a return value
parseCallNoReturn :: Parser Element Program
parseCallNoReturn e = do
    let mutators = elementsByName "mutation" (elementNodes e)
    let subname = mapM (\x -> unpack <$> M.lookup "name" (elementAttributes x)) mutators
    name <- case subname of
        Nothing    -> throwE "Subroutine name is undefined."
        Just []    -> throwE "Subroutine name is undefined."
        Just (x:_) -> return x

    let args = elementsByName "arg" (elementNodes $ Prelude.head mutators)
    let argNames = fromMaybe [] $ mapM (\x -> unpack <$> M.lookup "name" (elementAttributes x)) args
    let argValues = Prelude.concat $
         mapM (elementsByName "block" . elementNodes) $ elementsByName "value" (elementNodes e)

    argExprs <- mapM parseExpr argValues

    when (Prelude.length args /= Prelude.length argNames) $ 
        throwE $ "Number of arguments don't match parameters in " ++ name    
    let xs = Prelude.zip argNames argExprs

    p <- parseNext e
    return $ CallSubroutine name xs : p

-- | Parses a return statement with a predicate
-- Enforces presence of a CONDITION value and a value for whether there is an attached value.
parseReturnStmt :: Parser Element Program
parseReturnStmt e = do
    p <- parseNext e
    mutators <- mutations e
    let tv = M.lookup "value" mutators
    ty <- case tv of
        Nothing -> throwE "Return type not present"
        Just "0" -> return False
        Just "1" -> return True
        Just _ -> throwE "Invalid return type"
    condE <- force "CONDITION is required" $ value "CONDITION" (elementNodes e) 
    if ty then
        do  -- Returns a value
            rValE <- force "return VALUE is required" $ value "VALUE" (elementNodes e)
            return $ ReturnIfValue condE rValE : p
    else
        do  -- Does not return value
            return $ ReturnIf condE : p

-- | Parse a modulo expression block
-- Requires the DIVIDEND and DIVISOR values.
parseMathModulo :: Parser Element Expr
parseMathModulo e = do 
    dividendE <- force "DIVIDEND is required" $ value "DIVIDEND" (elementNodes e)
    divisorE <- force "DIVISOR is required" $ value "DIVISOR" (elementNodes e) 
    return $ BinOpE Mod dividendE divisorE

-- | Parse a while/until loop. Enforces that there is a "BOOL" value which is the predicate for the loop
-- As well as a "MODE" field which controls which type of loop it is.
parseWhileUntil :: Parser Element Program
parseWhileUntil e = do 
    p <- parseNext e
    predE <- force "BOOL predicate is required" $ value "BOOL" (elementNodes e)
    block <- statement "DO" e
    mode <- field "MODE" (elementNodes e)
    stmt <- case mode of 
        "UNTIL" -> return $ RepeatUntilStmt predE block
        "WHILE" -> return $ RepeatWhileStmt predE block
        _ -> throwE "Mode not recognised"
    return $ stmt : p

-- | Parse a for loop (count) statement into the program. Enforces the presence of the limits
-- and increment values.
parseForStmt :: Parser Element Program
parseForStmt e = do
    p <- parseNext e
    fromE <- force "FROM is required" $ value "FROM" (elementNodes e)
    toE <- force "TO is required" $ value "TO" (elementNodes e)
    byE <- force "BY is required" $ value "BY" (elementNodes e)
    variableE <- field "VAR" (elementNodes e)
    block <- statement "DO" e
    return $ CountStmt (unpack variableE) fromE toE byE block : p

-- | Parses a break statement in the program. Doesn't validate if the break is in a loop or not.
parseBreakStmt :: Parser Element Program
parseBreakStmt e = do
    p <- parseNext e
    t <- field "FLOW" (elementNodes e)  
    return $ ControlStmt (unpack t == "BREAK") : p

-- | Parses the body of an expression block.
parseExprTy :: Text -> Parser Element Expr
parseExprTy "math_number"           = parseMathNumber
parseExprTy "math_arithmetic"       = parseBinOp
parseExprTy "logic_compare"         = parseBinOp
parseExprTy "variables_get"         = parseVarGet
parseExprTy "procedures_callreturn" = parseCallReturn
parseExprTy "math_modulo" = parseMathModulo
parseExprTy ty                = const $ throwE $
    "Unknown block type (expr): " ++ unpack ty

-- | Parses the body of a statement block.
parseStmtTy :: Text -> Parser Element Program
parseStmtTy "entry_point"         = parseEntryPoint
parseStmtTy "variables_set"       = parseVarSet
parseStmtTy "controls_repeat_ext" = parseRepeat
parseStmtTy "controls_if"         = parseControlIf
parseStmtTy "procedures_callnoreturn" = parseCallNoReturn
parseStmtTy "procedures_ifreturn" = parseReturnStmt
parseStmtTy "controls_whileUntil" = parseWhileUntil
parseStmtTy "controls_for"        = parseForStmt
parseStmtTy "controls_flow_statements" = parseBreakStmt
parseStmtTy ty = const $ throwE $
    "Unknown block type (stmt): " ++ unpack ty

-- | Parses a block, including its header.
parseExpr :: Parser Element Expr
parseExpr e@Element {..} = case M.lookup "id" elementAttributes of
    Nothing -> throwE $
        "Block (expr) " ++ unpack (nameLocalName elementName) ++
        " is missing attribute: id"
    Just _  -> case M.lookup "type" elementAttributes of
        Nothing -> throwE "Block is missing attribute: type"
        Just ty -> parseExprTy ty e

-- | Parses a block, including its header.
parseStmt :: Parser Element Program
parseStmt e@Element {..} = case M.lookup "id" elementAttributes of
    Nothing -> throwE $
        "Block (stmt) " ++ unpack (nameLocalName elementName) ++
        " is missing attribute: id"
    Just _  -> case M.lookup "type" elementAttributes of
        Nothing -> throwE "Block is missing attribute: type"
        Just ty -> parseStmtTy ty e

-- | Parse a subroutine statement. Two options:
--  if it has a return value or not.
parseSubroutine :: Parser Element Stmt
parseSubroutine e@Element {..} = case M.lookup "id" elementAttributes of 
    Nothing -> throwE $ "Block (subroutine) " ++ unpack (nameLocalName elementName) ++ " is missing attribute: id"
    Just _ -> case M.lookup "type" elementAttributes of
        Nothing -> throwE "Block is missing attribute: type"
        Just ty -> case ty of
            "procedures_defnoreturn" -> parseDefSubroutine e
            "procedures_defreturn"   -> parseDefSubroutine e
            _ -> throwE $ "Unknown block type (procedures): " ++ unpack ty

-- | Parses a Google Blockly document.
parseDoc :: Parser Element Doc
parseDoc Element {..} = do
    ve <- require "Variables section is missing!" $
            element "variables" elementNodes
    vs   <- parseVars ve
    bs   <- mapM parseStmt (startElements elementNodes) -- Parses all blocks attached to the start node.
    subs <- mapM parseSubroutine (procedureElements elementNodes) -- parse all subroutine definition blocks in the XML
    let memory = Prelude.map (\s@(DefSubroutine n _) -> (n, s)) subs -- pass the subroutines back into the Main script

    return $ Doc vs (Prelude.concat bs) memory

-- | Tries to convert a byte string into a document.
convert :: BS.ByteString -> Either String Doc
convert = runExcept . parseDoc . documentRoot . parseLBS

--------------------------------------------------------------------------------
