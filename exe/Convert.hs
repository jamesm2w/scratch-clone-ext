--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import Debug.Trace

--------------------------------------------------------------------------------

-- | Represents a Google Blockly XML document.
data Doc = Doc {
    docVars    :: [String],  -- ^ A list of variable names.
    docProgram :: Program    -- ^ The program.
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
parseVar (Element {..}) = do
        v <- content elementNodes
        return (unpack v)

-- | Parses the global variables section.
parseVars :: Parser Element [String]
parseVars (Element {..}) =
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

-- | Parses the body of an expression block.
parseExprTy :: Text -> Parser Element Expr
parseExprTy "math_number"     = parseMathNumber
parseExprTy "math_arithmetic" = parseBinOp
parseExprTy "logic_compare"   = parseBinOp
parseExprTy "variables_get"   = parseVarGet
parseExprTy ty                = const $ throwE $
    "Unknown block type: " ++ unpack ty

-- | Parses the body of a statement block.
parseStmtTy :: Text -> Parser Element Program
parseStmtTy "entry_point"         = parseEntryPoint
parseStmtTy "variables_set"       = parseVarSet
parseStmtTy "controls_repeat_ext" = parseRepeat
parseStmtTy "controls_if"         = parseControlIf
parseStmtTy ty = const $ throwE $
    "Unknown block type: " ++ unpack ty

-- | Parses a block, including its header.
parseExpr :: Parser Element Expr
parseExpr e@Element {..} = case M.lookup "id" elementAttributes of
    Nothing -> throwE $
        "Block " ++ unpack (nameLocalName elementName) ++
        " is missing attribute: id"
    Just _  -> case M.lookup "type" elementAttributes of
        Nothing -> throwE "Block is missing attribute: type"
        Just ty -> parseExprTy ty e

-- | Parses a block, including its header.
parseStmt :: Parser Element Program
parseStmt e@Element {..} = case M.lookup "id" elementAttributes of
    Nothing -> throwE $
        "Block " ++ unpack (nameLocalName elementName) ++
        " is missing attribute: id"
    Just _  -> case M.lookup "type" elementAttributes of
        Nothing -> throwE "Block is missing attribute: type"
        Just ty -> parseStmtTy ty e

-- | Parses a Google Blockly document.
parseDoc :: Parser Element Doc
parseDoc Element {..} = do
    ve <- require "Variables section is missing!" $
            element "variables" elementNodes
    vs <- parseVars ve
    bs <- trace ("no. of top level blocks to parse " ++ show (Prelude.length (elementsByName "block" elementNodes))) 
        (mapM parseStmt (elementsByName "block" elementNodes)) -- TODO: only parse the entry point and ignore everything else
    return $ Doc vs (Prelude.concat bs)

-- | Tries to convert a byte string into a document.
convert :: BS.ByteString -> Either String Doc
convert = runExcept . parseDoc . documentRoot . parseLBS

--------------------------------------------------------------------------------
