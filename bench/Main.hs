--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Criterion.Main

import Control.DeepSeq

import GHC.Generics

import Language
import Interpreter

--------------------------------------------------------------------------------

instance NFData Err where
deriving instance Generic Err

instance NFData MemCell where
deriving instance Generic MemCell

instance NFData Stmt where
deriving instance Generic Stmt

instance NFData Expr where
deriving instance Generic Expr

instance NFData Op where
deriving instance Generic Op

memory :: Int -> Memory
memory n = [('x' : show i, Val i) | i <- [0..n]]

readProgram :: Int -> Program
readProgram n = [ AssignStmt name (VarE name) ]
    where
        name = 'x' : show n

main :: IO ()
main = defaultMain
    [
        bgroup "Memory access"
        [
            bench "reading and writing (10)" $
                nf (interpret $ readProgram 10) (memory 10),
            bench "reading and writing (50)" $
                nf (interpret $ readProgram 50) (memory 50),
            bench "reading and writing (100)" $
                nf (interpret $ readProgram 100) (memory 100),
            bench "reading and writing (500)" $
                nf (interpret $ readProgram 500) (memory 500)
        ]
    ]

--------------------------------------------------------------------------------
