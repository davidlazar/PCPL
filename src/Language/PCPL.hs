{-# LANGUAGE OverloadedStrings #-}
-- | Entry point to the Post Correspondence Programming Language
module Language.PCPL
    ( module Language.PCPL.Syntax
    , module Language.PCPL.Pretty
    , module Language.PCPL.CompileTM

    -- * Execute PCPL programs
    , runProgram

    -- * Utility
    , topString

    -- * Examples
    , unaryAdder
    , parensMatcher 
    ) where

import qualified Data.Map as Map
import Language.UTM.Syntax

import Language.PCPL.Syntax
import Language.PCPL.Pretty
import Language.PCPL.Solver
import Language.PCPL.CompileTM

-- ^ Run a PCPL program on the given input, producing a PCP match.
runProgram :: Program -> Input -> [Domino]
runProgram pgm w = map (dss !!) indices
  where
    dss@(d : ds) = (startDomino pgm w) : dominos pgm
    Just initialConfig = updateConf (Top []) d
    indices = reverse $ search (zip [1..] ds) [Node [0] initialConfig]

topString :: [Domino] -> String
topString ds = concat [ s | Domino xs _ <- ds, Symbol s <- xs]

unaryAdder :: TuringMachine
unaryAdder = TuringMachine
    { startState = "x"
    , acceptState = "a"
    , rejectState = "reject"
    , blankSymbol = "_"
    , inputAlphabet = ["1", "+"]
    , transitionFunction = Map.fromList
        [ (("x", "1"), ("x", "1", R))
        , (("x", "+"), ("y", "1", R))
        , (("y", "1"), ("y", "1", R))
        , (("y", "_"), ("z", "_", L))
        , (("z", "1"), ("a", "_", R))
        , (("z", "+"), ("a", "_", R))
        ]
    }

parensMatcher :: TuringMachine
parensMatcher = TuringMachine
    { startState = "S"
    , acceptState = "Y"
    , rejectState = "N"
    , blankSymbol = "_"
    , inputAlphabet = syms "$()A"
    , transitionFunction = Map.fromList
        [ (("S", "$"), ("0", "$", R))
        , (("0", "("), ("0", "(", R))
        , (("0", ")"), ("1", "A", L))
        , (("0", "A"), ("0", "A", R))
        , (("0", "_"), ("2", "_", L))
        , (("1", "("), ("0", "A", R))
        , (("1", "A"), ("1", "A", L))
        , (("2", "A"), ("2", "A", L))
        , (("2", "$"), ("Y", "$", R))
        ]
    }
