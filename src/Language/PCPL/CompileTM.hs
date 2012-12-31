-- | This module implements the translation from Turing machines to PCP
-- described in Sipser's /Theory of Computation/.
module Language.PCPL.CompileTM
    ( compileTM
    , compileTM'
    , Seperator
    ) where

import qualified Data.Map as Map
import Language.UTM.Syntax
import Language.PCPL.Syntax

-- | Symbol used to separate TM configurations
type Seperator = Symbol

compileTM :: TuringMachine -> Program
compileTM = compileTM' (Symbol "#")

compileTM' :: Seperator -> TuringMachine -> Program
compileTM' s tm = Program
    { startDomino = part1 s tm
    , dominos = concatMap (\f -> f s tm)
        [part2, part3, part4, part5, part6, part7]
    , separator = s
    }

part1 :: Seperator -> TuringMachine -> Input -> Domino 
part1 s tm w = Domino [s] ([s, stateSymbol (startState tm)] ++ w ++ [s])

part2 :: Seperator -> TuringMachine -> [Domino]
part2 _ tm = Map.foldrWithKey actionR [] (transitionFunction tm)

actionR :: (State, Symbol) -> (State, Symbol, Action) -> [Domino] -> [Domino]
actionR (q, a) (r, b, R) ds = Domino [stateSymbol q, a] [b, stateSymbol r] : ds
actionR _ _ ds = ds

part3 :: Seperator -> TuringMachine -> [Domino]
part3 _ tm = Map.foldrWithKey (actionL tm) [] (transitionFunction tm)

actionL :: TuringMachine -> (State, Symbol) -> (State, Symbol, Action) -> [Domino] -> [Domino]
actionL tm (q, a) (r, b, L) ds
    = [Domino [c, stateSymbol q, a] [stateSymbol r, c, b] | c <- tapeAlphabet tm] ++ ds
actionL _ _ _ ds = ds

part4 :: Seperator -> TuringMachine -> [Domino]
part4 _ tm = [Domino [a] [a] | a <- tapeAlphabet tm]

part5 :: Seperator -> TuringMachine -> [Domino]
part5 s tm = [Domino [s] [s], Domino [s] [blankSymbol tm, s]]

part6 :: Seperator -> TuringMachine -> [Domino]
part6 _ tm = concat [[Domino [a, q] [q], Domino [q, a] [q]] | a <- tapeAlphabet tm]
  where
    q = stateSymbol (acceptState tm)

part7 :: Seperator -> TuringMachine -> [Domino]
part7 s tm = [Domino [stateSymbol (acceptState tm), s, s] [s]]
 
stateSymbol :: State -> Symbol
stateSymbol (State s) = Symbol s
