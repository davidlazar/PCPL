module Language.PCPL.Syntax
    ( Program(..)
    , Domino(..)
    , Symbol
    , syms
    , unsyms
    ) where

import Language.UTM.Syntax

data Program = Program
    { startDomino :: Input -> Domino
    , dominos     :: [Domino]
    , separator   :: Symbol
    }

data Domino = Domino [Symbol] [Symbol]
    deriving (Eq, Show)
