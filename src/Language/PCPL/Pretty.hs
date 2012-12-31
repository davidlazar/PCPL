{-# LANGUAGE OverloadedStrings #-}
module Language.PCPL.Pretty where

import Prelude hiding (div)
import Data.List
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes

import Language.PCPL.Syntax

class Pretty a where
    pretty :: a -> String

instance Pretty Domino where
    pretty (Domino xs ys)
        = "{" ++ unsyms xs ++ " | " ++ unsyms ys ++ "}"

instance Pretty Program where
    pretty pgm = intercalate ", " $ map pretty ds
      where
        ds = startDomino pgm ["$IN"] : dominos pgm

instance Show Program where
    showsPrec _ pgm = showString ("Program [" ++ pretty pgm ++ "]")

instance ToMarkup Domino where
    toMarkup (Domino xs ys) = div ! class_ "d" $ do
        div (toMarkup $ unsyms xs)
        div (toMarkup $ unsyms ys)

instance ToMarkup Program where
    toMarkup pgm = div ! class_ "pcp" $ mapM_ toMarkup ds
      where
        ds = startDomino pgm ["$IN"] : dominos pgm
