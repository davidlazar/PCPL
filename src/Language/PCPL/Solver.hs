-- | A simple PCP solver.
-- There is a lot of potential for optimization here.
module Language.PCPL.Solver
    ( PCP
    , Node(..)
    , Configuration(..)
    , search
    , updateConf
    ) where

import Data.Maybe
import Language.PCPL.Syntax

-- | PCP instance
type PCP = [(Int, Domino)]

-- | Node in the search tree
data Node = Node [Int] Configuration

-- | The unmatched portion of an intermediate PCP state.
-- The paper /Tackling Post's Correspondence Problem/
-- calls this a configuration.
data Configuration
    = Top [Symbol]
    -- ^ Portion of the top string that extends past the bottom one
    | Bottom [Symbol]
    -- ^ Portion of the bottom string that extends past the top one
    deriving (Eq, Show)

-- | Find a solution using BFS.
search :: PCP -> [Node] -> [Int]
search pcp level = case filter isSolution level of
    [] -> search pcp cs
    (Node is _:_) -> is
  where
    cs = concatMap (children pcp) level

isSolution :: Node -> Bool
isSolution (Node _ (Top [])) = True
isSolution (Node _ (Bottom [])) = True
isSolution _ = False

children :: PCP -> Node -> [Node]
children pcp n = mapMaybe (nextNode n) pcp

nextNode :: Node -> (Int, Domino) -> Maybe Node
nextNode (Node is c) (i, d) = case updateConf c d of
    Nothing -> Nothing
    Just c' -> Just $ Node (i : is) c'

-- | Try to update the @Configuration@ with the given @Domino@.
updateConf :: Configuration -> Domino -> Maybe Configuration
updateConf (Top ts) (Domino xs ys) = reconfigure (Top $ ts ++ xs) (Bottom ys)
updateConf (Bottom bs) (Domino xs ys) = reconfigure (Top xs) (Bottom $ bs ++ ys)

reconfigure :: Configuration -> Configuration -> Maybe Configuration
reconfigure (Top []) b = Just b
reconfigure t (Bottom []) = Just t
reconfigure (Top (x:xs)) (Bottom (y:ys))
    | x == y = reconfigure (Top xs) (Bottom ys)
    | otherwise = Nothing
reconfigure (Bottom b) (Top t) = reconfigure (Top t) (Bottom b)
