module Relation (Relation (..), fromRelation, toRelation, (+++)) where

import Graph
import PartialOrder

data Relation a = Relation { domain :: [a], relation :: [(a, a)] }
    deriving (Eq, Show)

instance Ord a => PartialOrder (Relation a) where
    x -<- y = domain x -<- domain y && relation x -<- relation y

fromRelation :: Graph g => Relation a -> g a
fromRelation (Relation d r) =
    vertices d `overlay` overlays [ vertex x `connect` vertex y | (x, y) <- r ]

toRelation :: (Ord a, Graph g) => g a -> Relation a
toRelation = fold (Relation [] []) v o c
  where
    v x   = Relation [x] []
    o x y = Relation (domain x +++ domain y) (relation x +++ relation y)
    c x y = Relation (domain x +++ domain y) $
        relation x +++ relation y +++ [ (a, b) | a <- domain x, b <- domain y ]

(+++) :: Ord a => [a] -> [a] -> [a]
[]     +++ xs     = xs
xs     +++ []     = xs
(x:xs) +++ (y:ys) = case compare x y of
    LT -> x : (    xs +++ y : ys)
    GT -> y : (x : xs +++     ys)
    EQ -> x : (    xs +++     ys)

infixl 4 +++
