{-# LANGUAGE TypeFamilies #-}
module Relation (Relation (..), fromRelation) where

import PartialOrder
import Graph

data Relation a = Relation { domain :: [a], relation :: [(a, a)] }
    deriving (Eq, Show)

instance Ord a => PartialOrder (Relation a) where
    x -<- y = domain x -<- domain y && relation x -<- relation y

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = Relation [] []
    vertex  x   = Relation [x] []
    overlay x y = Relation (domain x +++ domain y) (relation x +++ relation y)
    connect x y = Relation (domain x +++ domain y) $
        relation x +++ relation y +++ [ (u, v) | u <- domain x, v <- domain y ]

(+++) :: Ord a => [a] -> [a] -> [a]
[]     +++ xs     = xs
xs     +++ []     = xs
(x:xs) +++ (y:ys) = case compare x y of
    LT -> x : (    xs +++ y : ys)
    GT -> y : (x : xs +++     ys)
    EQ -> x : (    xs +++     ys)

infixl 4 +++

fromRelation :: Graph g => Relation (Vertex g) -> g
fromRelation r = vertices (domain r) `overlay`
    overlays [ vertex x `connect` vertex y | (x, y) <- relation r ]
