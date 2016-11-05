module Relation (
    Relation (..), empty, singleton, union, unions, complete
    ) where

import PartialOrder

data Relation a = Relation { domain :: [a], relation :: [(a, a)] }
    deriving (Show, Eq)

instance Ord a => PartialOrder (Relation a) where
    x -<- y = domain x -<- domain y && relation x -<- relation y

empty :: Relation a
empty = Relation [] []

singleton :: a -> Relation a
singleton x = Relation [x] []

union :: Ord a => Relation a -> Relation a -> Relation a
union p q = Relation (domain p +++ domain q) (relation p +++ relation q)

unions :: Ord a => [Relation a] -> Relation a
unions rs = Relation (foldr (+++) [] $ map domain   rs)
                     (foldr (+++) [] $ map relation rs)

complete :: Ord a => [a] -> [a] -> Relation a
complete p q = Relation (p +++ q) [ (x, y) | x <- p, y <- q ]

(+++) :: Ord a => [a] -> [a] -> [a]
[]     +++ xs     = xs
xs     +++ []     = xs
(x:xs) +++ (y:ys) = case compare x y of
    LT -> x : (    xs +++ y : ys)
    GT -> y : (x : xs +++     ys)
    EQ -> x : (    xs +++     ys)

infixl 4 +++
