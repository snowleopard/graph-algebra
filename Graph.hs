{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
module Graph (
    Graph, empty, vertex, overlay, connect, clique, vertices, normalise
    ) where

import Data.Monoid
import Test.QuickCheck
import qualified Data.Set as Set

import PartialOrder
import Relation (Relation (..))
import qualified Relation as Relation

data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving (Show, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Graph a) where
    arbitrary = sized graph
      where
        graph 0 = return Empty
        graph 1 = Vertex <$> arbitrary
        graph n = do
            left <- choose (0, n)
            oneof [ Overlay <$> (graph left) <*> (graph $ n - left)
                  , Connect <$> (graph left) <*> (graph $ n - left) ]

    shrink Empty         = []
    shrink (Vertex    _) = [Empty]
    shrink (Overlay x y) = [Empty, x, y]
                        ++ [Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (Connect x y) = [Empty, x, y, Overlay x y]
                        ++ [Connect x' y' | (x', y') <- shrink (x, y) ]

instance Monoid (Graph a) where
    mempty  = Empty
    mappend = Overlay

instance Num a => Num (Graph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

empty :: Graph a
empty = Empty

vertex :: a -> Graph a
vertex = Vertex

overlay :: Graph a -> Graph a -> Graph a
overlay = Overlay

connect :: Graph a -> Graph a -> Graph a
connect = Connect

vertices :: [a] -> Graph a
vertices = foldr Overlay Empty . map Vertex

clique :: [a] -> Graph a
clique = foldr Connect Empty . map Vertex

foldGraph :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldGraph e v o c = go
  where
    go Empty         = e
    go (Vertex  x  ) = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)

normalise :: Ord a => Graph a -> Relation a
normalise = foldGraph Relation.empty Relation.singleton Relation.union cross
  where
    cross x y = Relation.unions [x, y, Relation.complete (domain x) (domain y)]

instance Ord a => Eq (Graph a) where
    x == y = normalise x == normalise y

instance Ord a => PartialOrder (Graph a) where
    x -<- y = normalise x -<- normalise y
