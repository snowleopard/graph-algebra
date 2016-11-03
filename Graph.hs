{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import           Relation (Relation (..))
import qualified Relation as Relation

data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving (Show, Functor, Foldable, Traversable)

instance Num a => Num (Graph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

clique :: [a] -> Graph a
clique = foldr Connect Empty . map Vertex

foldGraph :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldGraph empty vertex overlay connect = go
  where
    go Empty         = empty
    go (Vertex  x  ) = vertex x
    go (Overlay x y) = overlay (go x) (go y)
    go (Connect x y) = connect (go x) (go y)

normalise :: Ord a => Graph a -> Relation a
normalise = foldGraph Relation.empty Relation.singleton Relation.union cross
  where
    cross x y = Relation.unions [x, y, Relation.complete (domain x) (domain y)]

instance Ord a => Eq (Graph a) where
    x == y = normalise x == normalise y

instance Ord a => Ord (Graph a) where
    compare x y = compare (normalise x) (normalise y)

ex1 :: Graph Int
ex1 = 1 * (2 + 3)

ex2 :: Graph Int
ex2 = 1 * 2 * 3

main = do
    print ex1
    print ex2
    print $ normalise ex1
    print $ ex1 < ex2
    print $ ex2 == clique [1..3]
