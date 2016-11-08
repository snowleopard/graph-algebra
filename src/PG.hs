{-# LANGUAGE DeriveFunctor, FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module PG (
    Boolean (..), PG (..), (?), renormalise, evaluate, decode, Code (..),
    InstructionSet, Predicate, readBit, match
    ) where

import Prelude hiding ((&&), (||), not)
import Data.Bits
import qualified Data.Bool
import Data.List.Extra
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass hiding (empty)

import Basic
import Graph
import Relation

class Boolean a where
    {-# MINIMAL (true | false), not, ((&&) | (||)) #-}
    true  :: a
    false :: a
    not   :: a -> a
    (&&)  :: a -> a -> a
    (||)  :: a -> a -> a
    true   = not false
    false  = not true
    x && y = not (not x || not y)
    x || y = not (not x && not y)

infixr 3 &&
infixr 2 ||

instance Boolean Bool where
    true  = Data.Bool.True
    false = Data.Bool.False
    not   = Data.Bool.not
    (&&)  = (Data.Bool.&&)
    (||)  = (Data.Bool.||)

newtype PG a b = PG { graph :: Basic (a, b) }
    deriving (Arbitrary, Functor, Monoid, Pretty, Show)

instance (Num a, Boolean b) => Num (PG a b) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Boolean b => Graph (PG a b) where
    type Vertex (PG a b) = a
    empty       = PG empty
    vertex  x   = PG $ vertex (x, true)
    overlay x y = PG $ overlay (graph x) (graph y)
    connect x y = PG $ connect (graph x) (graph y)

(?) :: Boolean b => b -> PG a b -> PG a b
p ? x = fmap (p &&) x

infixr 8 ?

collapse :: (Ord a, Boolean b, Eq b) => [(a, b)] -> [(a, b)]
collapse = filter ((/= false) . snd) . map (fmap $ foldr (||) false) . groupSort

renormalise :: (Ord a, Boolean b, Eq b) => Relation (a, b)
                                        -> ([(a, b)], [((a, a), b)])
renormalise (Relation d r) = (collapse $ d ++ concat vs, collapse as)
  where
    vas      = [ ([(a, x), (b, y)], ((a, b), x && y)) | ((a, x), (b, y)) <- r ]
    (vs, as) = unzip vas

-- TODO: Ord b is not really necessary, but allows to reuse Relation
instance (Ord a, Ord b, Boolean b) => Eq (PG a b) where
    x == y = xvs == yvs && xas == yas
      where
        (xvs, xas) = renormalise . toRelation $ graph x
        (yvs, yas) = renormalise . toRelation $ graph y

evaluate :: (Ord (Vertex g), Graph g) => (b -> Bool) -> PG (Vertex g) b -> g
evaluate f x = fromRelation $ Relation (map fst vs) (map fst as)
  where
    (vs, as) = renormalise . toRelation . graph $ fmap f x

newtype Code = Code Int deriving (Bits, Eq)

newtype DNF = DNF [[Int]] deriving (Eq, Ord, Show)

instance Pretty DNF where pPrint = text . show

type InstructionSet a = PG a DNF
type Predicate = DNF

instance Boolean DNF where
    true             = DNF [[]]
    false            = DNF []
    not (DNF xs)     = DNF . sequence . reverse $ map (reverse . map negate) xs
    DNF xs && DNF ys = DNF [ x +++ y | x <- xs, y <- ys, compatible x y ]
    DNF xs || DNF ys = DNF $ xs +++ ys

compatible :: [Int] -> [Int] -> Bool
compatible xs ys = disjoint xs $ map negate ys

readBit :: Int -> (DNF, DNF)
readBit b = (DNF [[b + 1]], DNF [[-b - 1]])

match :: Code -> DNF -> Bool
match code (DNF xs) = any termSatisfied xs
  where
    termSatisfied = all clauseSatisfied
    clauseSatisfied k
        | k < 0 = not $ testBit code (-k - 1)
        | k > 0 =       testBit code ( k - 1)
        | otherwise = error $ "PG.match: internal invariant violated"

decode :: Ord a => Code -> InstructionSet a -> Basic a
decode code = evaluate (match code)
