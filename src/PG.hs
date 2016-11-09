{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs #-}
{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module PG (
    Boolean (..), Tagged (..), tag, PG, (?), evaluate, decode, groupTags,
    Code (..), Predicate, SN, readBit, match
    ) where

import Prelude hiding ((&&), (||), not)
import Data.Bits
import Data.Bifunctor
import qualified Data.Bool
import Data.List.Extra
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass hiding (empty, (<>), first)

import Basic
import Graph
import Relation

class Semiring a where
    zero     :: a
    one      :: a
    add      :: a -> a -> a
    multiply :: a -> a -> a
    isZero   :: a -> Bool

-- TODO: redefining essentially the same operators is weird
class Semiring a => Boolean a where
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

instance Semiring Bool where
    zero     = false
    one      = true
    add      = (||)
    multiply = (&&)
    isZero   = not

instance Boolean Bool where
    true  = Data.Bool.True
    false = Data.Bool.False
    not   = Data.Bool.not
    (&&)  = (Data.Bool.&&)
    (||)  = (Data.Bool.||)

newtype Tagged g t a = Tagged (g (t, a)) deriving Functor

deriving instance (Arbitrary a, Arbitrary t) => Arbitrary (Tagged Basic t a)
deriving instance (Arbitrary a, Arbitrary t) => Arbitrary (Tagged Undirected t a)
deriving instance Monoid (Tagged Basic t a)
deriving instance Monoid (Tagged Undirected t a)
deriving instance (Pretty a, Pretty t) => Pretty (Tagged Basic t a)
deriving instance (Pretty a, Pretty t) => Pretty (Tagged Undirected t a)
deriving instance (Show a, Show t) => Show (Tagged Basic t a)
deriving instance (Show a, Show t) => Show (Tagged Undirected t a)

tag :: (Functor g, Semiring t) => t -> Tagged g t a -> Tagged g t a
tag newTag (Tagged graph) = Tagged $ fmap (first $ multiply newTag) graph

instance (Graph g, Num a, Semiring t) => Num (Tagged g t a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance (Graph g, Semiring t) => Graph (Tagged g t) where
    empty                         = Tagged empty
    vertex  x                     = Tagged $ vertex (one, x)
    overlay (Tagged x) (Tagged y) = Tagged $ overlay x y
    connect (Tagged x) (Tagged y) = Tagged $ connect x y

    fold e v o c (Tagged graph) = fold e (v . snd) o c graph

collapse :: (Ord a, Semiring t) => [(t, a)] -> [(t, a)]
collapse = filter (not . isZero . fst) . map collapseOne . groupSortOn snd
  where
    collapseOne = bimap (foldr1 add) head . unzip

class (Functor g, Graph g) => GroupTags g t a where
    groupTags :: Tagged g t a -> ([(t, a)], [(t, (a, a))])

instance (Semiring t, Ord a, Ord t) => GroupTags Basic t a where
    groupTags (Tagged g) = (collapse $ d ++ concat vvs, collapse as)
      where
        Relation d r         = toRelation g
        (tx, x) -<>- (ty, y) = (tx `multiply` ty, (x, y))
        (vvs, as)            = unzip [ ([x, y], x -<>- y) | (x, y) <- r ]

instance (Semiring t, Ord a, Ord t) => GroupTags Undirected t a where
    groupTags (Tagged g) = (collapse $ d ++ concat vvs, collapse as)
      where
        Relation d r         = undirectedRelation g
        (tx, x) -<>- (ty, y) = (tx `multiply` ty, (x, y))
        (vvs, as)            = unzip [ ([x, y], x -<>- y) | (x, y) <- r ]

instance (GroupTags g t a, Ord a, Ord t) => Eq (Tagged g t a) where
    x == y = xvs == yvs && xas == yas
      where
        (xvs, xas) = groupTags x
        (yvs, yas) = groupTags y

evaluate :: (GroupTags g Bool a) => (t -> Bool) -> Tagged g t a -> g a
evaluate f (Tagged graph) = fromRelation $ Relation (map snd vs) (map snd as)
  where
    (vs, as) = groupTags . Tagged $ fmap (first f) graph

newtype Code = Code Int deriving (Bits, Eq)

newtype DNF = DNF [[Int]] deriving (Eq, Ord, Show)

instance Pretty DNF where pPrint = text . show

-- TODO: Switch to a proper Predicate implementation
type Predicate = DNF
type PG a b    = Tagged Basic b a
type SN a b    = Tagged Undirected b a

(?) :: (Boolean b, Functor g) => b -> Tagged g b a -> Tagged g b a
p ? x = tag p x

infixr 8 ?

instance Boolean DNF where
    true             = DNF [[]]
    false            = DNF []
    not (DNF xs)     = DNF . sequence . reverse $ map (reverse . map negate) xs
    DNF xs && DNF ys = DNF [ x +++ y | x <- xs, y <- ys, compatible x y ]
    DNF xs || DNF ys = DNF . filter (not . null) $ xs +++ ys

instance Semiring DNF where
    zero            = false
    one             = true
    add             = (||)
    multiply        = (&&)
    isZero (DNF xs) = null xs

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
        | otherwise = error $ "Tagged.match: internal invariant violated"

decode :: GroupTags g Bool a => Code -> Tagged g Predicate a -> g a
decode code = evaluate (match code)
