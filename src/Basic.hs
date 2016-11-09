{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}
{-# LANGUAGE FlexibleInstances, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Basic (
    Basic (..), simplify, flatten, pPrintBasic, BasicFormat (..), basicFormat,
    Undirected (..), undirectedRelation, simplifyU
    ) where

import Data.List
import Data.List.Extra
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass hiding (empty)

import Graph
import PartialOrder
import Relation

data Basic a = Empty
             | Vertex a
             | Overlay (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)
             deriving (Show, Functor, Foldable, Traversable)

instance Graph Basic where
    empty   = Empty
    vertex  = Vertex
    overlay = Overlay
    connect = Connect

    fold e v o c = go
      where
        go Empty         = e
        go (Vertex  x  ) = v x
        go (Overlay x y) = o (go x) (go y)
        go (Connect x y) = c (go x) (go y)

instance Arbitrary a => Arbitrary (Basic a) where
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

instance Monoid (Basic a) where
    mempty  = Empty
    mappend = Overlay

instance Num a => Num (Basic a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

instance Ord a => Eq (Basic a) where
    x == y = toRelation x == toRelation y

instance Ord a => PartialOrder (Basic a) where
    x -<- y = toRelation x -<- toRelation y

data BasicFormat = BasicFormat
    { formatEmpty   :: Doc
    , formatVertex  :: Doc -> Doc
    , formatOverlay :: Bool -> Doc -> Doc -> Doc
    , formatConnect :: Doc -> Doc -> Doc }

basicFormat :: BasicFormat
basicFormat = BasicFormat
    { formatEmpty   = text "()"
    , formatVertex  = id
    , formatOverlay = \p x y -> maybeParens p $ hsep [x, text "+" , y]
    , formatConnect = \  x y ->                 hsep [x, text "->", y] }

instance Pretty a => Pretty (Basic a) where
    pPrint g = pPrintBasic g basicFormat

pPrintBasic :: Pretty a => Basic a -> BasicFormat -> Doc
pPrintBasic g BasicFormat {..} = go False g
  where
    go p = \case
        Empty       -> formatEmpty
        Vertex  x   -> formatVertex $ pPrint x
        Overlay x y -> formatOverlay p (go False x) (go False y)
        Connect x y -> formatConnect   (go True  x) (go True  y)


simplifyU :: Ord a => Undirected a -> Undirected a
simplifyU (Undirected g) = Undirected $ simplify g

simplify :: Ord a => Basic a -> Basic a
simplify (Overlay x y)
    | x' -<- y' = y'
    | x' ->- y' = x'
    | otherwise = Overlay x' y'
  where
    x' = simplify x
    y' = simplify y
simplify (Connect x y)
    | x' == Empty = y'
    | y' == Empty = x'
    | otherwise = Connect x' y'
  where
    x' = simplify x
    y' = simplify y
simplify x = x

flatten :: Ord a => Basic a -> Basic a
flatten g = simplify $ vertices (d \\ (map fst r ++ map snd r)) `overlay`
    overlays [ vertex x `connect` vertex y | (x, y) <- r ]
  where
    Relation d r = toRelation g

newtype Undirected a = Undirected (Basic a)
    deriving (Functor, Graph, Monoid, Num, Arbitrary, Show)

instance Ord a => Eq (Undirected a) where
    x == y = undirectedRelation x == undirectedRelation y

instance Ord a => PartialOrder (Undirected a) where
    x -<- y = undirectedRelation x -<- undirectedRelation y

undirectedRelation :: Ord a => Undirected a -> Relation a
undirectedRelation x = Relation d (nubOrd . sort $ map sortPair r)
  where
    Relation d r    = toRelation x
    sortPair (a, b) = if a <= b then (a, b) else (b, a)

instance Pretty a => Pretty (Undirected a) where
    pPrint (Undirected g) = pPrintBasic g $ basicFormat
        { formatConnect = \x y -> hsep [x, text "--", y] }
