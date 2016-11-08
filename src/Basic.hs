{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, RecordWildCards, LambdaCase #-}
module Basic (
    Basic (..), simplify, flatten, toRelation, pPrintBasic, BasicFormat (..),
    basicFormat
    ) where

import Data.List
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

instance Graph (Basic a) where
    type Vertex (Basic a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = Overlay
    connect = Connect

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

fromBasic :: Graph g => Basic (Vertex g) -> g
fromBasic Empty         = empty
fromBasic (Vertex  x  ) = vertex x
fromBasic (Overlay x y) = overlay (fromBasic x) (fromBasic y)
fromBasic (Connect x y) = connect (fromBasic x) (fromBasic y)

toRelation :: Ord a => Basic a -> Relation a
toRelation = fromBasic

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
