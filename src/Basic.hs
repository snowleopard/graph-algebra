{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Basic (
    Basic (..), simplify, PG, Predicate, (?), decode, test, readBit
    ) where

import Control.Monad.Reader
import Data.Bits
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
    pPrint = pPrintBasic basicFormat

pPrintBasic :: Pretty a => BasicFormat -> Basic a -> Doc
pPrintBasic BasicFormat {..} = go False
  where
    go p g = case g of
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

-- Parameterised graphs
type PG a b      = Reader b (Basic a)
type Predicate b = Reader b Bool

instance Graph (PG a b) where
    type Vertex (PG a b) = a
    empty   = return empty
    vertex  = return . vertex
    overlay = liftM2 overlay
    connect = liftM2 connect

instance Monoid (Reader a (Basic b)) where
    mempty  = return mempty
    mappend = liftM2 overlay

decode :: PG a b -> b -> Basic a
decode = runReader

(?) :: Predicate b -> PG a b -> PG a b
p ? x = do { bool <- p; if bool then x else mempty }

infixr 8 ?

test :: (b -> Bool) -> Predicate b
test = asks

readBit :: Bits a => Int -> Predicate a
readBit = test . flip testBit
