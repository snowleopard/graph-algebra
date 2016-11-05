{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PG (
    module Data.Bits,
    PG, Predicate, empty, vertex, overlay, connect, decode, (?), test, readBit,
    (~>), clique, vertices, normalise, fromRelation, pretty, simplify
    ) where

import Control.Monad.Reader
import Data.Bits

import Graph hiding (empty, vertex, overlay, connect, (~>))
import qualified Graph

type PG a b      = Reader b (Graph a)
type Predicate b = Reader b Bool

decode :: PG a b -> b -> Graph a
decode = runReader

empty :: PG a b
empty = return Graph.empty

vertex :: a -> PG a b
vertex = return . Graph.vertex

overlay :: PG a b -> PG a b -> PG a b
overlay = liftM2 Graph.overlay

connect :: PG a b -> PG a b -> PG a b
connect = liftM2 Graph.connect

(~>) :: PG a b -> PG a b -> PG a b
(~>) = connect

(?) :: Predicate b -> PG a b -> PG a b
p ? x = do { bool <- p; if bool then x else mempty }

infixr 5 ?

test :: (b -> Bool) -> Predicate b
test = asks

readBit :: Bits a => Int -> Predicate a
readBit = test . flip testBit
