{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Concept (
    Transition (..), Concept (..), rise, fall, (~>), (<>),
    buffer, inverter, handshake, cElement
    ) where

import Data.Monoid
import Text.PrettyPrint.HughesPJClass hiding (empty, (<>))

import Basic
import Graph
import PartialOrder

data Transition a = Transition a Bool deriving (Eq, Functor, Ord)

instance Pretty a => Pretty (Transition a) where
    pPrint (Transition s v) = hcat [pPrint s, text $ if v then "+" else "-"]

newtype Concept a = Concept { graph :: Basic (Transition a) }
    deriving (Eq, Functor, Monoid, PartialOrder)

instance Pretty a => Pretty (Concept a) where
    pPrint (Concept g) = pPrintBasic g $ basicFormat
        { formatOverlay = \_ x y -> x $+$ y
        , formatConnect = \  x y -> hsep [x, text "->", y] }

rise :: a -> Concept a
rise s = vertex $ Transition s True

fall :: a -> Concept a
fall s = vertex $ Transition s False

(~>) :: Concept a -> Concept a -> Concept a
(~>) = connect

buffer :: a -> a -> Concept a
buffer a b = rise a ~> rise b <> fall a ~> fall b

inverter :: a -> a -> Concept a
inverter a b = rise a ~> fall b <> fall a ~> rise b

handshake :: a -> a -> Concept a
handshake a b = buffer a b <> inverter b a

cElement :: a -> a -> a -> Concept a
cElement a b c = buffer a c <> buffer b c

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Concept a) where
    type Vertex (Concept a) = Transition a
    empty       = Concept empty
    vertex      = Concept . vertex
    overlay x y = Concept $ overlay (graph x) (graph y)
    connect x y = Concept $ connect (graph x) (graph y)
