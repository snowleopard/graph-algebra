{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Concept (
    Transition (..), Concept, rise, fall, (~>), (<>),
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

newtype Tweaked a = Tweaked (Basic a)
    deriving (Eq, Functor, Graph, Monoid, PartialOrder)

instance Pretty a => Pretty (Tweaked a) where
    pPrint (Tweaked g) = pPrintBasic g $ basicFormat
        { formatOverlay = \_ x y -> x $+$ y
        , formatConnect = \  x y -> hsep [x, text "->", y] }

type Concept a = Tweaked (Transition a)

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
