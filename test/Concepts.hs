{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

import Data.Monoid
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass hiding (empty, (<>))

import Basic
import Graph
import Demo
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

oscillator :: a -> a -> a -> Concept a
oscillator a b c = cElement a b c <> inverter c a <> inverter c b

data Signal = A | B | C deriving (Eq, Ord, Show)

instance Pretty Signal where pPrint = text . show

handshakeDefinition :: Int -> Int -> Bool
handshakeDefinition a b = handshake a b == mconcat [ rise a ~> rise b
                                                   , rise b ~> fall a
                                                   , fall a ~> fall b
                                                   , fall b ~> rise a ]

oscillatorHandshakes :: Int -> Int -> Int -> Bool
oscillatorHandshakes a b c = oscillator a b c == handshake a c <> handshake b c

main :: IO ()
main = do
    demo [ ("buffer"    , buffer     A B  )
         , ("inverter"  , inverter   A B  )
         , ("handshake" , handshake  A B  )
         , ("cElement"  , cElement   A B C)
         , ("oscillator", oscillator A B C)
         , ("test"      , rise A ~> (rise C ~> handshake A B <> buffer B C)) ]
    quickCheck handshakeDefinition
    quickCheck oscillatorHandshakes
  where

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Concept a) where
    type Vertex (Concept a) = Transition a
    empty       = Concept empty
    vertex      = Concept . vertex
    overlay x y = Concept $ overlay (graph x) (graph y)
    connect x y = Concept $ connect (graph x) (graph y)
