{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass (hcat, text, Pretty (..), prettyShow)

import Basic
import Graph

data Transition a = Transition a Bool deriving (Eq, Ord)

instance Pretty a => Pretty (Transition a) where
    pPrint (Transition s v) = hcat [pPrint s, text $ if v then "+" else "-"]

type Concept a = Basic (Transition a)

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
    demo $ buffer     A B
    demo $ inverter   A B
    demo $ handshake  A B
    demo $ cElement   A B C
    demo $ oscillator A B C
    quickCheck handshakeDefinition
    quickCheck oscillatorHandshakes
  where
    demo = putStrLn . prettyShow . simplify
