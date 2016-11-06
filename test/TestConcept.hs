import Data.Char
import Text.PrettyPrint.HughesPJClass hiding (empty, (<>))

import Concept
import Demo

data Signal = A | B | C deriving (Eq, Ord, Show)

instance Pretty Signal where pPrint = text . map toLower . show

oscillator :: a -> a -> a -> Concept a
oscillator a b c = cElement a b c <> inverter c a <> inverter c b

handshakeDefinition :: Int -> Int -> Bool
handshakeDefinition a b = handshake a b == mconcat [ rise a ~> rise b
                                                   , rise b ~> fall a
                                                   , fall a ~> fall b
                                                   , fall b ~> rise a ]

oscillatorHandshakes :: Int -> Int -> Int -> Bool
oscillatorHandshakes a b c = oscillator a b c == handshake a c <> handshake b c

main :: IO ()
main = do
    demo "buffer"     $ buffer     A B
    demo "inverter"   $ inverter   A B
    demo "handshake"  $ handshake  A B
    demo "cElement"   $ cElement   A B C
    demo "oscillator" $ oscillator A B C

    putStrLn "Testing:"

    test "Handshake definition"  handshakeDefinition
    test "Oscillator handshakes" oscillatorHandshakes
