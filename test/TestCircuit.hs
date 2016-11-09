{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (reverse)
import Data.Monoid
import Text.PrettyPrint.HughesPJClass (text, Pretty (..))

import Basic
import Demo
import Graph
import PG

data Wire = EnvA | EnvB | CapA | CapB deriving (Eq, Ord, Show)

instance Pretty Wire where pPrint = text . show

type Circuit = SN Wire Predicate

envA, envB, capA, capB:: Circuit
[envA, envB, capA, capB] = map vertex [EnvA, EnvB, CapA, CapB]

(~~) :: Circuit -> Circuit -> Circuit
(~~) = connect

forward :: Circuit
forward = envA ~~ capA <> capB ~~ envB

reverse :: Circuit
reverse = envA ~~ capB <> capA ~~ envB

x, x' :: Predicate
(x, x') = readBit 0

twoway :: Circuit
twoway = x' ? forward <> x ? reverse

switch :: Circuit -> Circuit -> Predicate -> Circuit
switch a b c = a <> b <> c ? (a ~~ b)

hBridge :: Circuit
hBridge = mconcat [ envA `switch` capB $ x
                  , capA `switch` envB $ x
                  , envA `switch` capA $ x'
                  , capB `switch` envB $ x' ]

main :: IO ()
main = do
    putStrLn "============ Testing switching networks ============"
    demo "Forward" . simplifyU $ decode (Code 0) twoway
    demo "Reverse" . simplifyU $ decode (Code 1) twoway
    test "H-bridge definition" $ twoway == hBridge
