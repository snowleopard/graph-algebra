{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (reverse)
import Data.Monoid
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass (text, Pretty (..), prettyShow)

import PG

data Wire = EnvA | EnvB | CapA | CapB deriving (Eq, Ord, Show)

instance Pretty Wire where pPrint = text . show

newtype State = State Int deriving (Bits, Eq, Num)

type Expression = PG Wire State
type Condition  = Predicate State

envA, envB, capA, capB :: Expression
[envA, envB, capA, capB] = map vertex [EnvA, EnvB, CapA, CapB]

forward :: Expression
forward = envA ~> capA <> capB ~> envB

reverse :: Expression
reverse = envA ~> capB <> capA ~> envB

x, x' :: Condition
x  = readBit 0
x' = not <$> x

twoway :: Expression
twoway = x' ? forward <> x ? reverse

switch :: Expression -> Expression -> Condition -> Expression
switch a b c = c ? (a ~> b)

hBridge :: Expression
hBridge = mconcat [ envA, envB, capA, capB
                  , envA `switch` capB $ x
                  , capA `switch` envB $ x
                  , envA `switch` capA $ x'
                  , capB `switch` envB $ x' ]

hBridgeDefinition :: Small Int -> Bool
hBridgeDefinition (Small i) = decode twoway (State i) == decode hBridge (State i)

main :: IO ()
main = do
    demo $ decode twoway 0
    demo $ decode twoway 1
    demo $ decode hBridge 0
    demo $ decode hBridge 1
    quickCheck hBridgeDefinition
  where
    demo = putStrLn . prettyShow . simplify
