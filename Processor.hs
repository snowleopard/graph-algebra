{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid
import PG

data Unit = IncrPC | LoadIR | ALU deriving (Eq, Ord, Show)

newtype Opcode = Opcode Int deriving (Bits, Eq, Num, Show)

type Expression = PG Unit Opcode
type Condition  = Predicate Opcode

incrPC :: Expression
incrPC = vertex IncrPC

loadIR :: Expression
loadIR = vertex LoadIR

alu :: Expression
alu = vertex ALU

fetch :: Expression
fetch = incrPC ~> loadIR

nop :: Expression
nop = fetch

jmp :: Expression
jmp = alu ~> loadIR

add :: Expression
add = alu <> fetch

x, x' :: Condition
x  = readBit 0
x' = not <$> x

y, y' :: Condition
y  = readBit 1
y' = not <$> y

processor :: Expression
processor = mconcat [ x  ? y' ? nop
                    , x' ? y  ? jmp
                    , x  ? y  ? add ]

main :: IO ()
main = do
    demo $ decode processor 0
    demo $ decode processor 1
    demo $ decode processor 2
    demo $ decode processor 3
  where
    demo = putStrLn . pretty . simplify
