import Data.Monoid
import Text.PrettyPrint.HughesPJClass (text, Pretty (..))

import Basic
import Demo
import Graph
import PG

data Unit = IncrPC | LoadIR | ALU deriving (Eq, Ord, Show)

instance Pretty Unit where pPrint = text . show

type Expression = PG Unit Predicate

incrPC :: Expression
incrPC = vertex IncrPC

loadIR :: Expression
loadIR = vertex LoadIR

alu :: Expression
alu = vertex ALU

(~>) :: Boolean b => PG a b -> PG a b -> PG a b
(~>) = connect

fetch :: Expression
fetch = incrPC ~> loadIR

nop :: Expression
nop = fetch

jmp :: Expression
jmp = alu ~> loadIR

add :: Expression
add = alu <> fetch

x, x' :: Predicate
(x, x') = readBit 0

y, y' :: Predicate
(y, y') = readBit 1

processor :: Expression
processor = mconcat [ x  ? y' ? nop
                    , x' ? y  ? jmp
                    , x  ? y  ? add ]

main :: IO ()
main = do
    putStrLn "============ Testing processor instructions ============"
    demo "processor(0)" . flatten $ decode (Code 0) processor
    demo "processor(1)" . flatten $ decode (Code 1) processor
    demo "processor(2)" . flatten $ decode (Code 2) processor
    demo "processor(3)" . flatten $ decode (Code 3) processor
