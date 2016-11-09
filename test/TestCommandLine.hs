import Data.Graph
import Data.List
import Data.Monoid
import Prelude hiding ((&&), (||), not)
import Text.PrettyPrint.HughesPJClass (text, hsep, render)

import Basic
import Demo
import Graph
import PG
import Relation

type Expression = PG String Predicate

arg :: String -> Expression
arg = vertex

(~>) :: Expression -> Expression -> Expression
(~>) = connect

release :: Predicate
(release, _) = readBit 0

gccArgs :: String -> Expression
gccArgs file = arg "gcc" ~> (src <> arg "-o" ~> obj <> release ? arg "-O2")
  where
    src = arg $ file ++ ".c"
    obj = arg $ file ++ ".o"

gccArgs2 :: String -> Expression
gccArgs2 file = arg "gcc" ~> (src <> release ? arg "-O2" <> arg "-o" ~> obj)
  where
    src = arg $ file ++ ".c"
    obj = arg $ file ++ ".o"

commandLine :: Basic String -> String
commandLine cmd = render . hsep . map (text . str . rev) $ topSort graph
  where
    Relation d r  = toRelation cmd
    successors v  = map snd $ filter ((==v) . fst) r
    (graph, rev)  = graphFromEdges' [ (v, v, successors v) | v <- d ]
    str (x, _, _) = x

main :: IO ()
main = do
    putStrLn "============ Testing command lines ============"
    demo "Non-release command line" . commandLine . decode (Code 0) $ gccArgs "foo"
    demo "Release command line"     . commandLine . decode (Code 1) $ gccArgs "bar"
    test "Equivalent expressions" $ gccArgs "baz" == gccArgs2 "baz"
