{-# LANGUAGE ScopedTypeVariables #-}
import Data.Foldable
import Prelude hiding ((&&), (||), not)

import Basic
import Demo
import Graph
import PartialOrder
import PG

type G = Basic Int
type P = PG Int Bool

main :: IO ()
main = do
    putStrLn "============ Testing library ============"

    test "Overlay identity" $ \(x :: G) ->
        x + empty == x

    test "Overlay commutativity" $ \(x :: G) y ->
        x + y == y + x

    test "Overlay associativity" $ \(x :: G) y z ->
        x + (y + z) == (x + y) + z

    test "Overlay idempotence" $ \(x :: G) ->
        x + x == x

    test "Connect identity" $ \(x :: G) ->
        x * empty == x && empty * x == x

    test "Overlay associativity" $ \(x :: G) y z ->
        x * (y * z) == (x * y) * z

    test "Distributivity" $ \(x :: G) y z ->
        x * (y + z) == x * y + x * z && (x + y) * z == x * z + y * z

    test "Decomposition" $ \(x :: G) y z ->
        x * y * z == x * y + x * z + y * z

    test "Absorption" $ \(x :: G) y ->
        x + x * y == x * y && y + x * y == x * y

    test "True and false condition" $ \(x :: P) ->
        True ? x == x && False ? x == empty

    test "Conditional empty" $ \b ->
        b ? empty == (empty :: P)

    test "Conditional overlay" $ \(x :: P) y b ->
        b ? (x + y) == b ? x + b ? y

    test "Conditional connect" $ \(x :: P) y b ->
        b ? (x * y) == b ? x * b ? y

    test "AND condition" $ \(x :: P) b1 b2 ->
        (b1 && b2) ? x == b1 ? b2 ? x

    test "OR condition" $ \(x :: P) b1 b2 ->
        (b1 || b2) ? x == b1 ? x + b2 ? x

    test "Choice propagation" $ \(x :: P) y z b ->
        b ? (x * y) + not b ? (x * z) == x * (b ? y + not b ? z) &&
        b ? (x * z) + not b ? (y * z) == (b ? x + not b ? y) * z

    test "Condition regularisation" $ \(x :: P) y b1 b2 ->
        b1 ? x * b2 ? y == b1 ? x + b2 ? y + (b1 && b2) ? (x * y)

    test "Lower bound" $ \(x :: G) ->
        empty -<- x

    test "Upper bound" $ \(x :: G) ->
        x -<- vertices (toList x) * vertices (toList x)

    test "Overlay-connect order" $ \(x :: G) y ->
        x + y -<- x * y

    test "Partial order" partialOrder
  where
    partialOrder :: G -> G -> Bool
    partialOrder x y
        | x -<- y && x ->- y = x == y
        | x -<- y            = x + y == y
        | x ->- y            = x + y == x
        | x -|- y            = x /= y
        | otherwise          = False

