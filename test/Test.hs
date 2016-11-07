{-# LANGUAGE ScopedTypeVariables #-}
import Data.Foldable

import Basic hiding (test)
import Demo
import Graph
import PartialOrder

type G = Basic Int

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

