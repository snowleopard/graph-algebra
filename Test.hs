import Data.Foldable
import Test.QuickCheck

import Graph
import PartialOrder

-- Overlay properties:
overlayIdentity :: Graph Int -> Bool
overlayIdentity x = x + empty == x

overlayCommutativity :: Graph Int -> Graph Int -> Bool
overlayCommutativity x y = x + y == y + x

overlayAssociativity :: Graph Int -> Graph Int -> Graph Int -> Bool
overlayAssociativity x y z = x + (y + z) == (x + y) + z

overlayIdempotence :: Graph Int -> Bool
overlayIdempotence x = x + x == x

-- Connect properties:
connectIdentity :: Graph Int -> Bool
connectIdentity x = x * empty == x && empty * x == x

connectAssociativity :: Graph Int -> Graph Int -> Graph Int -> Bool
connectAssociativity x y z = x * (y * z) == (x * y) * z

-- Other properties:
distributivity :: Graph Int -> Graph Int -> Graph Int -> Bool
distributivity x y z = x * (y + z) == x * y + x * z && (x + y) * z == x * z + y * z

decomposition :: Graph Int -> Graph Int -> Graph Int -> Bool
decomposition x y z = x * y * z == x * y + x * z + y * z

absorption :: Graph Int -> Graph Int -> Bool
absorption x y = x + x * y == x * y && y + x * y == x * y

-- Test the partial order on graphs:
partialOrder :: Graph Int -> Graph Int -> Bool
partialOrder x y
    | x -<- y && x ->- y = x == y
    | x -<- y            = x + y == y
    | x ->- y            = x + y == x
    | x -|- y            = x /= y
    | otherwise          = False

lowerBound :: Graph Int -> Bool
lowerBound x = empty -<- x

upperBound :: Graph Int -> Bool
upperBound x = x -<- vertices (toList x) * vertices (toList x)

overlayConnectOrder :: Graph Int -> Graph Int -> Bool
overlayConnectOrder x y = x + y -<- x * y

main :: IO ()
main = do
    quickCheck overlayIdentity
    quickCheck overlayCommutativity
    quickCheck overlayAssociativity
    quickCheck overlayIdempotence
    quickCheck connectIdentity
    quickCheck connectAssociativity
    quickCheck distributivity
    quickCheck decomposition
    quickCheck absorption
    quickCheck partialOrder
    quickCheck lowerBound
    quickCheck upperBound
    quickCheck overlayConnectOrder
