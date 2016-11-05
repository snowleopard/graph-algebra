import Data.Foldable
import Test.QuickCheck

import Basic
import Graph
import PartialOrder

type DirectedGraph = Basic Int
-- Overlay properties:
overlayIdentity :: DirectedGraph -> Bool
overlayIdentity x = x + empty == x

overlayCommutativity :: DirectedGraph -> DirectedGraph -> Bool
overlayCommutativity x y = x + y == y + x

overlayAssociativity :: DirectedGraph -> DirectedGraph -> DirectedGraph -> Bool
overlayAssociativity x y z = x + (y + z) == (x + y) + z

overlayIdempotence :: DirectedGraph -> Bool
overlayIdempotence x = x + x == x

-- Connect properties:
connectIdentity :: DirectedGraph -> Bool
connectIdentity x = x * empty == x && empty * x == x

connectAssociativity :: DirectedGraph -> DirectedGraph -> DirectedGraph -> Bool
connectAssociativity x y z = x * (y * z) == (x * y) * z

-- Other properties:
distributivity :: DirectedGraph -> DirectedGraph -> DirectedGraph -> Bool
distributivity x y z = x * (y + z) == x * y + x * z && (x + y) * z == x * z + y * z

decomposition :: DirectedGraph -> DirectedGraph -> DirectedGraph -> Bool
decomposition x y z = x * y * z == x * y + x * z + y * z

absorption :: DirectedGraph -> DirectedGraph -> Bool
absorption x y = x + x * y == x * y && y + x * y == x * y

-- Test the partial order on graphs:
partialOrder :: DirectedGraph -> DirectedGraph -> Bool
partialOrder x y
    | x -<- y && x ->- y = x == y
    | x -<- y            = x + y == y
    | x ->- y            = x + y == x
    | x -|- y            = x /= y
    | otherwise          = False

lowerBound :: DirectedGraph -> Bool
lowerBound x = empty -<- x

upperBound :: DirectedGraph -> Bool
upperBound x = x -<- vertices (toList x) * vertices (toList x)

overlayConnectOrder :: DirectedGraph -> DirectedGraph -> Bool
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
