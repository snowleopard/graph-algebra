module Graph (Graph (..), vertices, clique, transpose, overlays, connects) where

class Graph g where
    empty   :: g a
    vertex  :: a -> g a
    overlay :: g a -> g a -> g a
    connect :: g a -> g a -> g a

    fold    :: r -> (a -> r) -> (r -> r -> r) -> (r -> r -> r) -> g a -> r

vertices :: Graph g => [a] -> g a
vertices = overlays . map vertex

clique :: Graph g => [a] -> g a
clique = connects . map vertex

transpose :: Graph g => g a -> g a
transpose = fold empty vertex overlay (flip connect)

-- 'foldr f empty' adds a redundant empty to the result; foldg avoids this
foldg :: Graph g => (g a -> g a -> g a) -> [g a] -> g a
foldg _ [] = empty
foldg f gs = foldr1 f gs

overlays :: Graph g => [g a] -> g a
overlays = foldg overlay

connects :: Graph g => [g a] -> g a
connects = foldg connect
