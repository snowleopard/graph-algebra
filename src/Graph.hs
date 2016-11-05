{-# LANGUAGE TypeFamilies #-}
module Graph (Graph (..), vertices, clique, overlays, connects) where

class Graph g where
    type Vertex g
    empty   :: g
    vertex  :: Vertex g -> g
    overlay :: g -> g -> g
    connect :: g -> g -> g

vertices :: Graph g => [Vertex g] -> g
vertices = overlays . map vertex

clique :: Graph g => [Vertex g] -> g
clique = connects . map vertex

overlays :: Graph g => [g] -> g
overlays = foldr overlay empty

connects :: Graph g => [g] -> g
connects = foldr connect empty
