{-# LANGUAGE FlexibleInstances #-}
module PartialOrder (PartialOrder (..)) where

import qualified Data.Set  as Set
import qualified Data.List as List

class PartialOrder a where
    {-# MINIMAL (-<-) | (->-) #-}
    (-<-) :: a -> a -> Bool
    x -<- y = y ->- x
    (->-) :: a -> a -> Bool
    x ->- y = y -<- x
    (-|-) :: a -> a -> Bool
    x -|- y = not (x -<- y) && not (x ->- y)

infix 4 -<-
infix 4 ->-
infix 4 -|-

instance Ord a => PartialOrder (Set.Set a) where
    (-<-) = Set.isSubsetOf

instance Ord a => PartialOrder [a] where
    (-<-) = List.isSubsequenceOf
