{-# LANGUAGE FlexibleInstances #-}
module PartialOrder (PartialOrder (..)) where

import qualified Data.Set as Set

class PartialOrder a where
    {-# MINIMAL (-<-) | (->-) #-}
    (-<-) :: a -> a -> Bool
    x -<- y = y ->- x
    (->-) :: a -> a -> Bool
    x ->- y = y -<- x

infix 4 -<-
infix 4 ->-

-- instance Ord a => PartialOrder a where
--     (-<-) = (<=)
--     (->-) = (>=)

instance Ord a => PartialOrder (Set.Set a) where
    (-<-) = Set.isSubsetOf
