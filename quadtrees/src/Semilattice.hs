module Semilattice where

import Data.Monoid
import Data.Set (Set)

class Monoid a => Semilattice a where
    (/\) :: a -> a -> a
    (/\) = (<>)

instance Semilattice Any
instance Semilattice All
instance Ord a => Semilattice (Set a)

