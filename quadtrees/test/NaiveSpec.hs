{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NaiveSpec where

import AlgebraCheckers
import Naive
import Test.QuickCheck
import Linear.V2
import Data.Foldable (traverse_)

instance Arbitrary a => Arbitrary (QuadTree a) where
  arbitrary
    = let terminal = [Leaf <$> arbitrary]
       in sized $ \n ->
            case n <= 1 of
              True -> oneof terminal
              False -> oneof $ [Tree <$> scale (subtract 1) arbitrary] <> terminal

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Quad a) where
  arbitrary = Quad <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

mk :: QuadTree Int -> QuadTree Int
mk = id

buildRegion :: V2 Int -> Region
buildRegion (V2 x y) = Quad x y 1 1


extend :: Region -> Region
extend (Quad n i j x) = Quad (n - 10) (i - 10) (j + 20) (x + 20)

$(pure [])

props :: [Property]
props = $(theoremsOf [e| do

  law "hello" $ getLocation xy (fill v (buildRegion xy) (regionify (extend (buildRegion xy)) (mk q))) == Just v

  |])


main :: IO ()
main = traverse_ quickCheck props
