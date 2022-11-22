{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module NaiveSpec where

import AlgebraCheckers
import Naive
import Test.QuickCheck
import Test.QuickCheck.Checkers
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

instance EqProp a => EqProp (Quad a) where

instance EqProp a => EqProp (QuadTree a) where
  Leaf a =-= Leaf a' = a =-= a'
  Leaf a =-= Tree qu = pure a =-= Tree qu
  Tree qu =-= Leaf a = pure a =-= Tree qu
  Tree qu =-= Tree qu' = qu =-= qu'

mk :: QuadTree [Int] -> QuadTree [Int]
mk = id

buildRegion :: V2 Int -> Region
buildRegion (V2 x y) = Quad x y 1 1


extend :: Region -> Region
extend (Quad n i j x) = Quad (n - 10) (i - 10) (j + 20) (x + 20)

$(pure [])

props :: [Property]
props = $(theoremsOf [e| do

  homo @Semigroup $ \v -> fill2 v r a (mk q)

  -- law "hello" $ getLocation2 xy (extend (buildRegion xy)) (fmap snd (fill v (buildRegion xy) (regionify (extend (buildRegion xy)) (mk q)))) == Just v

  |])


main :: IO ()
main = traverse_ quickCheck props
