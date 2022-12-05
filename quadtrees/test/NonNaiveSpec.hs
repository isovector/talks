{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-} {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module NonNaiveSpec where

import Data.Ratio
import Test.Hspec
import NonNaive hiding (elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Linear.V2
import Test.Hspec.QuickCheck
import Naive (containsPoint, Tree(..))
import Data.Monoid (Sum)
import Data.Semigroup (Any(..))
import Semilattice

instance Show (a -> b) where
  show _ = "<fn>"

instance Arbitrary a => Arbitrary (Quad a) where
  arbitrary = Quad <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance EqProp a => EqProp (QT a) where
  q1 =-= q2 = property $ \a -> flip getLocation q1 a =-= flip getLocation q2 a

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    let terminal = [Fill <$> arbitrary]
     in sized $ \n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ Split <$> Test.QuickCheck.scale (`div` 4) arbitrary
              ] <> terminal
  shrink = genericShrink

instance Arbitrary a => Arbitrary (QT a) where
  arbitrary = QT <$> arbitrary <*> elements [0..10] <*> arbitrary

observe :: QT a -> V2 Rational -> a
observe = flip getLocation

spec :: Spec
spec = modifyMaxSuccess (const 10000) $ do

  prop "empty" $ \(a :: Int) ->
    observe (pure a)
      =-= const a

  prop "fill" $ \r (a :: Int) q ->
    observe (fill r a q)
      =-= \p -> if containsPoint r p then a else observe q p

  prop "mempty" $
    observe (mempty @(QT (Sum Int)))
      =-= mempty

  prop "<>" $ \(q1 :: QT (Sum Int)) q2 ->
    observe (q1 <> q2)
      =-= observe q1 <> observe q2

  prop "fuse" $ \(q :: QT (Sum Int)) ->
    observe (fuse q)
      =-= observe q

  prop "query" $ \(applyFun -> f :: Bool -> Any) q x y w h ->
    let r = Quad x y w h in
      query f r q =-= f (getLocation (midpoint r) q) /\
        foldr1 (/\) ((flip (query f) q) <$> subdivide r)

main = hspec spec
