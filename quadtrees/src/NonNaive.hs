{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

module NonNaive
  ( QT (..)
  , rect
  , fill
  , elements
  , getLocation
  , subdivide
  , mkRegionByPow
  , query
  , fuse
  , overlay
  , midpoint
  , Quad(..)
  ) where

import GHC.Generics
import qualified Data.Set as S
import Data.Set (Set)
import Naive (Quad(..), Tree(..), unwrap, intersects, contains, getIntersect, containsPoint, normalize, sizeof)
import Semilattice
import qualified Naive as Raw
import Data.Ratio
import Data.Semigroup (mtimesDefault)
import Data.Monoid (Ap(..))
import qualified Data.Monoid as M
import GHC.Base (liftA2)
import Linear.V2
import Data.Foldable
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (Set)
import Data.Coerce (coerce)
import Data.Semigroup (Last(..))

type Region = Quad Rational

midpoint :: (Fractional a) => Quad a -> V2 a
midpoint (Quad x y w h) = V2 (x + w / 2) (y + h / 2)

subdivide :: Fractional a => Quad a -> Quad (Quad a)
subdivide (Quad x y w h) =
  let halfw = w / 2
      halfh = h / 2
   in Quad
        (Quad x           y           halfw halfh)
        (Quad (x + halfw) y           halfw halfh)
        (Quad x           (y + halfh) halfw halfh)
        (Quad (x + halfw) (y + halfh) halfw halfh)


data QT a = QT
  { qt_default :: a
  , qt_root_pow :: Integer
  , qt_tree :: Tree a
  }
  deriving stock (Show, Functor)
  deriving (Semigroup, Monoid) via (Ap QT a)

instance Eq a => Eq (QT a) where
  q1@(QT a m tr) == q2@(QT a' n tr') =
    case compare m n of
      LT -> realloc q1 == q2
      EQ -> a == a' && tr == tr'
      GT -> q1 == realloc q2


instance Applicative QT where
  pure a = QT a 0 $ pure a
  liftA2 fabc q1@(QT a m qta) q2@(QT b n qtb) =
    case compare m n of
      LT -> liftA2 fabc (realloc q1) q2
      EQ -> QT (fabc a b) m $ liftA2 fabc qta qtb
      GT -> liftA2 fabc q1 (realloc q2)




qt_region :: QT a -> Region
qt_region = mkRegionByPow . qt_root_pow


mkRegionByPow :: Integer -> Region
mkRegionByPow n =
  let side = 2 ^ n
   in Quad (-side) (-side) (side * 2) (side * 2)

doubleGo :: a -> Quad (Tree a) -> Tree a
doubleGo def (Quad tl tr
                   bl br) = Split $
  Quad
    (Split (Quad a a
                 a tl)) (Split (Quad a  a
                                     tr a))
    (Split (Quad a bl
                 a a)) (Split (Quad br a
                                    a  a))
  where
    a = Fill def


realloc :: QT a -> QT a
realloc (QT a n q) = QT a (n + 1) $ doubleGo a $ unwrap q


scale :: QT a -> QT a
scale (QT a n q) = QT a (n + 1) q

powToContainRegion :: Region -> Integer
powToContainRegion (Quad x y w h) =
  maximum $ (0 :) $ fmap (ceiling @Double . logBase 2 . fromRational)
    [ abs x
    , abs $ x + w
    , abs y
    , abs $ y + h
    ]

sel :: (Fractional r, Ord r) => a -> a -> Maybe (Quad r) -> Quad r -> Tree a
sel def _ Nothing _ = pure def
sel def v (Just r) qu = fillImpl def v r qu

fillImpl :: (Fractional r, Ord r) => a -> a -> Quad r -> Quad r -> Tree a
fillImpl def v area r
  | contains area r = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Split $ sel def v <$> subarea <*> subr
  | otherwise = pure def

rect :: a -> a -> Region -> QT a
rect def v (normalize -> r)
  | sizeof r == 0  = QT def (powToContainRegion r) $ pure def
  | otherwise = QT def (powToContainRegion r) $ fillImpl def v r $ mkRegionByPow (powToContainRegion r)

fill :: forall a. Region -> a -> QT a -> QT a
fill (normalize -> r) a q = liftA2 fromMaybe q (rect Nothing (Just a) r)

getLocationImpl :: V2 Rational -> Region -> Tree a -> Maybe a
getLocationImpl p r qt
  | containsPoint r p = case qt of
      Fill a -> Just a
      Split qu -> asum $ getLocationImpl p <$> subdivide r <*> qu
  | otherwise = Nothing

getLocation :: V2 Rational -> QT a -> a
getLocation v2 (QT a n q) = fromMaybe a $ getLocationImpl v2 (mkRegionByPow n) q

query :: Semilattice s => (a -> s) -> Region -> QT a -> s
query f (normalize -> area) (QT a n q)
  | contains r area = queryImpl f area r q
  | intersects r area = queryImpl f area r q /\ f a
  | otherwise = f a
  where
    r = mkRegionByPow n


queryImpl :: Semilattice s => (a -> s) -> Region -> Region -> Tree a -> s
queryImpl f area r (Fill a)
  | intersects area r = f a
  | otherwise = mempty
queryImpl f area r (Split qu)
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      fold $ sel2 f <$> subarea <*> subr <*> qu
  | otherwise = mempty

sel2 :: Semilattice s => (a -> s) -> Maybe Region -> Region -> Tree a -> s
sel2 _ Nothing _ _ = mempty
sel2 f (Just area) r q = queryImpl f area r q

atLeast :: Integer -> QT a -> QT a
atLeast m qt@(QT _ n _)
  | n < m = atLeast m $ realloc qt
  | otherwise = qt

elements :: Ord a => QT a -> Set a
elements qt = S.insert (qt_default qt) $ query S.singleton (qt_region qt) qt

fuse :: Eq a => QT a -> QT a
fuse (QT a n qt) = QT a n $ Raw.fuse qt

overlay :: forall a. QT a -> QT a -> QT a
overlay qt qt' = coerce @(QT (Last a)) $ coerce qt <> coerce qt'

