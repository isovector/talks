{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module NonNaive where

import GHC.Generics
import qualified Data.Set as S
import Data.Set (Set)
import Naive (Quad(..), QuadTree(..), unwrap, intersects, contains, getIntersect, containsPoint)
import Semilattice
import qualified Naive as Raw
import Data.Ratio
import Data.Semigroup (mtimesDefault)
import Data.Monoid
import GHC.Base (liftA2)
import Linear.V2
import Data.Foldable
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (Set)
import Data.Coerce (coerce)

type Region = Quad Rational

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
  , qt_tree :: QuadTree a
  }
  deriving stock (Eq, Ord, Show, Functor)
  deriving (Semigroup, Monoid) via (Ap QT a)


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

doubleGo :: a -> Quad (QuadTree a) -> QuadTree a
doubleGo def (Quad tl tr bl br) = Tree $
  Quad
    (Tree (Quad a a
                a tl)) (Tree (Quad a  a
                                   tr a))
    (Tree (Quad a bl
                a a)) (Tree (Quad br a
                                  a  a))
  where
    a = Leaf def


realloc :: QT a -> QT a
realloc (QT a n q) = QT a (n + 1) $ doubleGo a $ unwrap q


scale :: QT a -> QT a
scale (QT a n q) = QT a (n + 1) q

powToContainRegion :: Region -> Integer
powToContainRegion (Quad x y w h) =
  maximum $ fmap (ceiling @Double . logBase 2 . fromRational)
    [ abs x
    , abs $ x + w
    , abs y
    , abs $ y + h
    ]

sel :: (Fractional r, Ord r) => a -> a -> Maybe (Quad r) -> Quad r -> QuadTree a
sel def _ Nothing _ = pure def
sel def v (Just r) qu = fillImpl def v r qu

fillImpl :: (Fractional r, Ord r) => a -> a -> Quad r -> Quad r -> QuadTree a
fillImpl def v area r
  | contains area r = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Tree $ sel def v <$> subarea <*> subr
  | otherwise = pure def

fill :: a -> a -> Region -> QT a
fill def v r = QT def (powToContainRegion r) $ fillImpl def v r $ mkRegionByPow (powToContainRegion r)

getLocationImpl :: V2 Rational -> Region -> QuadTree a -> Maybe a
getLocationImpl p r qt
  | containsPoint r p = case qt of
      Leaf a -> Just a
      Tree qu -> asum $ getLocationImpl p <$> subdivide r <*> qu
  | otherwise = Nothing

getLocation :: V2 Rational -> QT a -> a
getLocation v2 (QT a n q) = fromMaybe a $ getLocationImpl v2 (mkRegionByPow n) q

query :: Semilattice s => (a -> s) -> Region -> QT a -> s
query f area (QT a n q)
  | intersects r area = queryImpl f area r q
  | otherwise = f a
  where
    r = mkRegionByPow n


queryImpl :: Semilattice s => (a -> s) -> Region -> Region -> QuadTree a -> s
queryImpl f area r (Leaf a)
  | intersects area r = f a
  | otherwise = mempty
queryImpl f area r (Tree qu)
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      fold $ sel2 f <$> subarea <*> subr <*> qu
  | otherwise = mempty

sel2 :: Semilattice s => (a -> s) -> Maybe Region -> Region -> QuadTree a -> s
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

overlay :: forall a. QT (Maybe a) -> QT (Maybe a) -> QT (Maybe a)
overlay qt qt' = coerce @(QT (Last a)) $ coerce qt <> coerce qt'

