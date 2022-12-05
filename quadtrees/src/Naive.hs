{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

module Naive where

import qualified Data.Set as S
import Data.Set (Set)
import Control.Applicative (liftA2)
-- import Control.Monad.Free
-- import Control.Monad.Free
import Data.Monoid (Ap(..), All)
import Linear.V2
import Data.Foldable (asum, toList, fold)
import Data.Semigroup (Any)
import GHC.Generics (Generic)
import Semilattice
import Data.Maybe (isJust)

data Quad a = Quad a a
                   a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Quad a


instance Applicative Quad where
  pure a = Quad a a a a
  liftA2 fabc (Quad a a' a2 a3) (Quad b b' b2 b3)
    = Quad (fabc a b) (fabc a' b') (fabc a2 b2) (fabc a3 b3)

data Tree a
  = Fill a
  | Split (Quad (Tree a))
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Eq a => Eq (Tree a) where
  Fill a   == Fill b    = a             == b
  Split qu == Split qu' = qu            == qu'
  Fill a   == Split qu  = pure (pure a) == qu
  Split qu == Fill a    = pure (pure a) == qu



instance Applicative Tree where
  pure = Fill
  liftA2 fabc (Fill a) (Fill b) = Fill $ fabc a b
  liftA2 fabc (Fill a) (Split qu) = Split $ fmap (fmap (fabc a)) qu
  liftA2 fabc (Split qu) (Fill b) = Split $ fmap (fmap (flip  fabc b)) qu
  liftA2 fabc (Split qu) (Split qu') = Split $ liftA2 (liftA2 fabc) qu qu'

instance Monad Tree where
  Fill a >>= f = f a
  Split qu >>= f = Split $ fmap (>>= f) qu


type Region = Quad Int

contains :: (Ord a, Num a) => Quad a -> Quad a -> Bool
contains (Quad bx by bw bh) (Quad sx sy sw sh) =
  and
    [ bx <= sx
    , by <= sy
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    ]

normalize :: (Num a, Ord a) => Quad a -> Quad a
normalize q@(Quad x y w h)
  | w < 0 = let w' = abs w in normalize $ Quad (x - w') y w' h
  | h < 0 = let h' = abs h in normalize $ Quad x (y - h') w h'
  | otherwise = q

containsPoint :: (Ord a, Num a) => Quad a -> V2 a -> Bool
containsPoint (normalize -> Quad bx by bw bh) (V2 x y) =
  and
    [ bx <= x
    , by <= y
    , x <= bx + bw
    , y <= by + bh
    ]

corners :: (Num a, Ord a) => Quad a -> [V2 a]
corners (normalize -> Quad x y w h) = do
  dx <- [0, w]
  dy <- [0, h]
  pure $ V2 (x + dx) (y + dy)

intersects :: (Ord a, Num a) => Quad a -> Quad a -> Bool
intersects r1 r2 = isJust $ getIntersect r1 r2
  -- or
  -- [ any (containsPoint r1) (corners r2)
  -- , any (containsPoint r2) (corners r1)
  -- ]

sizeof :: Num a => Quad a -> a
sizeof (Quad _ _ w h) = w * h

getIntersect :: (Ord a, Num a) => Quad a -> Quad a -> Maybe (Quad a)
getIntersect (normalize -> r1) (normalize -> r2)
 | sizeof r1 == 0 = Just r1
 | sizeof r2 == 0 = Just r2
 | otherwise =
  let r_x (Quad x _ _ _) = x
      r_y (Quad _ y _ _) = y
      r_w (Quad _ _ w _) = w
      r_h (Quad _ _ _ h) = h

      x0 = max (r_x r1) (r_x r2)
      y0 = max (r_y r1) (r_y r2)
      x1 = min (r_x r1 + r_w r1) (r_x r2 + r_w r2)
      y1 = min (r_y r1 + r_h r1) (r_y r2 + r_h r2)
      w = x1 - x0
      h = y1 - y0
   in case 0 < w && 0 < h of
        True -> Just $ Quad x0 y0 w h
        False -> Nothing

subdivide :: Region -> Quad Region
subdivide (normalize -> Quad x y w h) =
  let halfw = div w 2
      halfh = div h 2
   in Quad
        (Quad x           y           halfw halfh)
        (Quad (x + halfw) y           halfw halfh)
        (Quad x           (y + halfh) halfw halfh)
        (Quad (x + halfw) (y + halfh) halfw halfh)


fill :: a -> Region -> Tree (Region, a) -> Tree (Region, a)
fill v area (Fill (r, a))
  | contains area r   = Fill (r, v)
  | intersects area r =
      Split $ fill v area <$> fmap (Fill . (, a)) (subdivide r)
  | otherwise = Fill (r, v)
fill v area (Split qu) =
  Split $ fill v area <$> qu

unwrap :: Tree a -> Quad (Tree a)
unwrap (Fill a) = pure $ pure a
unwrap (Split qu) = qu

sel :: a -> Maybe Region -> Region -> Tree a -> Tree a
sel _ Nothing _ qu = qu
sel v (Just area) r qu = fill2 v area r qu

fill2 :: a -> Region -> Region -> Tree a -> Tree a
fill2 v area r q
  | contains r area = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Split $ sel v <$> subarea <*> subr <*> unwrap q
  | otherwise = q

fill3 :: a -> Region -> Region -> Tree a -> Tree a
fill3 v area r q
  | contains r area = pure v
  | intersects area r = Split $ origami id (uncurry . fill3 v) area (r, unwrap q)
  | otherwise = q



origami
    :: (a -> b)                      -- ^ What to do if there is no intersection
    -> (Region -> (Region, a) -> b)  -- ^ What to do on an intersection
    -> Region                        -- ^ Looking for what
    -> (Region, Quad a)              -- ^ In the unnested quad
    -> Quad b
origami miss hit what (r, q) =
  let subr = subdivide r
      subw = getIntersect what <$> subr
      -- sel :: Maybe Region -> Region -> Quadrant a -> m
      sel Nothing _ q'   = miss q'
      sel (Just w) r' q' = hit w (r',  q')
   in sel <$> subw <*> subr <*> q

origami'
    :: b  -- ^ What to do if there is no intersection
    -> (a -> b)  -- ^ What to do on an intersection
    -> (Quad b -> b)  -- ^ What to do on an intersection
    -> Region                        -- ^ Looking for what
    -> (Region, Quad a)              -- ^ In the unnested quad
    -> Quad b
origami' miss cover hit what (r, q)
  | intersects r what
  = undefined
  | otherwise = undefined

getLocation :: V2 Int -> Tree (Region, a) -> Maybe a
getLocation p (Fill (r, a))
  | containsPoint r p = Just a
  | otherwise = Nothing
getLocation p (Split qu) = asum $ getLocation p <$> toList qu

getLocation2 :: V2 Int -> Region -> Tree a -> Maybe a
getLocation2 p r qt
  | containsPoint r p = case qt of
      Fill a -> Just a
      Split qu -> asum $ getLocation2 p <$> subdivide r <*> qu
  | otherwise = Nothing

regionify :: Region -> Tree a -> Tree (Region, a)
regionify r (Fill a) = Fill (r, a)
regionify r (Split qu) = Split $ regionify <$> subdivide r <*> qu

-- test :: Semilattice s => (a -> s) -> Region -> Region -> Tree a -> s
-- test f area r (Fill a) = f a
-- test f area r (Split qu') = undefined

-- rect :: a -> Region -> Squadt



fuse :: Eq a => Tree a -> Tree a
fuse (Fill a) = Fill a
fuse (Split q) = doFuse $ fmap fuse q

doFuse :: Eq a => Quad (Tree a) -> Tree a
doFuse (Quad (Fill a) (Fill b) (Fill c) (Fill d))
  | a == b
  , b == c
  , c == d = Fill a
doFuse q = Split q


deriving via Ap Tree a instance Semigroup a => Semigroup (Tree a)
deriving via Ap Tree a instance Monoid    a => Monoid    (Tree a)

-- type Tree = Free Quad

-- pattern Fill :: a -> Tree a
-- pattern Fill a = Pure a

-- pattern Split :: Quad (Tree a) -> Tree a
-- pattern Split a = Free a

-- {-# COMPLETE Split, Fill #-}

