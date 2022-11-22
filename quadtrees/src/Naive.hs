{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}

module Naive where

import Control.Applicative (liftA2)
-- import Control.Monad.Free
-- import Control.Monad.Free
import Data.Monoid (Ap(..), All)
import Linear.V2
import Data.Foldable (asum, toList, fold)
import Data.Semigroup (Any)
import GHC.Generics (Generic)

data Quad a = Quad a a
                   a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Quad a

class Monoid a => Semilattice a where
    (/\) :: a -> a -> a
    (/\) = (<>)

instance Semilattice Any
instance Semilattice All


instance Applicative Quad where
  pure a = Quad a a a a
  liftA2 fabc (Quad a a' a2 a3) (Quad b b' b2 b3)
    = Quad (fabc a b) (fabc a' b') (fabc a2 b2) (fabc a3 b3)

data QuadTree a
  = Leaf a
  | Tree (Quad (QuadTree a))
  deriving (Eq, Ord, Show, Functor)

instance Applicative QuadTree where
  pure = Leaf
  liftA2 fabc (Leaf a) (Leaf b) = Leaf $ fabc a b
  liftA2 fabc (Leaf a) (Tree qu) = Tree $ fmap (fmap (fabc a)) qu
  liftA2 fabc (Tree qu) (Leaf b) = Tree $ fmap (fmap (flip  fabc b)) qu
  liftA2 fabc (Tree qu) (Tree qu') = Tree $ liftA2 (liftA2 fabc) qu qu'

instance Monad QuadTree where
  Leaf a >>= f = f a
  Tree qu >>= f = Tree $ fmap (>>= f) qu


type Region = Quad Int

contains :: Region -> Region -> Bool
contains (Quad bx by bw bh) (Quad sx sy sw sh) =
  and
    [ bx <= sx
    , by <= sy
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    ]

containsPoint :: Region -> V2 Int -> Bool
containsPoint (Quad bx by bw bh) (V2 x y) =
  and
    [ bx <= x
    , by <= y
    , x <= bx + bw
    , y <= by + bh
    ]

corners :: Region -> [V2 Int]
corners (Quad x y w h) = do
  dx <- [0, w]
  dy <- [0, h]
  pure $ V2 (x + dx) (y + dy)

intersects :: Region -> Region -> Bool
intersects r1 r2 = or
  [ any (containsPoint r1) (corners r2)
  , any (containsPoint r2) (corners r1)
  ]

getIntersect :: Region -> Region -> Maybe Region
getIntersect = undefined

subdivide :: Region -> Quad Region
subdivide (Quad x y w h) =
  let halfw = div w 2
      halfh = div h 2
   in Quad
        (Quad x           y           halfw halfh)
        (Quad (x + halfw) y           halfw halfh)
        (Quad x           (y + halfh) halfw halfh)
        (Quad (x + halfw) (y + halfh) halfw halfh)


fill :: a -> Region -> QuadTree (Region, a) -> QuadTree (Region, a)
fill v area (Leaf (r, a))
  | contains area r   = Leaf (r, v)
  | intersects area r =
      Tree $ fill v area <$> fmap (Leaf . (, a)) (subdivide r)
  | otherwise = Leaf (r, v)
fill v area (Tree qu) =
  Tree $ fill v area <$> qu

unwrap :: QuadTree a -> Quad (QuadTree a)
unwrap (Leaf a) = pure $ pure a
unwrap (Tree qu) = qu

sel :: a -> Maybe Region -> Region -> QuadTree a -> QuadTree a
sel _ Nothing _ qu = qu
sel v (Just area) r qu = fill2 v area r qu

fill2 :: a -> Region -> Region -> QuadTree a -> QuadTree a
fill2 v area r q
  | contains r area = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Tree $ sel v <$> subarea <*> subr <*> unwrap q
  | otherwise = q

fill3 :: a -> Region -> Region -> QuadTree a -> QuadTree a
fill3 v area r q
  | contains r area = pure v
  | intersects area r = Tree $ origami id (uncurry . fill3 v) area (r, unwrap q)
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

getLocation :: V2 Int -> QuadTree (Region, a) -> Maybe a
getLocation p (Leaf (r, a))
  | containsPoint r p = Just a
  | otherwise = Nothing
getLocation p (Tree qu) = asum $ getLocation p <$> toList qu

getLocation2 :: V2 Int -> Region -> QuadTree a -> Maybe a
getLocation2 p r qt
  | containsPoint r p = case qt of
      Leaf a -> Just a
      Tree qu -> asum $ getLocation2 p <$> subdivide r <*> qu
  | otherwise = Nothing

regionify :: Region -> QuadTree a -> QuadTree (Region, a)
regionify r (Leaf a) = Leaf (r, a)
regionify r (Tree qu) = Tree $ regionify <$> subdivide r <*> qu

test :: Semilattice s => (a -> s) -> Region -> Region -> QuadTree a -> s
test f area r (Leaf a) = f a
test f area r (Tree qu') = _wk


deriving via Ap QuadTree a instance Semigroup a => Semigroup (QuadTree a)
deriving via Ap QuadTree a instance Monoid    a => Monoid    (QuadTree a)

-- type QuadTree = Free Quad

-- pattern Leaf :: a -> QuadTree a
-- pattern Leaf a = Pure a

-- pattern Tree :: Quad (QuadTree a) -> QuadTree a
-- pattern Tree a = Free a

-- {-# COMPLETE Tree, Leaf #-}

