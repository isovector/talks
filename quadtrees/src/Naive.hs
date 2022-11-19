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

module Naive where

import Control.Applicative (liftA2)
import Control.Monad.Free
import Data.Monoid (Ap(..))
import Linear.V2
import Data.Foldable (asum, toList)

data Quad a = Quad a a
                   a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via Ap Quad a


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

getLocation :: V2 Int -> QuadTree (Region, a) -> Maybe a
getLocation p (Leaf (r, a))
  | containsPoint r p = Just a
  | otherwise = Nothing
getLocation p (Tree qu) = asum $ getLocation p <$> toList qu

getLocation2 :: V2 Int -> QuadTree (Region, a) -> Maybe a
getLocation2 p (Leaf (r, a))
  | containsPoint r p = Just a
  | otherwise = Nothing
getLocation2 p (Tree qu) = asum $ getLocation p <$> toList qu

regionify :: Region -> QuadTree a -> QuadTree (Region, a)
regionify r (Leaf a) = Leaf (r, a)
regionify r (Tree qu) = Tree $ regionify <$> subdivide r <*> qu


deriving via Ap QuadTree a instance Semigroup a => Semigroup (QuadTree a)
deriving via Ap QuadTree a instance Monoid    a => Monoid    (QuadTree a)

-- type QuadTree = Free Quad

-- pattern Leaf :: a -> QuadTree a
-- pattern Leaf a = Pure a

-- pattern Tree :: Quad (QuadTree a) -> QuadTree a
-- pattern Tree a = Free a

-- {-# COMPLETE Tree, Leaf #-}

