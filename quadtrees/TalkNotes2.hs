{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-typed-holes     #-}
{-# OPTIONS_GHC -fdefer-typed-holes  #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TalkNotes where

import Data.Monoid
import Data.Maybe (isJust)


data QT a

instance Functor QT
instance Semigroup a => Semigroup (QT a)
instance Monoid a => Monoid (QT a)

data Rect
instance Eq Rect

data Point
instance Eq Point


empty   :: Rect -> QT a
fill    :: Rect -> a -> QT a -> QT a
get     :: QT a -> Point -> Maybe a
hitTest :: QT a -> Rect -> Bool

-- for symmetry, we should also add
bounds  :: QT a -> Rect

-- what laws?
{-# RULES

"get/empty"
  forall r p.
    get (empty r) p = Nothing

"get/fill"
  forall r p a q.
    get (fill r a q) p =
      if contains r p
         then Just a
         else get q p

   #-}

-- this doesn't actually do bounds checking :)

-- maybe we can patch it like this

intersect :: Rect -> Rect -> Rect

{-# RULES

"fill bounded"
  forall r a q.
    fill r a q = fill (intersect r (bounds q)) a q
   #-}

rectPoints :: Rect -> [Point]
rectPoints = _

contains :: Rect -> Point -> Bool
contains = _

{-# RULES

"bounds/empty"
  forall r.
    bounds (empty r) = r

"bounds/fill"
  forall r a q.
    -- TODO(sandy): ?
    bounds (fill r a q) = bounds q

"get/empty"
  forall r p.
    get (empty r) p = Nothing

"get/fill"
  forall r p a q.
    get (fill r a q) p =
      if contains r p
         then Just a
         else get q p

"hitTest/get"
  forall q r.
    hitTest q r =
      getAny (foldMap (Any . isJust . get q) (rectPoints r))

"get/empty'"
  forall r p.
    get (empty r) p =
      if contains r p
         then Nothing
         else error "out of bounds"

"fill/fill"
  forall r q a a'.
    fill r a' (fill r a q) = fill r a' q

"get/<>"
  forall a. forall (q1 :: Semigroup a => QT a) q2 p.
    get (q1 <> q2) p = get q1 p <> get q2 p

"get/mempty"
  forall a. forall (p :: Monoid a => Point).
    get mempty p = mempty @(Maybe a)

   #-}

empty = _
bounds = _
fill = _
get = _
hitTest = _
intersect = _

