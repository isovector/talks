{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-methods       #-}
{-# OPTIONS_GHC -Wno-orphans               #-}
{-# OPTIONS_GHC -Wno-typed-holes           #-}
{-# OPTIONS_GHC -Wno-unused-imports        #-}
{-# OPTIONS_GHC -fdefer-typed-holes        #-}

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

--    get (fill r a q) p = if contains r p then Just a else get q p
--    get (fill (intersect r (bounds q)) a q) p = if contains r p then Just a else get q p
--    if contains r p then Just a else get q p
--    if contains (intersect r (bounds q)) p then Just a else get q p
--
--    forall r q. r == intersect r (bounds q)


-- the point being this is actually really hard
-- and we are prone to fooling ourselves

-- we are left with two options:
-- we can push this bounds checking everywhere and pray
-- or we can instead do something different.

-- instead lets go back and ask ourselves whats the METAPHOR here
-- what are we really trying to model? well it's 2d space.
-- and what is 2d space?
-- it's a mapping `Point -> a`

-- we should be able to replace `QT a` with `Point -> a` everywhere
-- ⟦_⟧ : QT a -> (Point -> a)
--
--
-- this is the METAPHOR. we must be able to implement this everywhere
--
-- ⟦ empty r ⟧ = ??
--
-- bounds q = ⟦ q ⟧ ????
--
-- get q p = ⟦ q ⟧ p
--
-- ⟦ fill r a q ⟧ = \p ->
--  if contains r p
--    then Just a   -- type error! needs to be a
--    else ⟦ q ⟧ p
--
-- q1 == q2 = ⟦ q1 ⟧ == ⟦ q2 ⟧
-- ⟦ mempty @(QT a) ⟧ = mempty @(Point -> a)
-- ⟦ q1 <> q2 ⟧ = ⟦ q1 ⟧ <> ⟦ q2 ⟧
-- ⟦ fa <*> a ⟧ = ⟦ fa ⟧ <*> ⟦ a ⟧

-- okay sweet!
-- most of the energy on talks like these has been getting this far
-- how do we know if our API is correct?
--
-- but that's only half the battle. we also have to figure out how to implement it.
--
-- we started wanting to make a quadtree.
-- a quadtree feels like a good tradeoff in terms of efficiency stuff
-- of course there are infinity options here for implementations but we need to start somewhere
-- this one feels promising. can we give something that feels like a quadtree
-- but gives us the semantics we want?
--
-- data Quad a
-- data Tree a
--
-- this has no notion of COORDINATES
--
-- the bounds are a property of the implementation; not of the semantics
-- but a quadtree definitely spans some finite amount of space
--
-- let's solve the fill and lookup problems first
--
-- ...
--
-- ok so now we have an implementation of a quadtree
-- but it doesn't line up with our specification
--
-- why not? primarily the quadtree is always bounded, but the specification is not
-- hmm
--
-- need to attach bounds to our final implementation
-- but there is an issue
-- what if the lines dont line up?
-- then we spent most of our computation time recomputing things and can't share any work
-- VERY IMPORTANT that our lines line up
--
-- one idea: make the quadtree span the entire space
-- doesn't work; all of your finite space is compressed down to nothing
--
--
--
-- insight: we can only ever look up a finite number of points
-- infinity here really means "what to do for the space we haven't explicitly filled"
--
--
-- instead, maybe we can grow?

data Quad a

subdivide :: Rect -> Quad Rect
subdivide = _

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

