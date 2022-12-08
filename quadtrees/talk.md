---
title: "Just Because It Works Doesn't Mean It's Right"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 2
    left: 20
    right: 20
---

&nbsp;

&nbsp;

&nbsp;

> The first principle is that you must not fool yourself
> and you are the easiest person to fool.
>
> --Richard Feynman

---

&nbsp;

# Just Because It Works Doesn't Mean It's Right

## Finding Elegance in Quadtrees

&nbsp;

Slides available: *tinyurl.com/hsx-quadtrees*

---

* **Sandy Maguire**
* hello@sandymaguire.me

* reasonablypolymorphic.com
* github.com/isovector


---

# A Philosophical Dichotomy

As programmers we each fall into two roles:

- users of libraries
- writers of libraries

These roles come with drastically different approaches.

---

# A Philosophical Dichotomy

As a *library user*:

. . .

- Software systems are too big to fit into my brain. I am unable to audit all
  code I use.

. . .

- Therefore, I must have a great deal of trust when I use libraries.

. . .

- Justifiably angry when that trust is broken.

---

# A Philosophical Dichotomy

As a *library writer*:

. . .

- I wrote it all; have a good understanding of the moving pieces.

. . .

- Likely written as part of a larger problem; separated afterwards.

. . .

- Lots of implicit assumptions about the code I've separated it from.

. . .

- I want to implement what's most convenient.

. . .

- If I am conscientious, I will document dodgy decisions made in the name of
  convenience.

---

**These two roles are fundamentally in tension.**

&nbsp;

. . .

Writers provide *macros.*

But users want *libraries.*

---

# Libraries vs Macros

Macros:

- Automate tedious tasks.

. . .

- Are explicitly about "how", never "why"

. . .

- Present pre-baked recipes; hope your problem can be solved by one.

---

# Examples of Macros

- Bash scripts
- Functions which exist only to call other functions
- API Wrappers
- CI
- "Facade pattern"
- Most algorithms

&nbsp;

. . .

Characterized by:

- lack of flexibility/composability
- the implementation is the specification
- require debugging the underlying layer when things go wrong

---

# Libraries vs Macros

Libraries:


- Separate *what* from *how*.

. . .

- Do not require an understanding of their implementation.

. . .

- Create a new semantic domain you can reason about.

. . .

- Can solve problems the author never anticipated.

. . .

- Are pulled in before you have a solution in mind.

. . .

&nbsp;

This is not true of most software we link in.

---

# What Makes a Good Library?

A good library interleaves these three principles:

&nbsp;

- *Flexibility:* applicability, composability, discoverability

. . .

- *Elegance:* parsimony, symmetry, derived from more general ideas

. . .

- *Correctness:* can you trust it?

. . .

&nbsp;

Sounds overly constraining; but we have many degrees of freedom.

---

# On Correctness

Correctness exists only in relation to a *model.*

. . .

The model is a metaphor for how the library behaves.

. . .

The model and the library must behave identically.

**It is a mortal sin to do otherwise.**

---

# On Correctness

Nobody tries to write bugs.

. . .

Bugs happen when our understanding disagrees with the implementation.

&nbsp;

. . .

*Our understanding is almost always more correct than the implementation is.*

&nbsp;

. . .

*Corollary:* bugs in implementation arise from bugs in the semantics.

---

# On Correctness

Having an indistinguishable model is an extremely tight constraint on
implementation.

. . .

This means:

- *every observation* you can make of the library *must hold* of the model

- you must not leak any implementation details

. . .

However:

- but not everything expressible about the model need hold in the library

---

**Enough philosophy.**

---

# The Problem

I wanted to build a scatter plot with labels.

. . .

- The labels must be legible.

. . .

- Important they not overlap.

. . .

- This is NP-complete :(

---

# Digging A Hole

- Maybe just brute force it?

. . .

- I need a spatial data structure.

. . .

- Quadtrees are the only spatial data structure I know about...

. . .

- I'll use a quadtree!

---

# A Quick Quadtree Introduction

```
┌───────────────┐
│               │
│               │
│               │
│               │
│               │
│               │
│               │
│      1        │
│               │
│               │
│               │
│               │
│               │
│               │
│               │
└───────────────┘
```

---

# A Quick Quadtree Introduction

```
┌───────┬───────┐
│       │       │
│       │       │
│       │       │
│   2   │   1   │
│       │       │
│       │       │
│       │       │
├───────┼───────┤
│       │       │
│       │       │
│       │       │
│   1   │   1   │
│       │       │
│       │       │
│       │       │
└───────┴───────┘
```

---

# A Quick Quadtree Introduction

```
┌───┬───┬───────┐
│   │   │       │
│ 2 │ 2 │       │
│   │   │       │
├───┼───┤   1   │
│   │   │       │
│ 2 │ 7 │       │
│   │   │       │
├───┴───┼───────┤
│       │       │
│       │       │
│       │       │
│   1   │   1   │
│       │       │
│       │       │
│       │       │
└───────┴───────┘
```

---

# A Quick Quadtree Introduction

```
┌───┬───┬───┬───┐
│   │   │   │   │
│ 2 │ 2 │ 1 │ 1 │
│   │   │   │   │
├───┼─┬─┼─┬─┼───┤
│   │0│0│0│0│   │
│ 2 ├─┼─┼─┼─┤ 1 │
│   │7│7│1│1│   │
├───┴─┴─┼─┴─┴───┤
│       │       │
│       │       │
│       │       │
│   1   │   1   │
│       │       │
│       │       │
│       │       │
└───────┴───────┘
```

---

# How Does a Quadtree Help?

Most problems have space locality:

> "Nearby areas of space likely have the same value."

Quadtrees take advantage of locality to compress information.

---

# Missing Library Support

Three options on hackage. None are what I'm looking for:

. . .

- No desirable instances (eg no `Monoid`)

. . .

- No support for spatial hit tests.

. . .

Might as well just use `Map Location a`.

---

**I guess I'll write my own.**

---

# Implementation

```haskell
data QuadTree a
  = Fill a
  | Split (QuadTree a) (QuadTree a)
          (QuadTree a) (QuadTree a)
```

. . .

Why not:

```haskell
data Quad a = Quad a a
                   a a

data QuadTree a
  = Fill a
  | Split (Quad (QuadTree a))
```

---

# Implementation

From the zoo:

```haskell
data Free f a
  = Pure a
  | Wrap (f (Free f a))
```

```haskell
type QuadTree = Free Quad
```

This is the free monad on `Quad`s!

---

**This is dangerously persuasive.**

It can be tempting to say our work here is done.

. . .

We have implemented a data structure in turns of the Functional Programming
Pantheon.

. . .

Slap some perfunctory operations on it and call it a day.

. . .

And that's what I did.

---

# Operations

```haskell
type Rect = Quad Int
type SQuadTree a = QuadTree (Rect, a)

data Point = Point
  { p_x :: Int
  , p_y :: Int
  }

regionify :: QuadTree a -> Rect -> SQuadTree a
fill      :: Rect -> a -> SQuadTree a -> QuadTree a
get       :: SQuadTree a -> Point -> Maybe a
hitTest   :: SQuadTree a -> Rect -> Bool
```

The implementations are not very involved.

---

```haskell
type Rect = Quad Int

subdivide :: Rect -> Quad Rect
subdivide (Quad x y w h) =
  let halfw = div w 2
      halfh = div h 2
      x' = x + halfw
      y' = y + halfh
      q x y = Quad x y halfw halfh
   in Quad (q x y ) (q x' y )
           (q x y') (q x' y')
```

---

```haskell
fill :: Rect -> a -> SQuadTree a -> QuadTree a
fill area a' v@(Fill (bounds, a))
  | area `contains`   bounds = Fill a'
  | area `intersects` bounds = Split $
      fill area a'
        <$> ( (,) <$> subdivide bounds
                  <*> pure @Quad
                        (pure @QuadTree a)
            )
  | otherwise = v
```

. . .

```haskell
fill area a' (Split q) =
  Split $ fill area a' <$> q
```

---

# It works!

. . .

## Is it right?

. . .

Unclear, but it's certainly *unusable.*

. . .

Turns out to be very slow!

---

Recall:

```haskell
data QuadTree a
  = Fill a
  | Split (Quad (QuadTree a))

type SQuadTree a = QuadTree (Rect, a)
```

. . .

The `Rect` data is only on the leaves of this tree!

**WE FOOLED OURSELVES**

. . .

Not hard to fix:

```haskell
type SQuadTree a = (Rect, QuadTree a)
```

Make everything typecheck again.

---



---

# First Attempt at the Library

`SQuadTree` isn't nice to work with. Instead, wrap it.

. . .

```haskell
data QT a = QT
  { qt_bounds :: Rect
  , qt_tree   :: QuadTree a
  }
```

```haskell
liftSQuad
    :: (SQuadTree a -> QuadTree a)
    -> QT a
    -> QT a
liftSQuad f qt@(QT b q) =
  qt { qt_tree = f (b, q)
     }
```

---

**Looks good.**

But what can we say about its semantics?

---

# Looking at Laws

```haskell
empty   :: Rect -> QT a
fill    :: Rect -> a -> QT a -> QT a
get     :: QT a -> Point -> Maybe a
hitTest :: QT a -> Rect -> Bool
```
. . .

```haskell
bounds  :: QT a -> Rect
```

. . .

Let's write some laws!

---

```haskell
-- get/empty law
forall r p.
  get (empty r) p = Nothing
```

. . .

```haskell
-- get/fill law
forall r p a q.
  get (fill r a q) p =
    bool (get q p) (Just a) (contains r p)

contains :: Rect -> Point -> Bool
```

. . .

Notice this doesn't actually do any bounds checking!

Hard to work around, and complicated laws are smells of a bad design.

---

```haskell
-- fill bounded law
forall r a q.
  fill r a q =
    fill (intersect r (bounds q)) a q

intersect :: Rect -> Rect -> Rect
```

---

```
┌──────────┐
│          │
│       ┌──┼────┐
│       │  │    │
│       └──┼────┘
│          │
└──────────┘
```

---

```
┌──────────┐
│          │
│       ┌──┤
│       │  │
│       └──┤
│          │
└──────────┘
```

---

# Uh Oh

```haskell
  get (fill r a q)
=  -- fill bounded
  get (fill (intersect r (bounds q)) a q)
```


. . .


```haskell
r = intersect r (bounds q)
```

. . .

```haskell
intersect :: Rect -> Rect -> Rect
intersect r _ = r
```

---

**WE FOOLED OURSELVES**

. . .

It's **really** hard to come up with good laws!

. . .

Two options:

. . .

* Push the bounds checking everywhere and pray.

. . .

* Do something different.

---

# Revisiting the API

```haskell
data QT a
data Rect
data Point

empty   :: Rect -> QT a
bounds  :: QT a -> Rect
fill    :: Rect -> a -> QT a -> QT a
get     :: QT a -> Point -> Maybe a
hitTest :: QT a -> Rect -> Bool
```

> What's the **model**?

- There isn't one! We don't even know what to strive for.

---

# Finding the Model

Data structures are *representations of functions.*

. . .

What *function* do we care about?

. . .

Go back to the problem statement.

---

# Finding the Model

We want to know what's stored where in space.

Therefore...

. . .

```haskell
type ⟦ QT a ⟧ = Point -> a
```

. . .

```haskell
⟦-⟧ :: QT a -> ⟦ QT a ⟧
```

. . .

http://conal.net/papers/type-class-morphisms/

---

# Taking the Model Seriously

This has immediate repercussions.

```haskell
get ::   QT a   -> Point -> Maybe a
```

---

# Taking the Model Seriously

This has immediate repercussions.

```haskell
get :: ⟦ QT a ⟧ -> Point -> Maybe a
```

---

# Taking the Model Seriously

This has immediate repercussions.

```haskell
get :: (Point -> a) -> Point -> Maybe a
```

. . .

Instead, it should be:

```haskell
get :: QT a -> Point -> a
```

---

# Taking the Model Seriously

Now:

```haskell
get :: QT a -> Point -> a
empty :: Rect -> QT a
```

If `get` is no longer partial, `empty` must provide a value.

. . .

```haskell
empty :: Rect -> a -> QT a
```

. . .

Looks vaguely like `fill` now.

---

# Taking the Model Seriously

What abound `bounds`?

```haskell
bounds ::   QT a   -> Rect
```

---

# Taking the Model Seriously

What abound `bounds`?

```haskell
bounds :: ⟦ QT a ⟧ -> Rect
```

---

# Taking the Model Seriously

What abound `bounds`?


```haskell
bounds :: (Point -> a) -> Rect
```

. . .

Clearly we need more structure to assign semantics.

. . .

```haskell
bounds :: QT (Maybe a) -> Rect
```

. . .

```haskell
bounds :: Monoid a => QT a -> Rect
```

. . .

Seemingly no good solution.

. . .

Axe it.

---

# Taking the Model Seriously

From symmetry:

```haskell
empty :: Rect -> a -> QT a
bounds :: QT a -> Rect
```

But no more `bounds`.

. . .

From symmetry, maybe there is no `Rect` for empty?

. . .

```haskell
empty :: a -> QT a
```

---

# Taking the Model Seriously

```haskell
pure  :: a -> QT a
(<*>) :: QT (a -> b) -> QT a -> QT b
```

What laws must these have?

. . .

```haskell
forall a.
  ⟦ pure @QT a ⟧ =
    pure @(Point -> _) a  -- const a
```

. . .


```haskell
forall qf qa.
  ⟦ qf <*> qa ⟧
    ⟦ qf ⟧ <*> ⟦ qa ⟧
```

. . .

*Extremely* constraining.

---

# More Problems

This is an issue. But there are also others.

We'll fix it after we've explored all the problems.

---

# Addressing Asymmetries

```haskell
hitTest :: QT a -> Rect -> Bool
```

Not enough structure anymore.

. . .

```haskell
hitTest :: (a -> Bool) -> QT a -> Rect -> Bool
```

What is this `Bool`?

. . .

```haskell
hitTest :: (a -> Any) -> QT a -> Rect -> Bool
```

. . .

Generalize to any monoid!

```haskell
hitTest :: Monoid m => (a -> m) -> QT a -> Rect -> m
```

---

# Correctness of hitTest

```haskell
forall r f q.
  hitTest f q r
    = foldMap (f . get q) $ pointsInRect r

pointsInRect :: Rect -> [Point]
```

---

# Implementation

```haskell
hitTest :: Monoid m => (a -> m) -> QT a -> Rect -> m
hitTest f qt r
  = isEmptyRect r = mempty
  | otherwise =
      case qt of
        Everywhere a -> f a
        QT b q a -> ...
          -- check each `Split` to see if it intersects, if so, recurse
          -- call `f` on every intersecting `Fill`
```

. . .

What goes wrong when `f = const (Sum 1)`?

. . .

We can observe the number of intersecting quadrants

. . .

&nbsp;

**THIS IS NOT A PROPERTY** of `Point -> a`

---

**WE FOOLED OURSELVES! AGAIN!**

. . .

&nbsp;

We can observe something of the interpretation that *doesn't hold in the
sematics.*

---

# Fixing It

**Problem**: we *compress most space.*

. . .

**Bad solution**: expand that space out again.

---

# Fixing It

Why a bad solution? Throws away locality.

---

# Fixing It

```
┌─┬─┬─┬─┬─┬─┬─┬─┐
│1│2│3│4│5│6│7│8│  Split
├─┼─┼─┼─┼─┼─┼─┼─┤    (Split
│a│b│c│d│e│f│g│h│      (Split 1 2 a b) (Split 3 4 c d)
├─┴─┼─┴─┼─┴─┼─┴─┤      ... ...)
│   │   │   │   │    (Split
│   │   │   │   │      (Split 5 6 e f) (Split 7 8 g h)
│   │   │   │   │      ... ...)
├───┴───┼───┴───┤    ...
│       │       │    ...
│       │       │
│       │       │
│       │       │
│       │       │
│       │       │
│       │       │
└───────┴───────┘
```

---

# Fixing It

Not only do we throw away locality, we also throw away our *compression.*

Two representations of the same space:

```
┌───────┐       ┌───┬───┐
│       │       │   │   │
│       │       │ x │ x │
│       │       │   │   │
│   x   │   =   ├───┼───┤
│       │       │   │   │
│       │       │ x │ x │
│       │       │   │   │
└───────┘       └───┴───┘
```

It would be nice if we could exploit this and always use the left-side.

---

# Fixing It

**Problem**: we *compress most space.*

**Solution**: patch `hitTest`'s type to make it unobservable.

. . .

&nbsp;

Good example of the degrees of freedom we can exploit in our API design.

---

# Fixing It

Locality concerns require our monoidal operation to be *commutative:*

```haskell
-- commutativity
forall x y.
  x <> y = y <> x
```

. . .

Compression concerns require *idempotence:*

```haskell
-- idempotency
forall x.
  x <> x = x
```

. . .

Therefore, `Monoid` is too loose a constraint for `hitTest`!

---

# Fixing It

A monoid with idempotence and commutativity is called a *semilattice.*

```haskell
class Monoid a => Semilattice a where
  (/\) :: a -> a -> a
```

. . .

```haskell
instance Semilattice Any
instance Semilattice Or
instance Ord a => Semilattice (Data.Set.Set a)
```

. . .

```haskell
hitTest :: Semilattice m => (a -> m) -> QT a -> Rect -> m
```

---

**The semantics are fixed!**

But is it right?

. . .

Almost.

. . .

We are missing an `Applicative` instance for `QT`.

---

# The Hunt for the Missing Applicative

Recall:

```haskell
data QT a = QT
  { qt_bounds     :: Rect
  , qt_tree       :: QuadTree a
  }
```

. . .

What if we lookup a point **outside of bounds?**

---

# The Hunt for the Missing Applicative

Recall:

```haskell
data QT a = QT
  { qt_bounds     :: Rect
  , qt_tree       :: QuadTree a
  , qt_everywhere :: a
  }
```

What if we lookup a point **outside of bounds?**

---

# Applicatives

*Fact:*

. . .

> products of "monoidal" types have obvious applicative instances.

. . .

```haskell
data Ex a = Ex
  { foo :: [a]        -- has applicative
  , qux :: Sum Int    -- has monoid
  }
```

---

# Applicatives

*Fact:*

> products of "monoidal" types have obvious applicative instances.

```haskell
data Ex a = Ex
  { foo :: [a]        -- has applicative
  , qux :: Sum Int    -- has monoid
  }

instance Applicative Ex where
  pure a = Ex (pure a)  -- use applicative
              mempty    -- use monoid
  Ex fx fy <*> Ex ax ay =
    Ex (fx <*> ax)  -- use applicative
       (fy <> ay)   -- use monoid
```

. . .

How can we apply this fact to `QT`?

---

# Implementation

```haskell
data QT a = QT
  { qt_bounds     :: Rect
  , qt_tree       :: QuadTree a  -- has applicative
  , qt_everywhere :: a           -- has applicative
  }
```

Need some sort of monoid or applicative on `Rect`.

. . .

Obvious idea: take the bounding volume:

---

# Bounding Volume

```
┌───────────┐
│           │
│           │
│     ┌─────┼─────┐
│     │     │     │
└─────┼─────┘     │
      │           │
      │           │
      └───────────┘
```

---

# Bounding Volume

```
┌─────────────────┐
│                 │
│                 │
│                 │
│                 │
│                 │
│                 │
│                 │
└─────────────────┘
```

. . .

Right idea; but has a **maximally antagonistic case.**

---

# Bounding Volume

```
┌─────────────┐
│┌────────────┼┐
││            ││
││            ││
││            ││
││            ││
└┼────────────┘│
 └─────────────┘
```

---

# Bounding Volume

```
┌──────┬──────┐
│┌─────┼┬─────┼┐
││     ││     ││
├┼─────┼┼─────┤│
│├─────┼┼─────┼┤
││     ││     ││
└┼─────┴┼─────┘│
 └──────┴──────┘
```

**Nothing lines up!**

---

# Bounding Volumes

Desirable to find "quantized" bounding regions that we can cheaply merge.

. . .

*Idea:* take inspiration from other amortized data structures.

---

# Bounding Volumes

*Idea:* Create a (0,0)-centered region with size $2ⁿ$


```




    ┌───2ⁿ──┐
    ┌───┬───┐
    │   │   │
    │ 1 │ 2 │
    │   │   │
    ├───┼───┤ 0
    │   │   │
    │ 3 │ 4 │
    │   │   │
    └───┴───┘
```

---

# Bounding Volumes

We can then "resize" the canvas to size $2ⁿ⁺¹$

```
┌─────2ⁿ⁺¹──────┐
┌───┬───┬───┬───┐
│   │   │   │   │
│ 0 │ 0 │ 0 │ 0 │
│   │   │   │   │
├───┼───┼───┼───┤
│   │   │   │   │
│ 0 │ 1 │ 2 │ 0 │
│   │   │   │   │
├───┼───┼───┼───┤ 0
│   │   │   │   │
│ 0 │ 3 │ 4 │ 0 │
│   │   │   │   │
├───┼───┼───┼───┤
│   │   │   │   │
│ 0 │ 0 │ 0 │ 0 │
│   │   │   │   │
└───┴───┴───┴───┘
```

. . .

This looks like it will work!

---


# Implementation

Recall, looking for a monoid instance:

```haskell
data QT a = QT
  { qt_bounds     :: Max Integer  -- has semigroup
  , qt_tree       :: QuadTree a   -- has applicative
  , qt_everywhere :: a            -- has applicative
  }

boundingVolume :: Integer -> Rect
```

. . .

A semigroup is not a monoid.

. . .

But we can generate one freely!

---

# Implementation


```haskell
data QT a
  = QT
    { qt_bounds     :: Max Integer
    , qt_tree       :: QuadTree a
    , qt_everywhere :: a
    }
  | Everywhere
    { qt_everywhere :: a
    }
```

This looks good!

---

# Addressing Asymmetries

```haskell
fill :: Rect -> a -> QT a -> QT a

forall r a q.
  ⟦ fill r a q ⟧ = \p ->
    if contains r p
      then a
      else ⟦ q ⟧ p
```

. . .

This is complicated!

Decompose it?

. . .


```haskell
rect :: a -> a -> Rect -> QT a

forall r a q.
  ⟦ rect f t r ⟧ = \p ->
    if contains r p
      then t
      else f
```

---

# Addressing Asymmetries

```haskell
fill :: Rect -> a -> QT a -> QT a
fill r a q
  = fromMaybe
      <$> q
      <*> rect Nothing (Just a) r
```

Note: this is a *definition*!

---

# Takeaways

- Bugs in the implementation point to bugs in the semantics.

. . .

- Bad semantics in libraries lead to immeasurable pain for downstream users.

. . .

- Decide on a model to define correctness; ruthlessly pursue it.

---

&nbsp;

# Resources

- http://conal.net/papers/type-class-morphisms/

&nbsp;

# Questions?

* **Sandy Maguire**
* sandy@sandymaguire.me

