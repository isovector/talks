---
title: "Just Because It Works Doesn't Mean It's Right"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 2
    left: 5
    right: 5
---

# Just Because It Works Doesn't Mean It's Right

## Finding Elegance in Quadtrees

---

* **Sandy Maguire**
* sandy@sandymaguire.me

. . .

* reasonablypolymorphic.com
* github.com/isovector

. . .

Today's slides:

> reasonablypolymorphic.com/quadtrees

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

# Unimpressed

Three options on hackage. None are what I'm looking for:

. . .

- No desirable instances (eg no `Monoid`)

. . .

- Weird restrictions:
  `getLocation :: Eq a => Location -> QuadTree a -> a`{.haskell}

. . .

- No support for spatial hit tests.

. . .

Might as well just use `Map Location a`.

---

# I guess I'll write my own.

---

# Deeper problems in software.

We keep slapping solutions onto half-understood problems.

---

I want non-overlapping labels.

. . .

-> I need a spatial data structure.

. . .

-> I'll use a quadtree.

. . .

-> The quadtree libraries don't do what I want.

. . .

-> I'll write my own.

---

```haskell
data Quad a = Quad a a
                   a a
```

. . .

```haskell
  deriving stock ( Eq, Ord, Show, Generic
                 , Functor, Foldable, Traversable
                 )
```

---

```haskell
instance Applicative Quad where
  pure a = Quad a a
                a a
```

. . .

```haskell
  liftA2 f (Quad l1 l2
                 l3 l4) (Quad r1 r2
                              r3 r4)
    = Quad (f l1 r1) (f l2 r2)
           (f l3 r3) (f l4 r4)
```

---

```haskell
data Tree a where
  Fill  :: a -> Tree a
  Split :: Quad (Tree a) -> Tree a

  deriving Functor
```

---

```haskell
instance Applicative Tree where
  pure = Fill

  liftA2 = ...
```

---

```haskell
instance Applicative Tree where
  ...

  liftA2 f (Fill a) (Fill b) =
    Fill $ f a b
```

. . .

```haskell
  liftA2 f (Split a) (Split b) =
    Split $ liftA2 (liftA2 f) a b
```

---

```haskell
instance Applicative Tree where
  ...

  liftA2 f (Fill a) (Split b) =
    liftA2 f
      (Split (pure @Quad a))
      (Split b)

  liftA2 f (Split a) (Fill b) =
    liftA2 f
      (Split a)
      (Split (pure @Quad b))
```

---

```haskell
deriving via (Ap Tree)
  instance Semigroup a => Semigroup (Tree a)

deriving via (Ap Tree)
  instance Monoid a => Monoid (Tree a)
```

---

```
┌───┬───┬───────┐   ┌───┬───┬───────┐   ┌───┬───┬───────┐
│ 1 │ 2 │       │   │ 0 │ 2 │       │   │ 1 │ 4 │       │
├───┼───┤   1   │   ├───┼───┤   0   │   ├───┼───┤   1   │
│ 1 │ 1 │       │   │ 2 │ 6 │       │   │ 3 │ 7 │       │
├───┴───┼───────┤ + ├───┼───┼───────┤ = ├───┼───┼───────┤
│       │       │   │ 2 │ 3 │       │   │ 7 │ 8 │       │
│   5   │   8   │   ├───┼───┤   1   │   ├───┼───┤   9   │
│       │       │   │ 4 │ 1 │       │   │ 9 │ 6 │       │
└───────┴───────┘   └───┴───┴───────┘   └───┴───┴───────┘
```

---

---

NEW

---

problem

quadtree

hackage doesnt help

---

```haskell
data QT a
data Rect
data Point

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

---

```haskell
-- fill bounded law
forall r a q.
  fill r a q =
    fill (intersect r (bounds q)) a q

intersect :: Rect -> Rect -> Rect
```

---

```haskell
  bool (get q p) (Just a) (contains r p)
=  -- sym (get/fill)
  get (fill r a q)
=  -- fill bounded
  get (fill (intersect r (bounds q)) a q)
=  -- get/fill
  bool (get q p) (Just a) (contains (intersect r (bounds q)) p)
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

# WE FOOLED OURSELVES

> TODO(sandy): what makes a good library?
> it helps you not write bugs
> because you have a good mental model of how it works


. . .

It's **really** hard to come up with good laws!

. . .

Two options:

. . .

* Push the bounds checking everywhere and pray.

. . .

* Do something different.

> TODO: 15m

---

# What makes a good library?

Interplay between:

1. Flexibility
2. Elegance
3. Correctness

---

# Flexibility

- Our library should solve more problems than we can think of.

. . .

- Necessary for longevity.

. . .

- Organized around principles we already understand; e.g. typeclasses.

---

# Elegance

- It should *feel good* to use.

. . .

- *Parsimonious* in its primitives, and *symmetric* in what it provides.

. . .

- No weird axioms. No "extra baggage."

. . .

---

# Elegance

```haskell
lookup ::         Container k v -> k -> Maybe v
```

---

# Elegance

```haskell
lookup :: Eq v => Container k v -> k -> Maybe v
```

---

# Correctness

. . .

- Organized around a guiding metaphor.

. . .

- *Invites a new way of looking at the problem.*

. . .

- Correct with respect to the metaphor. Never breaks the abstraction.


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

> Is it **flexible**?

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

> Is it **elegant**?

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

> What's the **metaphor**?

- There isn't one! We don't even know what to strive for.

---

# Finding the Metaphor

Data structures are *representations of functions.*

. . .

What *function* do we care about?

. . .

Go back to the problem statement.

---

# Finding the Metaphor

We want to know what's stored where in space.

Therefore...

. . .

```haskell
⟦ QT a ⟧ = Point -> a
```

. . .

We are not trained to see this.

Feels inefficient and uncomputable.

> 24m

---

# Taking the Metaphor Seriously

This has immediate repercussions.

```haskell
get ::   QT a   -> Point -> Maybe a
```

---

# Taking the Metaphor Seriously

This has immediate repercussions.

```haskell
get :: ⟦ QT a ⟧ -> Point -> Maybe a
```

---

# Taking the Metaphor Seriously

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

# Taking the Metaphor Seriously

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

# Taking the Metaphor Seriously

What abound `bounds`?

```haskell
bounds ::   QT a   -> Rect
```

---

# Taking the Metaphor Seriously

What abound `bounds`?

```haskell
bounds :: ⟦ QT a ⟧ -> Rect
```

---

# Taking the Metaphor Seriously

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

# Taking the Metaphor Seriously

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

# Taking the Metaphor Seriously

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

# Addressing Asymmetries


