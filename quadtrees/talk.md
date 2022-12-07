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

# A Philosophical Dichotomy

Nobody tries to write bugs.

. . .

Bugs arise through local/global impedience mismatches.

. . .

Bugs in implementation are probably bugs in design.

---

# A Philosophical Dichotomy

As programmers we each fall into two roles:

- users of libraries
- writers of libraries

These roles come with drastically different philosophies.

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

. . .

Users want *libraries.*

Writers provide *macros.*

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

# Libraries vs Macros

Macros:

- Automate tedious tasks.

. . .

- Are explicitly about "how", never "why"

. . .

- Present pre-baked recipes; hope your problem can be solved by one.

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

&nbsp;

. . .

Definitely a *macro* invocation!

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

**I guess I'll write my own.**

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

# WE FOOLED OURSELVES

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

```haskell
fill :: Rect -> a -> QT a -> QT a

forall r a q.
  ⟦ fill r a q ⟧ = \p ->
    if contains r p
      then a
      else ⟦ q ⟧ p
```

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

# Addressing Asymmetries

```haskell
hitTest :: QT a -> Rect -> Bool
```

Needs an update.

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

# Intermission

---

# Remaining Questions

- What is `Point`?

. . .

- How do we actually implement this?

. . .


The answers to these questions inform one another.

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

We have implemented a data structure in turns of the Functional Programming
Pantheon.

. . .

It can be tempting to say our work here is done.

. . .

But, it doesn't correspond at all with our desired API or semantics.

---

**This is dangerously persuasive.**

If we were writing a macro; we'd be done at this point.

. . .

Hackage stops here ± some helpers

---

# Implementation

Desired API:

```haskell
data QT a
instance Applicative QT
instance Monoid QT

rect    :: a -> a -> Rect -> QT a
get     :: Point -> QT a -> a
hitTest :: Monoid m => (a -> m) -> QT a -> Rect -> m
```

Contrast against:

```haskell
data QuadTree a = Fill a | Split (Quad (QuadTree a))
instance Monad QuadTree
instance Monoid QuadTree
```

. . .

Biggest problem: `QuadTree` has no notion of space.

---

# Implementation

Maybe parameterize `QuadTree` on the rectangle it bounds; enforce this as an
invariant:

```haskell
getImpl     :: Point -> QT (Rect, a) -> a
hitTestImpl :: Monoid m => (a -> m) -> QT (Rect, a) -> Rect -> m
```

. . .

Problem: **all spatial data is at the leafs.**

---

# Implementation

Instead, compose with the `(,) Rect` outside:

```haskell
getImpl     :: Point -> (Rect, QT a) -> a
hitTestImpl :: Monoid m => (a -> m) -> (Rect, QT a) -> Rect -> m
```

. . .

Maybe this is our wrapper type:

```haskell
data QT a = QT
  { qt_bounds :: Rect
  , qt_tree   :: QuadTree a
  }
```

. . .

What if we lookup a point **outside of bounds?**

---

# Implementation

```haskell
data QT a = QT
  { qt_bounds     :: Rect
  , qt_tree       :: QuadTree a
  }
```

---

# Implementation

```haskell
data QT a = QT
  { qt_bounds     :: Rect
  , qt_tree       :: QuadTree a
  , qt_everywhere :: a
  }
```

. . .

Getting an `Applicative` instance is hard.

---

# Applicatives

*Fact:*

. . .

> products of "monoidal" types have obvious applicative instances.

. . .

```haskell
data Ex a = Ex1
  { foo :: [a]        -- has applicative
  , bar :: State s a  -- has applicative
  , qux :: Sum Int    -- has monoid
  }
```

---

# Applicatives

*Fact:*

> products of "monoidal" types have obvious applicative instances.

```haskell
data Ex a = Ex1
  { foo :: [a]        -- has applicative
  , bar :: State s a  -- has applicative
  , qux :: Sum Int    -- has monoid
  }

instance Applicative Ex where
  pure a = Ex (pure a)  -- use applicative
              (pure a)  -- use applicative
              mempty    -- use monoid
  Ex fx fy fz <*> Ex ax ay az =
    Ex (fx <*> ax)  -- use applicative
       (fy <*> ay)  -- use applicative
       (fz <> az)   -- use monoid
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

