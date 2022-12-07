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

TODO: what is a quadtree

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
data Quad a = Quad a a
                   a a
  deriving instance Functor
```

```haskell
instance Applicative Quad where
  pure a = Quad a a a a
  Quad f1 f2
       f3 f4 <*> Quad a1 a2
                      a3 a4 =
    Quad (f1 a1) (f2 a2)
         (f3 a3) (f4 a4)
```

---

# Implementation

```haskell
data Tree a
  = Fill a
  | Split (Tree a) (Tree a)
          (Tree a) (Tree a)
```

. . .

Why not:

```haskell
data Tree a
  = Fill a
  | Split (Quad (Tree a))
```

. . .

From the zoo:

```haskell
type Tree = Free Quad
```

This is the free monad on `Quad`s!

. . .

```haskell
deriving
  via (Free Quad)
  instance (Functor, Applicative, Monad) Tree
```


