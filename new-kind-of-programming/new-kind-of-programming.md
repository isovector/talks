---
title: "A New Kind of Programming"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 3
    left: 5
    right: 5
  slideLevel: 1
---

# A New Kind of Programming

## Tactic Metaprogramming in Haskell

---

* **Sandy Maguire**
* sandy@sandymaguire.me

. . .

* reasonablypolymorphic.com
* github.com/isovector
* leanpub.com/thinking-with-types
* leanpub.com/algebra-driven-design

. . .

Today's slides:

* tinyurl.com/new-kind-of-programming

---

## High-Level Reasoning

Here's a type.

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]
```

. . .

Here's its functor instance.

```haskell
fmap f (Foo b a fsa lia) =
  Foo b (f a) (f . fsa) (fmap (fmap f) lia)
```

. . .

Is it right?

. . .

How do you know?

---

## How do you know?

Maybe...

. . .

> "theorems for free"

. . .

> "naturality"

. . .

or maybe

. . .

> "it's the only thing that would typecheck"

---

## How do you know?

These are all true¹, but *none of them is what your brain is actually doing.*

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

¹ sorta

---

<!--
I promise you not a single person in the audience just worked out a commutativity square in their heads, and then said "ah yes, this satisfies all of the necessary equalities"

no, that's not what you're doing!

instead, we're all running on mental-pattern-matching here
-->

## How do you know?


Your brain is not working out a commutativity square.

. . .

You are mentally *pattern-matching* against *what a functor instance looks like.*

---

<!--

we know what a functor looks like, and can quickly look to see "yeah, that looks like a functor"

it might not be entirely clear how to articulate the shape of a functor

but regardless, just like justice potter, we "know it when we see it"

additionally, in Haskell, there is only ever one unique implementation of a functor
so, if something looks like a functor. that's enough. it can't be "misleading"
in any real sense

-->

## How do you know?

1. Functors have a distinctive "shape."

2. A functor instance in Haskell is unique up to isomorphism.

---

<!--

So my question to you is this: how does that mental pattern matching actually
work? What work is it doing? Take a look again, and spend a few seconds trying
to determine what are the salient features.

<pause>

What if I write the nested fmaps like this instead?

-->

## How do you know?

What algorithm is your brain running when it determines this is a functor?

```haskell
fmap f (Foo b a fsa lia) =
  Foo b (f a) (f . fsa) (fmap (fmap f) lia)
```

---

<!--

Maybe that's a little clearer. What if we rewrite the function composition too?

-->

## How do you know?

What algorithm is your brain running when it determines this is a functor?

```haskell
fmap f (Foo b a fsa lia) =
  Foo b (f a) (f . fsa) ((fmap . fmap) f lia)
```

---

## How do you know?

What algorithm is your brain running when it determines this is a functor?

```haskell
fmap f (Foo b a fsa lia) =
  Foo b (f a) (fmap f fsa) ((fmap . fmap) f lia)
```

&nbsp;

recall:

&nbsp;

```haskell
instance Functor ((->) r) where
  fmap = (.)
```

---

## How do you know?

We spot functor instances via the composition of a few rules:

1. `fmap` should touch only `a` variables, and *do nothing else*

    . . .

    **Consequence:** `fmap` cannot change the data constructor.

    ```haskell
    fmap f Foo{..} = Foo ...
    ```

    . . .

    &nbsp;

    **Consequence:** If a term doesn't mention `a`, we must fill it in without
    changes.

---

## How do you know?

We spot functor instances via the composition of a few rules:

1. `fmap` should touch only `a` variables, and *do nothing else*

2. `fmap` may produce `b` terms *only* by applying `f`

    . . .

    **Consequence:** If a term is `a`, we *must* replace it with `f a`

---

## How do you know?

We spot functor instances via the composition of a few rules:

1. `fmap` should touch only `a` variables, and *do nothing else*

2. `fmap` may produce `b` terms *only* by applying `f`

3. `fmap` can only change terms that *mention* `a` via `fmap`. *This might need to be recursive.*

    . . .

    **Consequence:** To replace a term that mentions `a`:
      * Stick in an `fmap _`

      . . .

      * Fill the hole using rules 2 and 3

---

## How do you know?

Putting these rules together, a functor instance is the only that will
type-check under the following circumstances:

. . .

1. Destruct the mapped term, produce the same data constructor that you
   unpacked, with a hole for each argument.

    . . .

2. For each hole, consider the term in the same position from the data constructor.

    . . .

3. Fill the hole using whichever of the following typechecks:

    . . .

    * Use the term itself

    . . .

    * Apply `f` to the term

    . . .

    * Apply `(fmap . fmap ...) f` to the term

---

## A New Vocabulary

This analysis is "above" the level of syntax.

. . .

These operations are *semantic* and *conceptual* descriptions of how the program
was written.

<!--------------------------------------

I like to call it as "the way you'd guide a junior programmer through the
implementation."

It's a lot like being the navigator in a pair programming session.

-->

---

## A New Vocabulary

Why limit ourselves to *analyzing* code with these patterns we've identified?

. . .

**Why not synthesize it?**

---

## A New Vocabulary

Just like using typed holes in GHC, we can consider a program as a *collection of
judgments.*

. . .

Each judgment consists of:

. . .

- A **goal**: the type of value to fill the hole

. . .

- A **hypothesis**: free variables we can use to synthesize a term

    . . .

    (not necessarily the same as the terms in scope)

<!--

We'll see examples of how restricting the hypothesis can be useful later.

-->

---

## A New Vocabulary

Game plan:

1. Assign meaningful *semantic* operations over the AST

    . . .

2. Turn our description of a functor into a functor *synthesizer*

---

## A New Vocabulary

From earlier:

&nbsp;

> Fill the hole using whichever of the following typechecks:
>
> * Use the term itself
> * Apply `f` to the term
> * Apply `(fmap . fmap ...) f` to the term

---

## A New Vocabulary

From earlier:

&nbsp;

> Fill the hole using whichever of the following typechecks:
>
> * **Use the term itself**
> * Apply `f` to the term
> * Apply `(fmap . fmap ...) f` to the term


<!--

How can we implement this?

-->

---

## A New Vocabulary

A very simple algorithm:

1. For every term in the **hypothesis**:

    . . .

2. See if it can unify with the **goal**

    . . .

3. If so, use it!

. . .

Let's call it *assumption*.

<!--

Important to note that this uses ANYTHING in the hypothesis. You don't need to
refer to anything by name.

-->

---

## A New Vocabulary

For example, given a judgment:

* **Goal**: `Bool`
* **Hypothesis**:
    * `error_msg :: String`
    * `func :: a -> b `
    * `should_check :: Bool`

The result of *assumption* will be what?

. . .

> `should_check`

---

## A New Vocabulary

> Fill the hole using whichever of the following typechecks:
>
> * Use the term itself
> * **Apply `f` to the term**
> * Apply `(fmap . fmap ...) f` to the term

. . .

&nbsp;

New tactic, *apply*:

* One argument: the name of a function in the **hypothesis**
* Attempts to unify the result of the function with the **goal**
* If successful, create a new **judgment** for each argument to the function

---

## A New Vocabulary

For example, given the judgment:

* **Goal**: `Int`
* **Hypothesis**:
    * `wurble :: String -> Bool -> Int`

Running *assumption wurble* will result in:

. . .

> `wurble _1 _2`

&nbsp;

. . .

where `_1` has the judgment:

* **Goal**: `String`
* **Hypothesis**:
    * `wurble :: String -> Bool -> Int`

. . .

What judgment does `_2` have?

<!--

Notice here that we've split one judgment into two.
They share the same hypothesis, but the goal has changed, as you might expect,
in order to actually get this thing to typecheck.

-->

---

## A New Vocabulary

One more, to get a feeling for it.

&nbsp;

*intros*: create a lambda expression

. . .

1. Given a judgment whose **goal** is a function type:

    . . .

2. Build a lambda expression, binding each variable

    . . .

3. Add every bound variable to the **hypothesis**

    . . .

4. Make a new **judgment** for the result of the function

---

## A New Vocabulary

For example:


* **Goal**: `[a] -> Bool -> c`
* **Hypothesis**:
    * `x :: String`

&nbsp;

. . .

Running *intros* will produce:

```haskell
\as bool -> _
```

&nbsp;

. . .

where `_` has judgment:

* **Goal**: `c`
* **Hypothesis**:
    * `as :: [a]`
    * `bool :: Bool`
    * `x :: String`

---

## A New Vocabulary

Notice that *intros* came up with reasonable variable names!

. . .

Not always what you want:

* *assumption* can still find them

    . . .

* But *apply* can't!

. . .

&nbsp;

Instead, we can pass optional names to *intros* to pick the names it binds.

---

## A New Vocabulary

The same example again:


* **Goal**: `[a] -> Bool -> c`
* **Hypothesis**:
    * `x :: String`

&nbsp;

Running *intros foo bar* will instead produce:

. . .

```haskell
\foo bar -> _
```

---

## Constructing Programs

Tactics are composable, semantic-aware autocomplete.

So let's use them to write some code.

---

## Constructing Programs

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]

instance Functor (Foo s) where
  fmap = (_ :: (a -> b) -> Foo a -> Foo b)
```

. . .

Use *intros f x* to bind the lambda.

---

## Constructing Programs

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]

instance Functor (Foo s) where
  fmap f x = (_ :: Foo s b)
```

. . .

We can case-split on `x` via *homo x*:

---

## Constructing Programs

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]

instance Functor (Foo s) where
  fmap f (Foo bool a fsa lia) =
    Foo
      (_1 :: Bool)
      (_2 :: b)
      (_3 :: s -> b)
      (_4 :: [(Int, b)])
```

. . .

We can finish the first judgment via *assumption*:

---

## Constructing Programs

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]

instance Functor (Foo s) where
  fmap f (Foo bool a fsa lia) =
    Foo
      bool
      (_2 :: b)
      (_3 :: s -> b)
      (_4 :: [(Int, b)])
```

. . .

Because the second goal is a `b`, we know we need to *apply f*:

---

## Constructing Programs

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]

instance Functor (Foo s) where
  fmap f (Foo bool a fsa lia) =
    Foo
      bool
      (f (_1 :: a))
      (_3 :: s -> b)
      (_4 :: [(Int, b)])
```

. . .

And now we can use *assumption* to fill in `_1`:

---

## Constructing Programs

```haskell
data Foo s a = Foo Bool a (s -> a) [(Int, a)]

instance Functor (Foo s) where
  fmap f (Foo bool a fsa lia) =
    Foo
      bool
      (f a)
      (_3 :: s -> b)
      (_4 :: [(Int, b)])
```

and so on and so forth.

---

## Constructing Programs

At the end of the day, our program looks like this:

```
intros f x,
homo x,
assumption,
apply f,
assumption
use fmap,
assumption,
assumption,
use fmap,
intros x,
use fmap,
assumption,
assumption,
assumption
```

. . .

Gross.

---

## Constructing Programs

What went wrong?

We wrote a bespoke subroutine for each hole.

---

## Constructing Programs

But our algorithm said:

> **For each hole**, consider the term in the same position from the data constructor.
>
> Fill the hole **using whichever of the following typechecks**

---

## Constructing Programs

Combinators to the rescue!

. . .

We need the ability to run a tactic over every judgment.

&nbsp;

. . .

Use separators:

- `,` --- run the subsequent tactic on the *first* judgment

- `;` --- run the subsequent tactic on *all* judgments

---

## Constructing Programs

Also we need the ability to branch.

Separate simultaneous tactics via `|`.

---

## Constructing Programs

Our functor instance is now significantly terser:

```
intros f x,
homo x;
( assumption
| (apply f, assumption)
| (*use fmap; assumption)
)
```

85 bytes, compared to 76 for the original instance.

. . .

&nbsp;

*Same amount of work as the original instance,* but this will derive any instance
for us.

&nbsp;

. . .

**And we only need to write this once.**

<!--
- you'll notice this is about the same effort as having written the instance by hand
  - except that this is the SAME program for every functor instance
  - which means we need write it only once
  - some clever library author can write it for us, and we never need to think about functors ever again
-->

---

  - destruct <x>
    - lookup x in the hypothesis, and write a case expression
    - for every data constructor, produce a new judgment.
      - each with the same goal
      - but each having different hypotheses
  - better: assumption
    - use SOMETHING in the hypothesis
    - what if there are multiple things? no unique answer to this; wingman will try to tie-break based on linearity and other heuristics
- in a sense, a tactic is a composable, semantic-aware autocomplete function
- what really got me thinking about this sort of code synthesis was the all-too-common of a pairwise semigroup
  - Blah a1 b1 c1 <> Blah a2 b2 c3 = Blah (a1 <> a2) (b1 <> b2) (c1 <> c2)
  - this thing comes up ALL THE TIME. SO OFTEN
    - why the hell do i need to write this?
    - if i were describing this to a collegue i'd say
    - "its semigroup instance is homomorphic"
    - or "it's a semigroup pointwise"
    - WAY EASIER THAN TYPING
  - how would we make this?
    - intros x1 x2,
    - destruct x1;
    - homo x2
  - and, well, now what? we'd like to be able to restrict the hypothesis in our hole
  - we can do it via `pointwise`
    - which filters out any arguments that didn't come from the same context you're in
      - IMPL NOTE -- need to track this!!
    - now it's just what we know
      - use <>;
      - assumption
  - et voila
  - same order of magnitude as writing it by hand
    - but again, like functor, this is completely reusable
    - it will automatically scale to the correct arity
- one last thing
  - sometimes you need to change your thinking pattern to really capture what it is you're trying to accomplish
    - this is the hard part
    - and it's where your input is the most valuable
  - let's say we want to write a GHC.Generics implementation to serialize something to disk
  - if you're not familiar, the idea is that we need to abuse the typeclass machinery for several unrelated types
    - we write one class for the operation we want to perform
    - and then we write lots of instances
    - for V1, U1, K1, M1, + and *
    - each of these corresponds to the fundamental building blocks of types
      - but in 90% of the generics code, the interesting case is K1
      - everything else just needs to route your code to get there
    - sounds like a good example to TACTICS
  - but this is a hard case.
    - for every type, we have a different number of arguments
    - we want to recurse on them
    - and then combine them in different ways
  - there is not a lot of syntactic structure here to work with
  - rather than work at the level of syntax, let's instead study what our goal is
    - for every generic constructor, we need to know how to serialize its children
    - and then find a way of combining those serializations togethre
  - this sounds like a CATAMORPHISM
    - using `cata` will let us destruct a term, and recurse on every introduced value
    - and then how do we put them together?
    - it turns out the operation we want here is "stick EVERYTHING together of the same type"
      - i call it "collapse"
  - so our program is
    - intros x
    - cata x
    - collapse
  - this just leaves a hole for how we'd like to combine our subterms
  - wham, bam, thank you maam
    - a huge amount of boilerplate was just automated away for us
    - allowing us to focus on the interesting parts of the computation

- "Working with the type system is a very interactive and enriching activity, we learn from it, and we constantly go back to improve things, some of which we will understand and remember the next time we find ourselves solving a similar problem. The type checker is an excellent teacher and companion; we try one approach and fail, we try another approach and fail again but this time understanding why, then we succeed but later realize that maybe if we had done something a bit differently we could have covered a larger space and solved more problems, so we go back and fail again until eventually we don’t, and then we have learned something. Throughout this process we gain some understanding about which things are worth doing and which aren’t, and from there we make informed choices."

