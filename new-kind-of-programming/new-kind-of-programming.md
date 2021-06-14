---
title: "The Talk on Types"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 3
    left: 5
    right: 5
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
fmap f (Foo b a fsa lia) = Foo b (f a) (f . fsa) (fmap (fmap f) lia)
```

-
  - this is a type you've never seen before, because i just made it up
  - nevertheless, despite the fact that you've never seen it before, you know that the following is its functor instance:
    - describe our hole 
  - how do you know that?
    - maybe you'll say something like "theorems for free!" citing wadler
    - or "naturality"
    - or "it's the only obvious thing that would typecheck"
      - (not true, you can swap the bool)
    - but neither of these is truly the algorithm your brain is doing
      - i promise you not a single person in the audience just worked out a commutativity square in their heads, and then said "ah yes, this satisfies all of the necessary equalities"
  - no, that's not what you're doing!
  - instead, we're all running on mental-pattern-matching here
    - we know what a functor looks like, and can quickly look to see "yeah, that looks like a functor"
    - armed with the knowledge that functors instances are unique
      - it's trivial to say "yes, that's a functor"
  - so my question to you is:
    - what is the explicit algorithm your brain is running to check if this thing is a functor?
    - does it help if i write it like this?
      - fmap f (Foo b a fsa lia) = Foo b (f a) (fmap f fsa) (fmap (fmap f) lia)
- what's really going on here is actually the composition of a few rules
  - semantically the idea is "fmap should touch only `a` variables _AND NOTHING ELSE_"
  - consequence:
    - we must fmap a particular data constructor to the same data constructor
    - fmap f Foo{..} = Foo {..}
  - consequence:
    - if a term doesn't mention `a`, we must fill it in without changes
    - fmap f (Foo b _ _ _) = Foo b
  - HIDDEN SEMANTIC IDEA:
    - if we modify an `a`, we MAY ONLY MODIFY IT VIA `f`
  - consequence:
    - if a term is `a`, we must replace it with `f a`
  - what do we do about the other case? add an `fmap` and try again!
- having worked out these rules is not just informative, but actually useful too!
  - rather than running them "forwards" in "pattern matching mode"
  - we can run them "backwards" in "synthesizing mode"
- all we need to do is assign meaningful operations over the AST for each action, and a description of a functor can be sufficient to synthesize a functor instance
  - what does this even mean?
  - at every hole we have a judgment
  - consisting of a goal and a hypothesis
  - the goal is the type we're currently trying to synthesize
  - and the hypothesis is everything we have in scope at our disposal
- so for example, we can write some beginner tactics
  - assume <x> : look for something called x in the hypothesis, and use it, if it can unify with the goal type
    - not a great tactic; takes strictly more typing than just typing in the thing you want
  - intros [<x>, <y>...]
    - if the goal type is a function, then bind every function argument in a lambda expression bind
    - produce a NEW judgment, with the resulting type, whose hypothesis contains the introduced terms
    - as you can see --- the hypothesis is a dynamic sort of thing; it changes throughout the lifetime of the tactic search
  - destruct <x>
    - lookup x in the hypothesis, and write a case expression
    - for every data constructor, produce a new judgment.
      - each with the same goal
      - but each having different hypotheses
  - better: assumption
    - use SOMETHING in the hypothesis
    - what if there are multiple things? no unique answer to this; wingman will try to tie-break based on linearity and other heuristics
- in a sense, a tactic is a composable, semantic-aware autocomplete function
- so let's rethink about what tactics we would need to use to write a functor instance
  - intros f x
    - bind the function and argument
  - homo x
    - case split it, use the same data con on the other side
  - now, FOR EVERY RESULTING HOLE, we want to attempt to fill it in via:
    - either:
      - use an assumption.
        - due to the type of fmap, the tactic search will fail if it tries to assume something of type `a` for the new type `b`
        - therefore this is guaranteed to fill in variables that were unpacked but which do not reference a
      - apply `f`, then use an assumption
        - if the type is `b`, this will let us call `f`only
        - then we must have had an `a` in scope
        - so the assumption will work
      - use `fmap`, some function, and then an assumption
        - what function is it?
        - it's either `f`
        - or we can use fmap again
- that's it! that's how we can synthesize fmap
- so let's try it!
  - <write the code>
- you'll notice this is about the same effort as having written the instance by hand
  - except that this is the SAME program for every functor instance
  - which means we need write it only once
  - some clever library author can write it for us, and we never need to think about functors ever again
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

