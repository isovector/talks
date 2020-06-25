---
title: "Algebra-Driven Design"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 3
    left: 5
    right: 5
---

# Algebra-Driven Design

---

* **Sandy Maguire**
* sandy@sandymaguire.me

. . .

* reasonablypolymorphic.com
* github.com/isovector
* patreon.com/algebra_driven_design

. . .

Today's slides:

* github.com/isovector/talks/blob/master/algebra/algebra.md

---

## Coming Up

- Our industry *fetishizes algorithms and data structures*, to the detriment of
    maintainability and comprehensibility.

. . .

- Thinking about our *APIs as mathematical objects* is a critical way of
    discovering desirable semantics.

. . .

- Compositional semantics are invaluable not only for our users, but also *help
    us as implementers*.

---

## Unquestioned Assumptions

- Software is hard to write. Our brains are simply too small to understand an
    entire codebase simultaneously.

. . .

- Complexity accumulates over time. There is rarely a PR that changes a codebase
    from simple to complex.

. . .

- "Interesting" bugs result from unforeseen interactions between different
    components.

. . .

- Software is a tool for thought. A messy interface is indicative of a messy
    mind.

. . .

- We can completely isolate the *design of a system from its implementation.*

. . .

&nbsp;

> *If you disagree, please suspend your disbelief.*

. . .

> *(but you're wrong)*

---

## How Most Software Seems to Get Written

> "I have a great idea for a new feature. I'm going to go implement it."
>
> --J. Random Hacker

. . .

&nbsp;

There is no, if any, thought put into "**what mental models does this software
afford me?**"

---

## The Next 700 Programming Languages

Peter Landin defines a **denotational design** as follows:

. . .

* each expression has a *nesting subexpression structure*

. . .

* each *subexpression denotes something* (usually a number, truth value or numerical function)

. . .

* the thing an expression denotes, i.e., its "value", *depends only on the
  values of its sub-expressions,* not on other properties of them

. . .

&nbsp;

These are particularly valuable properties for human understanding.

A denotational design allows us to **compose understanding.** There is nothing
to keep in mind other than the individual meanings of the pieces.

---

## Properties of Denotational Designs

Implied by the definition of a denotational design:

. . .

- every syntactically valid expression is meaningful

. . .

- the meaning of an expression doesn't change depending on where it is, or how
    many times it's present

. . .

- we can freely combine programs, and the meanings compose

. . .

- *a mandated separation between describing what you want, and actually running
    it*

---

## Non-compositional Software is Everywhere :(

Every time you see:

* immediately executed IO (no separation between building and executing)

. . .

* mutability (values don't depend only on subexpressions)

. . .

* mandated order of function calls (every syntactically valid program is
    meanignful)

. . .

* mutually exclusive options (understanding composes)

---

## The Value of Denotational Designs

- Turns complicated problems of semantics into data modeling problems.

. . .

- Statically analyzable (no Gödelian issues)

. . .

- Easy to test, due to separating concerns of "what to do" from "how to do it."

. . .

- So easy to reason about that it can be *automated.*

---

## In Practice

Let's build a regex engine.

**Question:** what is the "meaning" of a regular expression?

. . .

&nbsp;

**Answer:** the meaning of anything is "*what do we want to be able to do with it?*"

In this case, given a string, get back all possible ways it can be matched.

---

## Regex

```haskell
μ Regex = String -> [String]
```

Why this meaning?

It allows us to compose the meanings of multiple regexes.

. . .

Nothing prevents us from also returning pattern matches. I just didn't implement
it.

. . .

&nbsp;

For example, the concatenation regex of matching regex one after another.

```haskell
cat :: Regex -> Regex -> Regex
```

```haskell
μ (cat rx1 rx2) = λ str. flatMap (μ rx2) ((μ rx1) str)
```

**This is not an implementation!**

---

## Regex

Without talking about implementation, in what ways can we build a regular
expression?

> `char :: Char -> Regex`{.haskell} (eg. */a/*)

. . .

> `anyChar :: Regex`{.haskell} (eg. */./*)

. . .

> `cat :: Regex -> Regex -> Regex`{.haskell} (eg. */ab/*)

. . .

> `optional :: Regex -> Regex`{.haskell} (eg. */a?/*)

. . .

> `either :: Regex -> Regex -> Regex`{.haskell} (eg. */a|b/*)

. . .

> `star :: Regex -> Regex`{.haskell} (eg. */a\*/*)

. . .

Operators like */[ab]/*, */a{1,3}/*, and */a+/* can be implemented in terms of
these.

---

## Regex

What about */^a/* and */a$/*?

. . .

These are non-compositional --- */(a$)b/* is meaningless.

. . .

Just because they're useful doesn't mean they necessarily have a place in our
algebra.

. . .

**Spoilers:** we will later find a better way of expressing these operators.

---

## Regex

Is our API for regexes complete?  What does that mean?

. . .

Are we missing any operators that should be there?

---

## Reasoning Algebraically

The `cat` operation satisfies the associative property:

```haskell
cat a (cat b c) = cat (cat a b) c
```

We call such a thing a *semigroup.*

. . .

&nbsp;

Where there's a semigroup, there's usually a *monoid*.

. . .

&nbsp;

Is there a special regex `E` such that:

```haskell
cat a E = a = cat E a
```
. . .

```haskell
emptyMatch :: Regex
```

---

## Reasoning Algebraically

`either` is also associative:

```haskell
either a (either b c) = either (either a b) c
```

Does it have a monoid?

. . .

&nbsp;

```haskell
fail :: Regex
```

The regex that never matches anything!

Why is this valuable? It's like 0.

---

## Reasoning Algebraically

Just `either`? Why not `both`?

```haskell
both :: Regex -> Regex -> Regex
```

Semantically, this should match a string only if both of its sub-regexes match
it.

Use the *longer match* if they match different lengths of strings.

. . .

&nbsp;

Also associative. Also forms a monoid under `emptyMatch`

. . .

&nbsp;

Probably requires two traversals of the string. Won't that be slow?

**Don't let potential performance anxiety get in the way of a good design.**

---

## Reasoning Algebraically

How do we not match a string with a regex?

*/[^h][^e][^l][^l][^o]/*

. . .

Disgusting.

. . .

Instead:

```haskell
negative :: Regex -> Regex
```

and

```haskell
negative (string "hello")
```

. . .

&nbsp;

What are the semantics?

---

## Reasoning Algebraically

```haskell
negative :: Regex -> Regex
```

The denotation to the rescue!

```haskell
μ (negative r) = λ str.
  case (μ r) str of
    [] -> [str]  -- Don't consume any input!
    _  -> []     -- Fail if the subexpr succeeded.
```

---

## Resolving Old Issues

We can now meaningfully describe */^a/* as:

```haskell
first :: Regex -> Regex
first a = both a (negative (cat anyChar a))
```

. . .

and */a$/*:

```haskell
last :: Regex -> Regex
last a = both a (negative (cat a anyChar))
```

&nbsp;

Fully compositional!

---

## The Power of Algebra

Look how much we discovered about our problem domain. We haven't even written
any code yet!

*We never would have come up with these things if we were primarily concerned
with how to implement them.*

. . .

&nbsp;

>     Let the implementation stem from what you're building, rather than building
>     around the implementation!

---

## A Quick Look at QuickSpec


