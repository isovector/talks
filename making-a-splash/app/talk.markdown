---
title: "Making a Splash"
author: "Sandy Maguire"
url: "https://reasonablypolymorphic.com/making-a-splash"
---

# Making a Splash


##

"I'm not \$FAMOUS_PERSON, so who am I to work on this kind of thing?"

On the other hand, who is \$FAMOUS_PERSON other than someone who got famous
doing this kind of thing?


## Lots of Free Monads

Historically hard to get right.

| Package         | Boilerplate?    | Higher Order? | Performance?     |
| ---------       | ------------    | ------------- | ---------------- |
| `free`          | Some            | No            | $O(n^2)$         |
| `freer`         | Almost Zero     | No            | $700 \times$     |
| `capacity`      | Significant     | No            | $1 \times$      |
| `fused-effects` | UNGODLY AMOUNTS | Yes           | $1 \times$      |


## Assumed Impossible

> My conjecture is that while we could very likely relate effects to handlers
> without all the [boilerplate] --- we would lose a lot of performance.
>
> I’m doubtful that we could ever achieve the constant-time
> Writer implementation ...  that’s a direct result of GHC being able to
> specialize, inline, and optimize the whole chain.


## Introducing Polysemy

> The coexistence of many possible meanings for a word or phrase.

The hat-trick: low boilerplate, higher order, crazy fast.


## Introducing Polysemy

| Package         | Boilerplate?    | Higher Order? | Performance?     |
| ---------       | ------------    | ------------- | ---------------- |
| `polysemy`      | Some            | Yes           | $1 \times$       |
| `free`          | Some            | No            | $O(n^2)$         |
| `freer`         | Almost Zero     | No            | $700 \times$     |
| `capacity`      | Significant     | No            | $1 \times$      |
| `fused-effects` | UNGODLY AMOUNTS | Yes           | $1 \times$      |


## Origins

My company had a bunch of spaghetti code and no real testing strategy.

It kept biting us in the ass.

I was tasked to come up with an alternative.


## Free Monads to the Rescue!

Briefly:

* Write a custom, very-high-level DSL for every problem.
* Evaluate the DSL through a series of "compiler" passes.


## High Level DSLs

Most problems are easy to describe, but hard to solve.

Separate the description from the implementation.

Non-specialists can write the business logic, and leave the implementation
details to the programmers.


## Timeline

* 2017-01: First deployment of service using `freer-effects`.
* 2017-02: Start refactoring other services to use same.
* 2017-05: Project is canceled, citing "it will be too slow."


## "It Will Be Too Slow"

The project lead pulled me aside.

Getting this far took me about two years.

Most of that was wasted time.

