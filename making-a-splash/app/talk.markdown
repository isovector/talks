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




---

* stubbornness is a virtue
* at the end of the day, the computer is running EXACTLY THE SAME PROGRAM
    * it's expressed differently
    * but everything is still statically known
    * in principle they should be the same!
* so why can't it be as fast as the hand-rolled version?
    * either the library is too dumb
    * or the compiler is too dumb
* i got stupid lucky
    * when you're doing novel things, you're going to run into weird corner
        cases
    * here the compiler would optimize my program when written in one specific
        way
    * i found this accidentally

```haskell
newtype Free f a = Free
  { runFree :: ∀ m. Monad m => (∀ x. f x -> m x) -> m a
  }

fast :: Free f a -> Free g a
fast (Free m) = Free $ \k -> m $ \fx ->
  ...

slow :: Free f a -> Free g a
slow (Free m) = m $ \fx ->
  ...
```

* it's fast if we do the "dumb" thing of unwrapping and then rewrapping a Free
* but it's slow if we just let `m ~ Free g`
* conceptually identical! but VERY DIFFERENT to the compiler
* as it happens, most compiler's optimizations are ad-hoc.
    * usually more empirical than grounded in theory
    * and that's fine! usually they work
* but if you're working on something that DEPENDS on an optimization, your best
    bet is to understand that optimization
    * in general, understanding is better than not

> I wanted to understand why this limitation was there. Then learning the answer
> revealed that it's actually something that can be fixed --- as is always the
> case with these things if you think about them enough.
>
> -Csongor Kiss

* if you're anything like me, you probably run into impostor syndrome all the time
* in this case i was unconsciously flinching away from looking at the compiler
    * it's big and scary.
    * it's about as old as i am.
* the guys who work on the compiler are all legends.
    * who am i to be in their midst?
* but here's the thing. it sounds obvious, but it REALLY WASN'T
    * these guys _became_ legends by working on the compiler
    * the legendary status came after.
    * at first they were just guys who settled in to make a compiler
* this is one hell of a relief
    * you can just do work. you don't need to be brilliant.

> None of these seems special to me. Anyone could’ve [done X]; anyone could’ve
> [done Y]. If I have done anything meritorious with them, it was perhaps simply
> putting more work into them than someone else would have.



> I think you’ll see what I mean if I teach you a few principles magicians
> employ when they want to alter your perceptions…Make the secret a lot more
> trouble than the trick seems worth. You will be fooled by a trick if it
> involves more time, money and practice than you (or any other sane onlooker)
> would be willing to invest.
>
> My partner, Penn, and I once produced 500 live cockroaches from a top hat on
> the desk of talk-show host David Letterman. To prepare this took weeks. We
> hired an entomologist who provided slow-moving, camera-friendly cockroaches
> (the kind from under your stove don’t hang around for close-ups) and taught us
> to pick the bugs up without screaming like preadolescent girls. Then we built
> a secret compartment out of foam-core (one of the few materials cockroaches
> can’t cling to) and worked out a devious routine for sneaking the compartment
> into the hat. More trouble than the trick was worth? To you, probably. But not
> to magicians.

> EVERY ONE OF TODAY’S most famous and familiar ideas was once unknown and
> unsuspected...  Contrarian thinking doesn’t make any sense unless the world
> still has secrets left to give up.

* so what's the secret here?
* start paying attention to when you think "i wonder why"
    * think about the answer. do you know /can you work out the answer?
    * if the answer isn't motivated from first principles, *it's not a real
        answer*
    * if the answer is "because \$AUTHORITY said it can't be done" that's stupid
* this process lets you find the frontier of what's possible today
* once you know where that frontier is, the solution is to just learn enough to
    answer "why" for yourself
* and at that point, the solution will probably be apparent.
    * or if not, at least you learned a lot in the process
    * "losing is fun" aka "having your solution not work is still learning"

