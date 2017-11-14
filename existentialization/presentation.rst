:title: Existentialization
:data-transition-duration: 150

:css: fonts.css
:css: presentation.css



















----

:id: title

.. raw:: html

  <script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>

  <h1>Existentialization</h1>
  <h2>What Is It Good For?</h2>
  <h3>A talk by <span>Sandy Maguire</span></h3>
  <h4>reasonablypolymorphic.com</h4>

----

Slides available.
=================

.. raw:: html

  <pre>
  <h3>reasonablypolymorphic.com/existentialization</h3>

  </pre>


----

th-dict-discovery
=================

- This title is a little misleading
  - it's actually more
  - how can we implement my th-dict-discovery library
  - to get from here to there we're going to need to go through existentialization
  - so it hasn't been false advertising
  - but you're actually going to get MORE THAN YOU WERE BARGAINING FOR

----

The Problem
===========

- We would like to be able to quickcheck properties for instances we write
- wouldn't it be nice to have tests that AUTOMATICALLY get generated?
- like, every monad instance i write is actually a monad?? very cool

----

Heterogeneous Lists
===================

  - let's say we are javascript programmers and for some reason we want to shove any type we want into a list
  - haskell doesn't let us do this:
    - [1, 2] :: [Int]
    - [True] :: [Bool]
    - [1, True] :: type error, mafucka

----

Heterogeneous Lists
===================

  - but WDGAF. even though this isn't actually useful in any way, we want to do it anyway
  - how can we? is haskell suchh a shit language that it can't express this?
  - no! we can do it

----

The Any Type
============

.. code:: haskell

  data Any where
    Any :: a -> Any


  - here any can be thought of as a container
    - we can stuff any type we want into it, and get back a value of type any
    - this any contains the `a`
    - but we say it is now existential
    - as in, we know it EXISTS, but we don't know what it is

----

RIGID SKOLEMS
=============


  - so what happens if we try it?

.. code:: haskell

  f (Any a) = a


    - • Couldn't match expected type ‘t’ with actual type ‘a’
      - because type variable ‘a’ would escape its scope
      - This (rigid, skolem) type variable is bound by
        - a pattern with constructor: Any :: forall a. a -> Any,

----

RIGID SKOLEMS
=============

.. code:: haskell

  f :: Any -> a
  f (Any a) = a


  - hmm. let's think about this. what type would this thing have to have?
  - but recall this the same as saying `forall a. Any -> a`
    - ie "i can give you back any `a` you want"
  - BUT THIS IS NOT TRUE
    - i have a SPECIFIC a inside of my `Any`
    - but i don't know what it is
    - if it's an Int and you ask for a Bool, I can't just give you a bool because i have an int

----

Too Rigid
=========

  - so that's what this means
    - a rigid skolem variable is a type that is existentially quantified
    - you can't leak it out because it doesn't even EXIST outside

----

Anyway
======

  - this kind of solves our problem:

.. code:: haskell

  listOfAnything :: [Any]
  listOfAnything = [ Any 5
                   , Any Bool
                   , Any (show :: Char -> String)
                   ]


    - but it's not actaully useful because we can never get any of this data out
    - shit

----

No Really, It's Actually Useful
===============================

  - as you might guess, this doesn't mean we can't actually do anything useful with the technique
  - just that it requires MORE THINKING
  - let's talk about iterators
    - like in python or whatever

----

Iterators
=========

  - we want to be able to produce a series of values
    - and maybe these values depend on some sort of state
    - we don't really care what that state is, so long as we can pull values out of it

----

Iterators
=========

.. code:: haskell

  data Iterator a where
    Iterator :: { iterState :: s
                , iterNext  :: s -> (a, s)
                } -> Iterator a


  - we can think of an iterator as containing a piece of internal state, along with a function that will use that state to spit out a value and a new state
    - the thing to notice here is that i don't care what the internal state is
    - it doesn't leak out of my type signature
    - so this thing could depend on the weather, or who knows
    - i don't care though

----

Pump It Real Good
=================

  - we can implement a function that uses an Iterator to spit out as

.. code:: haskell

  pump :: Iterator a -> (a, Iterator a)
  pump iter = let getNext = iterNext iter
                  (a, s') = getNext $ iterState iter
               in (a, Iterator s' getNext)


  - this is kind of neat
  - just because we don't know what type is inside of the iterator's state
    - doens't mean that GHC doesn't know that these types are the same

  - so outside of iterator we don't know and can't look at the type
    - but GHC was smart enough to know there is only actually a single type in here
    - even though it doesn't know what it is, it can still reason about it

----

A More Interesting GADT
=======================

.. code:: haskell

  data Dict c where
    Dict :: c => Dict c


  - notice here that c exists in the type, and so it is not existential. ghc can track it
  - but this is not any old data type

----

Constructing Dicts
==================

  - we're saying we can only construct Dict c if c is an instance of a typeclass
  - eg Dict (Enum Bool), Dict (Show Int), but not (Dict (Show (Int -> Int))

----

Reified Constraints
===================

  - what value does THIS provide us?
  - it means we can pass constraints along as values
    - they're now reified at the value level
  - example

.. code:: haskell

  maybeShow :: a -> Maybe (Dict (Show a)) -> String
  maybeShow a (Just Dict) = show a
  maybeShow _ Nothing     = "i don't know how to show that"


  -- example
  maybeShow True (Just Dict)  -- "True"
  maybeShow flip Nothing      -- "i don't know how to show that"


  - we only get a proof of Show a inside of the first case

----

Generalizing
============

- we can use the same technique to make a more useful any-list

.. code:: haskell

  data Showable where
    Showable :: Show a => a -> Showable

  showList :: [Showable] -> [String]
  showList = fmap (λ(Showable a) -> show a)


----

A Counter Example
=================

- but what we can't do is

.. code:: haskell

  data Equatable where
    Equatable :: Eq a => a -> Equatable

  equate :: Equatable -> Equatable -> Bool
  equate (Equatable a) (Equatable b) = a == b


- we can't do this because we don't know that the types packed inside of these things are the same
  - implicitly what we have is `(a :: exists. var0)` and `(b :: exists. var1)` and we are trying to say `a == b` which obviously we can't do since they are different types

----

Eliminators
===========

  - in general, the strategy for doing useful things with existential variables is to introduce eliminators for them
  - if we want to do something useful with a value of unknown type
    - we're going to need to provide a function that can do something FOR ALL types

----

Eliminators
===========

  - the general form of it is this:

.. code:: haskell

  eliminate :: SomeExistential -> (forall a. a -> r) -> r


    - the forall a. a bit should be replaced with the definition of the existential

    - for example:

.. code:: haskell

  eliminateShowable :: Showable
                    -> (forall a. Show a => a -> r)
                    -> r

  eliminateIterator :: Iterator a
                    -> (forall s. s
                              -> (s -> (a, s))
                              -> r)
                    -> r


----

Eliminate the Lack of Intuition
===============================

  - the idea is that if can produce some `r` (that i get to choose) from whatever contents are inside the existential
    - then i can produce an r given some existential

----


- you might be wondering what useful work you can do with an existential value
  - consider this: if the value you're existential over is only an IMPLEMENTATION DETAIL
    - zipkin example
  - or if you don't even care about the existential anyway
- putting it all together
  - we can make a GADT existential over its dict parameters:
  - data SomeDict1 (c :: k -> Constraint) where
    - SomeDict 1 :: c a => Proxy a -> SomeDict1 c
  - we can use this eg `SomeDict1 Show` to get represent that we have a proof of being able to show SOMETHING, even though we don't know what
  - and so we can use the SAME TRICK
    - a list of [SomeDict1 Monad], for example, is a list of Monad instances
    - if someone provided us with such a list, we could use it to generate quicktest checks proving that each instance follows the laws

