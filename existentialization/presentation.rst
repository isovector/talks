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

The Problem
===========

- We would like to be able to automatically generate quickcheck properties for instances we write
- eg. automatic proofs that every `Monad` instance is well behaved

----

BUT FIRST
=========

----

Heterogeneous Lists
===================

- Let's say we are Javascript programmers and we hate types
- We want to make a list that can contain values of any type

- Haskell doesn't let us do this:

.. code:: haskell

  [1, 2, 3] :: [Int]   -- ok
  [True]    :: [Bool]  -- ok
  [1, True] :: ??      -- type error, :(


----

Heterogeneous Lists
===================

- This is not all that useful, it turns out.
- But even so, is Haskell such a shit language that we can't express such a thing?

----

No! We can!
===========

----

The Any Type
============

Use a GADT.

.. code:: haskell

  data Any where
    Any :: a -> Any


We can think of `Any` as a container.

We can stuff whatever we want into it, and get back a value of type `Any`.

----

The Any Type
============

.. code:: haskell

  data Any where
    Any :: a -> Any

  Any True                    :: Any
  Any (show :: Int -> String) :: Any



We've lost track of what type the value inside the `Any` had.

We say the `a` is now *existential*.

As in: we know it *exists*, but not much else about it.

----

RIGID SKOLEMS
=============

What happens if we try to take the value out of the `Any`?

.. code:: haskell

  f (Any a) = a


Any guesses?

----

RIGID SKOLEMS
=============

.. raw:: html

  <pre>
  • Couldn't match expected type ‘t’ with actual type ‘a’
    because type variable ‘a’ would escape its scope
    This (rigid, skolem) type variable is bound by
      a pattern with constructor: Any :: forall a. a -> Any,

  </pre>


u wot m8

----

RIGID SKOLEMS
=============

We can get some insight by looking at what type this thing would have.

.. code:: haskell

  f :: Any -> a
  f (Any a) = a


----

RIGID SKOLEMS
=============

But recall that this is short form for:

.. code:: haskell

  f :: forall a. Any -> a
  f (Any a) = a


ie. "I can give you back any `a` you want"

----

That's a damn lie
=================

There's a specific `a` inside the `Any`.

It might be a `Bool` or a `String` or whatever, but it is *not* "whatever you ask for".

----

Too Rigid
=========

You will run into this error all the time when you first start existentializing things.

So that's what this means:

- a (rigid, skolem) variable is a type that is existentially quantified
- you can't leak it out because it doesn't even EXIST outside

----

Anyways
=======

This kind of solves our subproblem:

.. code:: haskell

  listOfAnything :: [Any]
  listOfAnything = [ Any 5
                   , Any Bool
                   , Any (show :: Char -> String)
                   ]


But it's not actaully useful because we can never get any of this data out.

----

But that doesn't mean the technique isn't useful
================================================

----

Usefulness
==========

As you might guess, this doesn't mean we can't actually do anything useful with the technique.

Just that it requires *more thinking*

Let's talk about iterators. Like in Python or whatever.

----

Iterators
=========

We want to be able to produce a series of values.

And maybe these values depend on some sort of state

We don't really care what that state is, so long as we can pull values out of it

----

A first try
===========

.. code:: haskell

  data Iterator s a = Iterator
    { iterState :: s
    , iterNext  :: s -> (a, s)
    }


This seems to do what we want.

----

But it's kinda gross
====================

The state variable leaks.

That means you can't make a list of these things with different pieces of internal state, eg.

----

Iterators Take 2
================

Let's existentialize it!

.. code:: haskell

  data Iterator a where
    Iterator :: { iterState :: s
                , iterNext  :: s -> (a, s)
                } -> Iterator a


The thing to notice here is that i don't care what the internal state is

It doesn't leak out of my type signature

----

Pump It Real Good
=================

- We can implement a function that uses an Iterator to spit out `a` s

.. code:: haskell

  pump :: Iterator a -> (a, Iterator a)
  pump iter = let getNext = iterNext iter
                  (a, s') = getNext $ iterState iter
               in (a, Iterator s' getNext)


This is kind of neat.

----

Pump It Real Good
=================

.. code:: haskell

  pump :: Iterator a -> (a, Iterator a)
  pump iter = let getNext = iterNext iter
                  (a, s') = getNext $ iterState iter
               in (a, Iterator s' getNext)


We can think of this as

.. code:: haskell

  (iterState, iterNext) :: exists s. (s, s -> (a, s))


GHC doesn't know what this `s` type variable is, but it knows that `iterState` and `iterNext` are talking about the same
`thing`.

----

And now for something seemingly completely different.
=====================================================

----

A More Interesting GADT
=======================

.. code:: haskell

  data Dict (c :: Constraint) where
    Dict :: c => Dict c


Notice here that `c` exists in the type, and so it is not existential.

But this is not any old data type!

----

Constructing Dicts
==================

This says we can only construct a `Dict c` if `c` is an instance.

eg.

.. code:: haskell

  Dict :: Dict (Enum Bool)        -- ok
  Dict :: Dict (Show Int)         -- ok

  Dict :: Dict (Eq (Int -> Int))  -- bad


Haskell doesn't have equality defined for functions.

----

Reified Constraints
===================

What value does this provide us?

It means we can pass constraints along as values -- they're now refied at the value level.

.. code:: haskell

  maybeShow :: a -> Maybe (Dict (Show a)) -> String
  maybeShow a (Just Dict) = show a
  maybeShow _ Nothing     = "i don't know how to show that"


  maybeShow True (Just Dict)  -- "True"
  maybeShow flip Nothing      -- "i don't know how to show that"


----

Generalizing
============

We can use the same technique to make a more useful any-list

.. code:: haskell

  data Showable where
    Showable :: Show a => a -> Showable

  showList :: [Showable] -> [String]
  showList = fmap (λ(Showable a) -> show a)

  -------------------------------------

  myList :: [Showable]
  myList = [Showable 1, Showable Bool, Showable "hello"]

  showList myList  -- [1, Bool, "\"hello\""]


----

A Counter Example
=================

Something we *can't* do:

.. code:: haskell

  data Equatable where
    Equatable :: Eq a => a -> Equatable

  equate :: Equatable -> Equatable -> Bool
  equate (Equatable a) (Equatable b) = a == b


----

A Counter Example
=================

This doesn't work, because it's morally equivalent to this:

.. code:: haskell

  equate :: exists a b. (Eq a, Eq b) => a -> b -> Bool
  equate a b = a == b


We don't know that `a` and `b` have the same type!

----

Eliminators
===========

In general, the strategy for doing useful things with existential variables is to introduce **eliminators** for them.

If we want to do something useful with a value of unknown type, we're going to need to provide a function that can do
something FOR ALL types.

----

Eliminators
===========

The general form of it is this:

.. code:: haskell

  eliminate :: SomeExistential -> (forall a. a -> r) -> r


If you give us an existential, and a way of constructing an `r` for any type I throw at you, then I can give you back an
`r`.

wat?

----

Eliminators
===========

A dumb example:

.. code:: haskell

  eliminate myExistential (const True)   -- True


----

Eliminators
===========

.. code:: haskell

  eliminate :: SomeExistential -> (forall a. a -> r) -> r


The `forall a. a` bit should be replaced with the definition of the existential.

----

Eliminators
===========

.. code:: haskell

  data Showable where
    Showable :: Show a => a -> Showable


  eliminateShowable :: Showable
                    -> (forall a. Show a => a -> r)
                    -> r


----

Eliminators
===========

.. code:: haskell

  data Iterator a where
    Iterator :: { iterState :: s
                , iterNext  :: s -> (a, s)
                } -> Iterator a


  eliminateIterator :: Iterator a
                    -> (forall s. s
                              -> (s -> (a, s))
                              -> r)
                    -> r


----

Eliminate the Lack of Intuition
===============================

The idea is that if can produce some `r` (that i get to choose) from whatever contents are inside the existential

Then I can produce an `r` given some existential value!

----

A Server
========

Let's say I want to run a server that will respond to different endpoints.

But each endpoint will take and return different payload types.

----

A Server
========

.. code:: haskell

  class Encodable a where
    encode :: a -> ByteString
    decode :: ByteString -> a

  instance Encodable Bool
  instance Encodable Int
  instance Encodable String
  -- etc


----

A Server
========

.. code:: haskell

  data SomeHandler where
    SomeHandler :: (Encodable a, Encodable b)
                => (a -> IO b)
                -> SomeHandler


----

A Server
========

.. code:: haskell

  recv :: IO (Endpoint, ByteString)
  send :: IO ByteString


  serve :: [(Endpoint, SomeHandler)] -> IO ()
  serve handlers = forever $ do
    (endpoint, payload) <- recv

    case lookup endpoint handlers of
      Nothing -> putStrLn "no handler!"
      Just (SomeHandler handler) ->
        result <- handler $ decode payload
        send $ encode result


----

Thanks for listening!
=====================

Any questions?

