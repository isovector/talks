:title: Don't Eff It Up
:data-transition-duration: 150

:css: fonts.css
:css: presentation.css

----

:id: title

.. raw:: html

  <h1>Don't Eff It Up</h1>
  <h2>Freer Monads in Action</h2>
  <h3>A talk by <span>Sandy Maguire</span></h3>
  <h4>reasonablypolymorphic.com</h4>

----

A dumb example.
===============

We will make a (very) simple banking app.

* It will attempt to withdraw funds
* Logs a message if it fails
* Updates the current balance if it succeeds

----

Types first!
============

.. code:: haskell

  withdraw :: ( MonadIO     m
              , MonadLogger m
              )
           => Int
           -> m (Maybe Int)

----

.. code:: haskell

  withdraw :: ( MonadIO     m
              , MonadLogger m
              )
           => Int
           -> m (Maybe Int)

  withdraw desired = do
    amount <- getCurrentBalance
    if amount < desired
       then do
         log "not enough funds"
         return Nothing

       else do
         putCurrentBalance $ amount - desired
         return $ Just amount

----

But how can we test it?
=======================

----

A new datatype describing if we're running for real:

.. code:: haskell

  data Mode = ForReal
            | Test (IORef Int)

----

.. raw:: html

  <pre>
  withdraw :: ( MonadIO     m
              , MonadLogger m
              )
           => <span class="new">Mode</span>
           -> Int
           -> m (Maybe Int)

  withdraw mode desired = do
    amount <- <span class="new">case mode of
                ForReal    -> </span>getCurrentBalance<span class="new">
                Test ioref -> liftIO $ readIORef ioref</span>
    if amount < desired
       then do
         log "not enough funds"
         return Nothing

       else do
         <span class="new">let putAction =
               case mode of
                 ForReal    -> </span>putCurrentBalance<span class="new">
                 Test ioref -> liftIO . writeIORef ioref</span>
         putAction $ amount - desired
         return $ Just amount
  </pre>

----

This sucks!
===========

* IO is directly exposed
* Test code is interspersed with our real logic
* No compiler guarantees that we mocked *all* of our IO

----

Wouldn't it be nice...
======================

... if we could just write the program that we cared about?

----

Polymorphism to the rescue!
===========================

.. code:: haskell

  class Monad m => MonadBank m where
    getCurrentBalance :: m Int
    putCurrentBalance :: Int -> m ()

----

The code we want to write.
==========================

.. raw:: html

  <pre>
  withdraw :: ( <span class="new">MonadBank</span>   m
              , MonadLogger m
              )
           => Int
           -> m (Maybe Int)

  withdraw desired = do
    amount <- <span class="new">getCurrentBalance</span>
    if amount < desired
       then do
         log "not enough funds"
         return Nothing

       else do
         <span class="new">putCurrentBalance</span> $ amount - desired
         return $ Just amount
  </pre>

----

By adding this new constraint, we can abstract over IO.

Our application and test code can swap out different monads.

----

All is right in the world.
==========================

Or is it?

This abstraction comes with a heavy cost.

----

We need a carrier...
====================

.. code:: haskell

  newtype IOBankT m a = IOBankT
    { runIOBankT :: IdentityT m a
    }

----

one that behaves with MTL...
============================

.. code:: haskell

  {-# LANGUAGE GeneralizedNewtypeDeriving #-}

    deriving ( Functor
             , Applicative
             , Monad
             , MonadError e
             , MonadIO
             , MonadRWS r w s
             , MonadReader r
             , MonadState s
             , MonadTrans
             , MonadWriter w
             , ...
             )

----

which implements our monad...
=============================

.. code:: haskell

  instance MonadIO m => MonadBank (IOBankT m) where
    getCurrentBalance = ...
    putCurrentBalance = ...

----

and doesn't need to be at the top of the stack...
=================================================

.. code:: haskell

  instance MonadBank m => MonadBank (ReaderT r m) where
    getCurrentBalance = lift getCurrentBalance
    putCurrentBalance = lift . getCurrentBalance

  instance MonadBank m => MonadBank (WriterT w m) where
    getCurrentBalance = lift getCurrentBalance
    putCurrentBalance = lift . getCurrentBalance

  instance MonadBank m => MonadBank (StateT s m) where
    getCurrentBalance = lift getCurrentBalance
    putCurrentBalance = lift . getCurrentBalance

  -- so many more

----

.. raw:: html

  <h1 style="text-align: center; font-size: 48pt;"><span class="cursive">Nobody</span> has time for this crap.</h1>

----

Things that take a lot of work don't get done.
==============================================

Even if they're best practices.

Boilerplate gets in the way.

----

Monad transformers are a hack.
==============================

Everything else we use in Haskell composes.

Why don't monads?

----

There's a better way.
=====================

----

Free monads.
============

Monadic programs expressed as data structures we can manipulate.

Provided by the `freer-effects` package.

----

.. raw:: html

  <h1 class="cursive">
  Write it now; decide what it means later.
  </h1>

----

Eff to the Rescue!
==================

.. raw:: html

  <pre>
  withdraw :: ( <span class="new">Member Bank   r</span>
              , <span class="new">Member Logger r</span>
              )
           => Int
           -> <span class="new">Eff r</span> (Maybe Int)

  withdraw desired = do
    amount <- getCurrentBalance
    if amount < desired
       then do
         log "not enough funds"
         return Nothing

       else do
         putCurrentBalance $ amount - desired
         return $ Just amount
  </pre>

----

Small change. Big impact.
=========================

.. code:: haskell

  withdraw :: ( MonadBank   m
              , MonadLogger m
              )
           => Int
           -> m (Maybe Int)

.. raw:: html

  <div class="bigass down">➠</div>

.. code:: haskell

  withdraw :: ( Member Bank   r
              , Member Logger r
              )
           => Int
           -> Eff r (Maybe Int)

----

Listen to the types.
====================

----

An unambiguous monad.
=====================

.. raw:: html

  <pre>
  withdraw :: ( Member Bank   r
              , Member Logger r
              )
           => Int
           -> <span class="new">Eff r</span> (Maybe Int)
  </pre>

----

No nominal typing.
==================

.. raw:: html

  <pre>
  withdraw :: ( <span class="new">Member Bank   r</span>
              , Member Logger r
              )
           => Int
           -> Eff r (Maybe Int)
  </pre>

----

Effects as data.
================

.. code:: haskell

  {-# LANGUAGE GADTs #-}

  data Bank a where
    GetCurrentBalance :: Bank Int
    PutCurrentBalance :: Int -> Bank ()

----

.. code:: haskell

  getCurrentBalance :: Member Bank r
                    => Eff r Int
  getCurrentBalance = send GetCurrentBalance

----

.. code:: haskell

  putCurrentBalance :: Member Bank r
                    => Int
                    -> Eff r ()
  putCurrentBalance amount = send $ PutCurrentBalance amount

----

Still too much boilerplate?
===========================

.. code:: haskell

  {-# LANGUAGE TemplateHaskell #-}

  data Bank a where
    GetCurrentBalance :: Bank Int
    PutCurrentBalance :: Int -> Bank ()

  makeFreer ''Bank

----

Don't forget the lumberjack.
============================

.. code:: haskell

  data Logger a where
    Log :: String -> Logger ()

  makeFreer ''Logger

----

What's left?
============

.. raw:: html

  <pre>
  withdraw :: ( Member Bank   r
              , Member Logger r
              )
           => Int
           -> Eff <span class="new">r</span> (Maybe Int)
  </pre>

----

The REPL can help.
==================

.. code:: haskell

  > :kind Eff

  Eff :: [* -> *] -> * -> *

----

An exact correspondence.
========================

.. code:: haskell

  StateT s (ReaderT r IO) a

.. raw:: html

  <div class="bigass down">⬌</div>

.. code:: haskell

  Eff '[State s, Reader r, IO] a

----

So what?
========

`main` runs in `IO` -- not in `Eff`.

----

We have one special function:

.. code:: haskell

  runM :: Monad m => Eff '[m] a -> m a

----

Not just for monads!
====================

.. code:: haskell

  run :: Eff '[] a -> a

----

`run` and `runM` provide base cases.

----

Induction.
==========

We want a function that looks like this:

.. code:: haskell

  runLogger :: Eff (Logger ': r) a
            -> Eff r a

It "peels" a `Logger` off of our eff stack.

----

What does it mean to run a `Logger`?

Maybe we want to log those messages to `stdout`.

.. raw:: html

  <pre>
  runLogger :: <span class="new">Member IO r</span>
            => Eff (Logger ': r) a
            -> Eff r a
  </pre>

----

All for naught?
===============

No!
---

Even though we have `IO` here, it's not the program that requires it; only the
intepretation.

----

.. code:: haskell

  runLogger :: Member IO r
            => Eff (Logger ': r) a
            -> Eff r a

  runLogger = runNat logger2io
    where
      logger2io :: Logger x -> IO x
      logger2io (Log s) = putStrLn s

----

We can do the same thing for `Bank`.
====================================

----

.. code:: haskell

  runBank :: Member IO r
          => Eff (Bank ': r) a
          -> Eff r a

  runBank = runNat bank2io
    where
      bank2io :: Bank x -> IO x
      bank2io GetCurrentBalance            =  -- use IO to return an Int
      bank2io (PutCurrentBalance newValue) =  -- perform IO and return ()

----

Back to the REPL.
=================

.. code:: haskell

  > :t (runM . runLogger . runBank)

  Eff '[Bank, Logger, IO] a -> IO a

----

.. code:: haskell

  > :t (runM . runLogger . runBank $ withdraw 50)

  IO (Maybe Int)

----

But how can we test this?
=========================

----

.. code:: haskell

  {-# LANGUAGE ScopedTypeVariables #-}

  ignoreLogger :: forall r a
                . Eff (Logger ': r) a
               -> Eff r a

  ignoreLogger = handleRelay pure bind
    where
      bind :: forall x
            . Logger x
           -> (x -> Eff r a)
           -> Eff r a
      bind (Log _) cont = cont ()

----

.. code:: haskell

  testBank :: forall r a
             . Int
            -> Eff (Bank ': r) a
            -> Eff r a

  testBank balance = handleRelayS balance (const pure) bind
    where
      bind :: forall x
            . Int
           -> Bank x
           -> (Int -> x -> Eff r a)
           -> Eff r a
      bind s GetCurrentBalance      cont = cont s  s
      bind _ (PutCurrentBalance s') cont = cont s' ()

----

Finally, pure interpretations!
==============================

.. code:: haskell

  > :t (run . ignoreLogger . testBank)

  Eff '[Bank, Logger] a -> a

----

.. code:: haskell

  > :t (run . ignoreLogger . testBank $ withdraw 50)

  Maybe Int

----

So far, this doesn't seem very reusable.
========================================

----

Instead of this...
==================

.. code:: haskell

  data Logger a where
    Log :: String -> Logger ()

----

Why not this?
=============

.. raw:: html

  <pre>
  data <span class="new">Writer w</span> a where
    Tell :: <span class="new">w</span> -> Writer w ()
  </pre>

Note: there is no `Monoid` constraint here!

----

Instead of this...
==================

.. code:: haskell

  data Bank a where
    GetCurrentBalance :: Bank Int
    PutCurrentBalance :: Int -> Bank ()

----

Why not this?
=============

.. raw:: html

  <pre>
  data <span class="new">State s</span> a where
    Get :: State s <span class="new">s</span>
    Put :: <span class="new">s</span> -> State s ()
  </pre>

----

.. raw:: html

  <h1>This gives us more <span class="cursive" style="font-size: 48pt;">denotational meaning</span>.</h1>

----

.. raw:: html

  <pre>
  withdraw :: ( Member <span class="new">(State Int)</span>     r
              , Member <span class="new">(Writer String)</span> r
              )
           => Int
           -> Eff r (Maybe Int)

  withdraw desired = do
    amount <span class="new">:: Int</span> <- <span class="new">get</span>
    if amount < desired
       then do
         <span class="new">tell</span> "not enough funds"
         return Nothing

       else do
         <span class="new">put</span> $ amount - desired
         return $ Just amount
  </pre>

----

Mo' generality = fewer problems.
================================

More general types are more likely to already have the interpretations that you
want.

----

A drop-in for MTL?
==================

Yes! But more than just that!

----

A conceptually different execution model.
=========================================

In MTL:

.. code:: haskell

  runReaderT :: ReaderT x m a -> x -> m a

In Eff:

.. code:: haskell

  runReader :: Eff (Reader x ': r) a -> x -> Eff r a

----

Interpreting effects in terms of one another.
=============================================

.. code:: haskell

  data Exc e a where
    ThrowError :: e -> Exc e a
  makeFreer ''Exc

----

.. code:: haskell

  accumThenThrow :: ( Eq e
                    , Monoid e
                    , Member (Exc e) r
                    )
                  => Eff (Writer e ': r) a
                  -> Eff r a
  accumThenThrow prog = do
    let (a, e) = pureWriter prog
    unless (e == mempty) $ throwError e
    return a

----

Non-trivial transformations.
============================

.. code:: haskell

  data SetOf s a where
    SetAdd      :: s -> SetOf s ()
    SetContains :: s -> SetOf s Bool
  makeFreer ''SetOf

----

.. code:: haskell

  dedupWriter :: ( Member (SetOf  w) r
                 , Member (Writer w) r
                 )
              => Eff r a
              -> Eff r a
  dedupWriter = interpose pure bind
    where
      bind (Tell w) cont = do
        alreadyTold <- setContains w
        unless alreadyTold $ do
          setAdd w
          tell w
        cont ()

----

Summing up.
===========

* Eff gives us the flexibility of MTL without the boilerplate.
* We get testing (mostly) for free.
* We're forced to separate our business logic from implementation details.

----

Thanks for listening!
=====================

Questions?
==========

