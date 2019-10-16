---
title: "Polysemy: Chasing Performance in Free Monads"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 3
    left: 5
    right: 5
---

# Polysemy

## Chasing Performance in Free Monads

---

* **Sandy Maguire**
* sandy@sandymaguire.me

* reasonablypolymorphic.com
* github.com/isovector


Today's slides:

* reasonablypolymorphic.com/polysemy-talk

---

Our codebase was written by contractors.

Big ball o' IO spaghetti.

Impossible to test.

---

Programming is really hard.

. . .

Programming culture puts lots of arbitrary difficulties in the way.

---

The biggest problem?

*The computer's understanding comes first and foremost.*

---

## Software Lifecycle

* Product person has an idea
* Explains it to an engineer
* The engineer explains it to the computer

. . .

* **There's a bug!**

. . .

* Another engineer reads between the lines
* Tries to rebuild the understanding the original author had

---

Alternative idea:

. . .

> Let's just write code that better captures our understanding.

. . .

Today's languages are not well suited to this problem.

---

Instead, *let's write a new language.*

A domain specific language that is *perfect* for solving the specific problem at
hand.

. . .

... but there are lots of problems. No DSL is going to be great for all of
them.

---

Instead, instead, *let's build a tool for writing new languages.*

. . .

Let's make it easy to create the perfect language to solve a particular problem.

...and then solve the problem in that language!

---

*Free monads* are what I think programming will look like in 30 years.

. . .

Write your applications in *domain specific language* designed for your exact
problem.

. . .

*Run a series of transformations* to compile your high-level specification into
lower-level DSLs.

---

Most programs are easy to describe.

. . .

The majority of a codebase is spent dealing with nitty-gritty details.

. . .

This is where most of the bugs are.

---

Let's turn implementation details into library code!

---

# Example

Data ingestion service that:

* reads encrypted CSV files
* emits them in batches to a streaming HTTP service
* records statistics in Redis

---

```haskell
ingest
    :: ( Member (Input Record) r
       , Member (Output Record) r
       , Member (Output Stat) r
       )
    => Eff r ()
ingest = input >>= \case
  Nothing     -> pure ()
  Just record -> do
    output record
    output ProcessedRecordStat
    ingest
```

---

```haskell
main = ingest

```

> Open Effects:
>
> { Input Record, Output Record, Output Stat }

---

```haskell
main = ingest
     & csvInput "file.csv"

```

> Open Effects:
>
> { FileProvider, Output Record, Output Stat }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
```

> Open Effects:
>
> { FileProvider, Output Record, Output Stat, Encryption }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
```

> Open Effects:
>
> { FTP, Output Record, Output Stat, Encryption }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
```

> Open Effects:
>
> { FTP, Output [Record], Output Stat, Encryption }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall

```

> Open Effects:
>
> { FTP, HTTP, Output Stat, Encryption }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall
     & redisOuput @Stat   mkRedisKey
```

> Open Effects:
>
> {FTP, HTTP, Encryption, Redis }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall
     & redisOuput @Stat   mkRedisKey
     & runEncryption

```

> Open Effects:
>
> { FTP, HTTP, Redis }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall
     & redisOuput @Stat   mkRedisKey
     & runEncryption
     & runHTTP

```

> Open Effects:
>
> { FTP, Redis, IO }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall
     & redisOuput @Stat   mkRedisKey
     & runEncryption
     & runHTTP
     & runFTP

```

> Open Effects:
>
> { Redis, IO }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall
     & redisOuput @Stat   mkRedisKey
     & runEncryption
     & runHTTP
     & runFTP
     & runRedis
```

> Open Effects:
>
> { IO }

---

```haskell
main = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & ftpFileProvider
     & batch      @Record 500
     & postOutput @Record mkApiCall
     & redisOuput @Stat   mkRedisKey
     & runEncryption
     & runHTTP
     & runFTP
     & runRedis
     & runM
```

---

But maybe we want to test this without a million mocked services?

. . .

```haskell
test :: ([Stat], ([Record], ()))
test = ingest
     & runInput [record1, record2]
     & runPureOuput @Record
     & runPureOuput @Stat
     & run
```

---


If both a test and real interpreter are correct,

. . .

And the program is correct under the test,

. . .

Then the program is correct under the real interpreter!

. . .

**Correctness composes!**

----

```haskell
data Teletype k
  = Done k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)


echo :: Teletype ()
echo = ReadLine $ \msg ->
       WriteLine msg
     $ Done ()
```

. . .


```haskell
instance Monad Teletype where
  return = Done
  Done          k >>= f = f k
  WriteLine msg k >>= f = WriteLine msg $ k >>= f
  ReadLine      k >>= f = ReadLine $ \str -> k str >>= f
```

----

Because it's a monad, we can write this more idiomatically.

```haskell
echo :: Teletype ()
echo = do
  msg <- ReadLine Done
  WriteLine msg $ Done ()
```

----

... and define evaluation semantics for it.

. . .

```haskell
runTeletypeInIO :: Teletype a -> IO a
runTeletypeInIO (Done a) = pure a
```
. . .
```haskell
runTeletypeInIO (WriteLine msg k) = do
  putStrLn msg
  runTeletypeInIO k
```
. . .
```haskell
runTeletypeInIO (ReadLine k) =  do
  msg <- getLine
  runTeletypeInIO $ k msg
```

----


```haskell
runTeletypePurely :: [String] -> Teletype a -> ([String], a)
runTeletypePurely _ (Done a) = ([], a)
```
. . .
```haskell
runTeletypePurely ls (WriteLine msg k) =
  let (rs, a) = runTeletypePurely ls k
   in (msg : rs, a)
```
. . .
```haskell
runTeletypePurely []       (ReadLine k) =
  runTeletypePurely [] $ k ""
```
. . .
```haskell
runTeletypePurely (l : ls) (ReadLine k) =
  runTeletypePurely ls $ k l
```

----

```haskell
data Teletype k
  = Done k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)
```

The `Done` constructor and the recursion are only necessary to make this a
`Monad`.

We can factor them out.


---

Before:

```haskell
data Teletype k
  = Done k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)
```

After:

```haskell
data Free f k
  = Pure k
  | Impure (f (Free f k))


data Teletype a
  = WriteLine String a
  | ReadLine (String -> a)
```

----

`Free f` is a `Monad` whenever `f` is a `Functor`!

```haskell
instance Functor f => Monad (Free f) where
  return = Pure
  Pure k   >>= f = f k
  Impure z >>= f = Impure $ fmap (\x -> x >>= f) z
```

----

Let's write some helper functions:

```haskell
writeLine :: String -> Free Teletype ()
writeLine msg = Impure $ WriteLine msg $ pure ()

readLine :: Free Teletype String
readLine = Impure $ ReadLine pure
```

. . .

`echo` is no longer conspicuous:

. . .
```haskell
echo :: Free Teletype ()
echo = do
  msg <- readLine
  writeLine msg
```

----

We can also factor out the evaluation plumbing:

```haskell
runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
runFree _ (Pure a)  = pure a
runFree f (Impure k) = f k >>= runFree f
```

. . .

Less boilerplate in our interpretation:

```haskell
runTeletypeInIO :: Free Teletype a -> IO a
runTeletypeInIO = runFree $ \case
  WriteLine msg k -> do
    putStrLn msg
    pure k
  ReadLine k -> do
    msg <- getLine
    pure $ k msg
```

----

# Combining Multiple Effects

```haskell
data Bell k
  = RingBell k
  deriving Functor
```

. . .

```haskell
data Sum f g a
  = L (f a)
  | R (g a)

instance (Functor f, Functor g) => Functor (Sum f g)
```

. . .

```haskell
type TeletypeWithBell = Sum Teletype Bell
```

----

Before:

```haskell
writeLine :: String -> Free Teletype ()
writeLine msg = Impure $ WriteLine msg $ pure ()
```

After:

```haskell
writeLine :: String -> Free TeletypeWithBell ()
writeLine msg = Impure $ L $ WriteLine msg $ pure ()

ringBell :: Free TeletypeWithBell ()
ringBell = Impure $ R $ RingBell $ pure ()
```

----

We can interleave actions from both effects.

```haskell
ringItSingIt :: Free TeletypeWithBell ()
ringItSingIt = do
  msg <- readLine
  when (msg == "ring the bell!") ringBell
```

----

```haskell
interpret
    :: Monad m
    => (forall x. f x -> m x)
    -> (forall x. g x -> m x)
    -> Sum f g a
    -> m a
interpret hf _ (L mf) = hf mf
interpret _ hg (R mg) = hg mg
```

. . .

We can nest effects as deeply as we want inside of `Sum`!

----

# Effects a la Carte

```haskell
data Union r a
```

. . .

For example:

```haskell
Union '[Bell, Teletype, State Bool, Error InvalidArgument] a
```

. . .

`Union r` is a `Functor` iff every type inside of `r` is.

----

We can get in and out of a `Union`.


```haskell
class Member f r where
  inj  :: f a       -> Union r a
  proj :: Union r a -> Maybe (f a)
```


----


Before:

```haskell
writeLine :: String -> Free TeletypeWithBell ()
writeLine msg = Impure $ L $ WriteLine msg $ pure ()
```

After:


```haskell
writeLine :: Member Teletype r => String -> Free (Union r) ()
writeLine msg = Impure $ inj $ WriteLine msg $ pure ()
```

. . .

Now we are **polymorphic in our capabilities**.

---

## Free Constructions

```haskell
data Coyoneda f a where
  Coyoneda
      :: f a  -- *
      -> (a -> b)
      -> Coyoneda f b
```

. . .

`Coyoneda f` is a functor, even when `f` is not!

---

```haskell
instance Functor (Coyoneda f) where
  fmap f' (Coyoneda a f) = Coyoneda a (f' . f)
```

For this reason, we call `Coyoneda` the "free functor."

---

By implementing `Union` in terms of `Coyoneda`, we remove the need for users to
write `Functor` instances.

Before:

```haskell
Union '[State s, Error e] a
    = Sum (State s)
          (Error e)
          a
```

Now:

```haskell
Union '[State s, Error e] a
    = Sum (Coyoneda (State s))
          (Coyoneda ((Error e))
          a
```

---

With this encoding, we no longer need to have continuations in our effects.

Recall that these only existed in order to give Functor instances!

. . .

Before:

```haskell
data Teletype a
  = WriteLine String a
  | ReadLine (String -> a)
```

. . .

After:

```haskell
data Teletype a where
  WriteLine :: String -> Teletype ()
  ReadLine  :: Teletype String
```

. . .

> Exactly parallels the types of the actions

----

We have successfully cut away almost all of the boilerplate!

Unfortunately, our performance here is terrible.

---

```haskell
-- We have a pointer here
-- v
Impure (Coyoneda ReadLine (\msg ->
  Impure (Coyoneda (WriteLine msg) (\() ->


    ()
    ))))
```

---

```haskell


Impure (Coyoneda ReadLine (\msg ->
  Impure (Coyoneda (WriteLine msg) (\() ->
  -- But we need to change this part of the data structure
  -- v
    ()
    ))))
```

. . .

Modifying this thing is `O(n)`; therefore *constructing it* is `O(n^2)`!

----

## The Continuation Passing Transformation

Interesting fact: composing functions is `O(1)`. Just stick one after the other!

If we could somehow implement `Free` in terms of function composition, we would
avoid the bad performance.

---

Recall that `Free` is uniquely determined by its interpretation function:

```haskell
runFree
    :: ∀ m
     . Monad m
    => (∀ x. Union r x -> m x)
    -> Free r a
    -> m a
```

As it happens, this thing is exactly what we need.

---

```haskell
runFree
    -> Free r a
    -> ∀ m
     . Monad m
    => (∀ x. Union r x -> m x)
    -> m a
```

and then put it into a record:

```haskell
newtype Freer r a = Freer
  { runFreer
        :: ∀ m
         . Monad m
        => (∀ x. Union r x -> m x)
        -> m a
  }
```

---

This transformation is known as the *Boehm-Berarducci encoding.*

Harder to work with, but often asymptotically faster.

---

## The Takeaway

Theory is not just intellectual signaling.

It's *actually useful* for finding better ways of implementing things.

---

## An Effect We Can't Have!

```haskell
throw
    :: Member (Error e) r
    => e
    -> Freer r a

catch
    :: Member (Error e) r
    => Freer r a
    -> (e -> Freer r a)
    -> Freer r a
```

---

```haskell
class Effect e where
  weave :: Functor tk
        => tk ()
        -> (∀ x. tk (m x) -> n (tk x))
        -> e m a
        -> e n (tk a)
```

Think `m` is an effect stack with some effect, and `n` is the same stack, but
*without* that effect.

. . .

`Effect e` describes how other effects can push their statefulness through `e`.

---

```haskell
data Weaving e m a where
  Weaving
     :: Functor tk
     => e m a
     -> tk ()
     -> (forall x. tk (m x) -> n (tk x))
     -> (tk a -> b)
     -> Weaving e n b
```

`Weaving` is the free `Effect`!

. . .

```haskell
instance Effect (Weaving e) where
  weave tk' distrib' (Weaving e tk distrib f) =
    Weaving e (Compose $ tk <$ tk')
         (fmap Compose . distrib' . fmap distrib . getCompose)
         (fmap f . getCompose)
```

----

And we can get into a `Weaving` by using an `Identity` functor as our initial state.

```haskell
liftWeaving :: Functor m => e m a -> Weaving e m a
liftWeaving e =
    Weaving e (Identity ())
              (fmap Identity . runIdentity)
              runIdentity
```

----

Somewhat amazingly, this works!

. . .

But all it means is we've delayed giving a meaning for `Effect` until we need to
interpret it.

----

A problem:

The type of `runFree` doesn't allow us to change the return type.

```haskell
runFree
    :: ∀ m. Monad m
    => Free r a
    -> (∀ x. Union r (Free r) x -> m x)
    -> m a
```

. . .

It seems like maybe we could just stick a functor in here.

. . .

```haskell
runFree
    :: ∀ m tk. (Monad m, Functor tk)
    => Free r a
    -> (∀ x. Union r (Freer r) x -> m (tk x))
    -> m (tk a)
```

. . .

**Unfortunately this is no longer a `Monad`!**

----

Recall that we're allowed to pick *any* `Monad` for the result of `runFree`.

Instead of evaluating to the final monad `m`...

----

Just transform it into `StateT s m` and immediately evaluate *that*!

. . .

```haskell
import qualified Control.Monad.Trans.State as S

runState
    :: s
    -> Free (e ': r) a
    -> Free r (s, a)
runState s (Free m) = Free $ \nt ->
  S.runStateT s $ m $ \u ->
    case decomp u of
      Left x -> S.StateT $ \s' ->
        nt . weave (s', ()) (uncurry $ runState f)
           $ x
      Right (Weaving Get _ f)      -> fmap f $ S.get
      Right (Weaving (Put s') _ f) -> fmap f $ S.put s'
```

----

We've solved all of the problems! We now have solutions for

* *performance*
* *expressiveness*
* *boilerplate*

all of which work together!

---

# Thanks for listening!

. . .

Questions?


