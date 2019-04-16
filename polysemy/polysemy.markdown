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

```haskell
data Teletype k
  = Pure k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)
  deriving (Functor, Applicative) via WrappedMonad Teletype
```

----

```haskell
instance Monad Teletype where
  return = Pure
  Pure          k >>= f = f k
  WriteLine msg k >>= f = WriteLine msg $ k >>= f
  ReadLine      k >>= f = ReadLine $ \str -> k str >>= f
```

----

```haskell
echo :: Teletype ()
echo = ReadLine
     $ \msg -> WriteLine msg
     $ Pure ()
```

----

```haskell
echo :: Teletype ()
echo = do
  msg <- ReadLine pure
  WriteLine msg $ pure ()
```

----

```haskell
runTeletypeInIO :: Teletype a -> IO a
runTeletypeInIO (Pure a) = pure a
runTeletypeInIO (WriteLine msg k) = do
  putStrLn msg
  runTeletypeInIO k
runTeletypeInIO (ReadLine k) =  do
  msg <- getLine
  runTeletypeInIO $ k msg
```

----

```haskell
runTeletypePurely :: [String] -> Teletype a -> ([String], a)
runTeletypePurely _ (Pure a) = ([], a)
runTeletypePurely ls (WriteLine msg k) =
  let (rs, a) = runTeletypePurely ls k
   in (msg : rs, a)
runTeletypePurely []       (ReadLine k) =  runTeletypePurely [] $ k ""
runTeletypePurely (l : ls) (ReadLine k) =  runTeletypePurely ls $ k l
```

----

```haskell
data Free f k
  = Pure k
  | Impure (f (Free f k))
  deriving (Functor, Applicative) via WrappedMonad (Free f)
```

----

```haskell
instance Functor f => Monad (Free f) where
  return = Pure
  Pure k   >>= f = f k
  Impure z >>= f = Impure $ fmap (\x -> x >>= f) z
```

----

```haskell
data Teletype a
  = WriteLine String a
  | ReadLine (String -> a)
  deriving Functor
```

----

```haskell
writeLine :: String -> Free Teletype ()
writeLine msg = Impure $ WriteLine msg $ pure ()

readLine :: Free Teletype String
readLine = Impure $ ReadLine pure
```

----

-- TODO(sandy): remove the trailing ticks in this file

```haskell
echo :: Free Teletype ()
echo = do
  msg <- readLine
  writeLine msg
```

----

```haskell
runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
runFree _ (Pure a)  = pure a
runFree f (Impure k) = f k >>= runFree f
```

----

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

```haskell
data Bell k
  = RingBell k
  deriving Functor
```

----

```haskell
data Sum f g a
  = L (f a)
  | R (g a)
  deriving Functor

type TeletypeWithBell = Sum Teletype Bell
```

----

```haskell
writeLine :: String -> Free TeletypeWithBell ()
writeLine msg = Impure $ L $ WriteLine msg $ pure ()

readLine :: Free TeletypeWithBell String
readLine = Impure $ L $ ReadLine pure

ringBell :: Free TeletypeWithBell ()
ringBell = Impure $ R $ RingBell $ pure ()
```

----

```haskell
ringItSingIt :: Free TeletypeWithBell String
ringItSingIt = do
  msg <- readLine
  when (msg == "ring the bell!") ringBell
  pure msg
```

----

```haskell
data Union r a

class Member f r where
  inj  :: f a       -> Union r a
  proj :: Union r a -> Maybe (f a)
```

----

```haskell
writeLine :: Member Teletype r => String -> Free (Union r) ()
writeLine msg = Impure $ inj $ WriteLine msg $ pure ()

readLine :: Member Teletype r => Free (Union r) String
readLine = Impure $ inj $ ReadLine pure

ringBell :: Member Bell r => Free (Union r) ()
ringBell = Impure $ inj $ RingBell $ pure ()
```

----

Various Extensions

----


```haskell
runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
```

What if we just GADTd it?

```haskell
data Teletype a where
  WriteLine :: String -> Teletype ()
  ReadLine  :: Teletype String

data Bell a where
  RingBell :: Bell ()
```

No more \ty{Functor}s!

----

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

----

```haskell
runReader :: Monad m => ReaderT r m a -> r -> m a

runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
```

This thing is a \ty{ReaderT} in disguise!

----


```haskell
newtype Freer r a = Freer
  { runFreer
        :: ∀ m. Monad m
        => (∀ x. Union r x -> m x)
        -> m a
  }
  deriving (Functor, Applicative) via WrappedMonad (Freer r)
```

----

```haskell
instance Monad (Freer f) where
  return a = Freer $ \nt -> pure a
  m >>= f  = Freer $ \nt -> do
    a <- runFreer m nt
    runFreer (f a) nt

instance (Monad m) => Monad (ReaderT r m) where
  return a = ReaderT $ \r -> pure a
  m >>= f  = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (f a) r
```

----

```haskell
liftFreer :: Member f r => f a -> Freer r a
liftFreer fa = Freer $ \nt -> nt $ inj fa
```

----

```haskell
writeLine' :: Member Teletype r => String -> Freer r ()
writeLine' msg = liftFreer $ WriteLine msg

readLine' :: Member Teletype r => Freer r String
readLine' = liftFreer ReadLine

ringBell' :: Member Bell r => Freer r ()
ringBell' = liftFreer RingBell
```

----

What the heck is going on?

Now any time our free monad wants to use an action, it immediately runs it in
the final monad.

---

```haskell
echo :: Member Teletype r => Freer r ()
echo = do
  msg <- readLine
  writeLine msg

echoIO :: IO ()
echoIO = runFreer runTeletypeInIO echo
```

----

```haskell
echoIO :: IO ()
echoIO = runFreer runTeletypeInIO echo
```

----

```haskell
echoIO :: IO ()
echoIO = runFreer runTeletypeInIO $ do
  msg <- readLine
  writeLine msg
```

----

```haskell
echoIO :: IO ()
echoIO = do
  msg <- runTeletypeInIO readLine
  runTeletypeInIO $ writeLine msg
```

----

```haskell
echoIO :: IO ()
echoIO = do
  msg <- runTeletypeInIO ReadLine
  runTeletypeInIO $ WriteLine msg
```

----

```haskell
echoIO :: IO ()
echoIO = do
  msg <- case ReadLine of
           ReadLine      -> getLine
           WriteLine msg -> putStrLn msg
  case WriteLine msg of
    ReadLine -> getLine
    WriteLine msg -> putStrLn msg
```

----

```haskell
echoIO :: IO ()
echoIO = do
  msg <- case ReadLine of
           ReadLine      -> getLine
           -- WriteLine msg -> putStrLn msg
  case WriteLine msg of
    -- ReadLine -> getLine
    WriteLine msg -> putStrLn msg
```

----

```haskell
echoIO :: IO ()
echoIO = do
  msg <- case ReadLine of
           ReadLine      -> getLine
  case WriteLine msg of
    WriteLine msg -> putStrLn msg
```

----

```haskell
echoIO :: IO ()
echoIO = do
  msg <- getLine
  putStrLn msg
```

So free!

----

Shoutouts to Li-Yao Xia for the final encoding

And to Ollie Charles for pointing out that this thing is just a ReaderT

----

Lets rewind.

----

```haskell
throw
    :: Member (Error e) r
    => e
    -> Semantic r a

catch
    :: Member (Error e) r
    => Semantic r a
    -> (e -> Semantic r a)
    -> Semantic r a
```

----

```haskell
data Error e k
  = Throw e
  | ∀ x. Catch (???)
               (e -> ???)
               (x -> k)
```

----

```haskell
data Error e m k
  = Throw e
  | ∀ x. Catch (m x)
               (e -> m x)
               (x -> k)
  deriving Functor
```

----

```haskell
data State s m k
  = Get (s -> k)
  | Put s k
  deriving Functor
```

----

```haskell
data Free r k
  = Pure k
  | Impure (Union r (Free r) k)
  deriving (Functor, Applicative) via WrappedMonad (Free r)
```

----

What is a functor, really? Just a value in some sort of context.

If the functor itself is existential, the only thing you CAN do with it is fmap;
not ever inspect or modify the context.

----

```haskell
class Effect e where
  weave :: Functor tk
        => tk ()
        -> (∀ x. tk (m x) -> n (tk x))
        -> e m a
        -> e n (tk a)
```

----

```haskell
instance Effect (State s) where
  weave tk _ (Get k)   = Get   $ \s -> k s <$ tk
  weave tk _ (Put s k) = Put s $       k   <$ tk
```

----

```haskell
runState :: s -> Free (State s ': r) a -> Free r (s, a)
runState s (Pure a) = pure (s, a)
runState s (Impure u) =
  case decomp u of
    Left other -> Impure $
      weave (s, ())
            (\(s, m) -> runState s m)
            other
    Right (Get k)    -> pure (s,  k s)
    Right (Put s k) -> pure (s, k)

decomp
    :: Union (e ': r) m a
    -> Either (Union r m a) (e m a)
```

runState is recursive. GHC wont listen to you if you ask it to
inline these definitions.

we can break the recursion by hand

----

```haskell
runState :: s -> Free (State s ': r) a -> Free r (s, a)
runState s (Pure a) = pure (s, a)
runState s (Impure u) =
  case decomp u of
    Left other -> Impure $
      weave (s, ())
            (\(s, m) -> runState_b s m)
            other
    Right (Get k)    -> pure (s,  k s)
    Right (Put s k) -> pure (s, k)
{-# INLINE runState #-}

runState_b :: s -> Free (State s ': r) a -> Free r (s, a)
runState_b = runState
{-# NOINLINE runState_b #-}
```

----

now the inliner is happy

----

higher order effects are key, because without them people complain "you can't
even write bracket"

----

There are two problems here.

----

1) Writing `Effect` instances is boilerplate!
2) `weave` changes the return type :(

----

Problem 1

```haskell
data Yo e m a where
  Yo :: Functor tk
     => e m a
     -> tk ()
     -> (forall x. tk (m x) -> n (tk x))
     -> (tk a -> b)
     -> Yo e n b
```

`Yo` is the free `Effect`!

----

```haskell
liftYo :: Functor m => e m a -> Yo e m a
liftYo e = Yo e (Identity ())
                (fmap Identity . runIdentity)
                runIdentity
```

----

```haskell
instance Effect (Yo e) where
  weave tk' distrib' (Yo e tk distrib f) =
    Yo e (Compose $ tk <$ tk')
         (fmap Compose . distrib' . fmap distrib . getCompose)
         (fmap f . getCompose)
```

----

Somewhat amazingly, this works!

But all it means is we've delayed giving a meaning for `Effect` until we need to
interpret it.

----

Problem 2

The type of `runFreer` doesn't allow us to change the return type.

```haskell
runFreer
    :: ∀ m. Monad m
    => (∀ x. Union r (Freer r) x -> m x)
    -> m a
```

----

Non-solutions

```haskell
runFreer
    :: ∀ m tk. (Monad m, Functor tk)
    => (∀ x. Union r (Freer r) x -> m (tk x))
    -> m (tk a)
```

Unfortunately this is no longer a `Monad`!

----

Recall that we're allowed to pick *any* `Monad`.

Instead of evaluating to the final monad `m`...

----

Just transform it into `StateT s m` and immediately evaluate *that*!

```haskell
import qualified Control.Monad.Trans.State as S

runState
    :: s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
runState s (Semantic m) = Semantic $ \nt ->
  S.runStateT s $ m $ \u ->
    case decomp u of
      Left x -> S.StateT $ \s' ->
        nt . weave (s', ()) (uncurry $ runState f)
           $ x
      Right (Yo Get _ f)      -> fmap f $ S.get
      Right (Yo (Put s') _ f) -> fmap f $ S.put s'
```

----

We've solved all of the problems.

But what we've built isn't yet a joyful experience.

In particular, dealing with `Yo` is painful.

----

We can clean up the mess of writing effect handlers...

. . .

...

. . .

...with an effect-handler effect!

---

```haskell
data Tactics tk n r m a where
  GetInitialState     :: Tactics tk n r m (tk ())
  HoistInterpretation :: (a -> n b)
                      -> Tactics tk n r m (tk a -> Semantic r (tk b))
```

- `GetInitialState` is the `tk ()` parameter

. . .

- `HoistInterpretation` is the distribution law

----

```haskell
type WithTactics e tk m r = Tactics tk m (e ': r) ': r
```

. . .

```haskell
pureT
   :: a
   -> Semantic (WithTactics e tk m r) (tk a)

runT
    :: m a
    -> Semantic (WithTactics e tk m r)
                (Semantic (e ': r) (tk a))

bindT
    :: (a -> m b)
    -> Semantic (WithTactics e tk m r)
                (tk a -> Semantic (e ': r) (tk b))
```

----

This is where we stop. We've now simultaneously solved the boilerplate and
performance problems, as well as put a friendly UX around the whole thing.

----

I'd like to leave you with a comparison.

First, `fused-effects`, the incumbent library, and it's implementation of the
`Resource` effect:

----

```haskell

data Resource m k
  = forall resource any output.
      Resource (m resource)
               (resource -> m any)
               (resource -> m output)
               (output -> k)

deriving instance Functor (Resource m)

instance HFunctor Resource where
  hmap f (Resource acquire release use k) =
    Resource (f acquire) (f . release) (f . use) k

instance Effect Resource where
  handle state handler (Resource acquire release use k)
    = Resource (handler (acquire <$ state))
               (handler . fmap release)
               (handler . fmap use)
               (handler . fmap k)

bracket :: (Member Resource sig, Carrier sig m)
        => m resource
        -> (resource -> m any)
        -> (resource -> m a)
        -> m a
bracket acquire release use =
  send (Resource acquire release use pure)

runResource :: (forall x . m x -> IO x)
            -> ResourceC m a
            -> m a
runResource handler = runReader (Handler handler) . runResourceC

newtype ResourceC m a = ResourceC
  { runResourceC :: ReaderC (Handler m) m a
  }
  deriving ( Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans ResourceC where
  lift = ResourceC . lift

newtype Handler m = Handler (forall x . m x -> IO x)

runHandler :: Handler m -> ResourceC m a -> IO a
runHandler h@(Handler handler) = handler . runReader h . runResourceC

instance (Carrier sig m, MonadIO m) =>
      Carrier (Resource :+: sig) (ResourceC m) where
  eff (L (Resource acquire release use k)) = do
    handler <- ResourceC ask
    a <- liftIO (Exc.bracket
      (runHandler handler acquire)
      (runHandler handler . release)
      (runHandler handler . use))
    k a
  eff (R other) = ResourceC (eff (R (handleCoercible other)))
```

----

Compare to `polysemy`:

----


```haskell

data Resource m a where
  Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b

makeSemantic ''Resource


runResource
    :: Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Resource ': r) a
    -> Semantic r a
runResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let runIt = finish .@ runResource
    sendM $ X.bracket (runIt a) (runIt . d) (runIt . u)
```

----

Thanks for listening!

Questions?


