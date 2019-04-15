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
  { unFreer
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

----

```haskell
instance Effect (Error e) where
  weave _ _ (Throw e) = Throw e
  weave tk distrib (Catch try handle k) =
    Catch (distrib $ try <$ tk)
          (\e -> distrib $ handle e <$ tk)
          (fmap k)
```

----

```haskell
runError :: Free (Error e ': r) a -> Free r (Either e a)
runError (Pure a) = pure $ Right a
runError (Impure u) =
  case decomp u of
    Left other -> Impure $
      weave (Right ())
            (\case
              Left e  -> pure $ Left e
              Right m -> runError m
            )
            other
```

----

```haskell
-- runError continuned
  Right (Throw e) -> pure $ Left e

  Right (Catch try handle k) -> do
    tried <- runError try
    case tried of
      Right a -> pure $ Right $ k a

      Left e -> do
        handled <- runError $ handle e
        case handled of
          Right a -> pure $ Right $ k a
          Left e -> pure $ Left e
```

----

runState and runError are recursive. GHC wont listen to you if you ask it to
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



