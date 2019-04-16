{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UnicodeSyntax         #-}

import Control.Monad.Trans.Reader
import Control.Monad

newtype WrappedMonad m a = WrappedMonad (m a)

instance Functor (WrappedMonad m) where
  fmap = undefined
instance Applicative (WrappedMonad m) where
  (<*>) = undefined
  pure = undefined


data Teletype k
  = Pure k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)
  deriving (Functor, Applicative) via WrappedMonad Teletype

instance Monad Teletype where
  return = Pure
  Pure          k >>= f = f k
  WriteLine msg k >>= f = WriteLine msg $ k >>= f
  ReadLine      k >>= f = ReadLine $ \str -> k str >>= f

echo :: Teletype ()
echo =         ReadLine
     $ \msg -> WriteLine msg
     $ Pure ()

echo2 :: Teletype ()
echo2 = do
  msg <- ReadLine pure
  WriteLine msg $ pure ()


runTeletypeInIO :: Teletype a -> IO a
runTeletypeInIO (Pure a) = pure a
runTeletypeInIO (WriteLine msg k) = do
  putStrLn msg
  runTeletypeInIO k
runTeletypeInIO (ReadLine k) =  do
  msg <- getLine
  runTeletypeInIO $ k msg


runTeletypePurely :: [String] -> Teletype a -> ([String], a)
runTeletypePurely _ (Pure a) = ([], a)
runTeletypePurely ls (WriteLine msg k) =
  let (rs, a) = runTeletypePurely ls k
   in (msg : rs, a)
runTeletypePurely []       (ReadLine k) =  runTeletypePurely [] $ k ""
runTeletypePurely (l : ls) (ReadLine k) =  runTeletypePurely ls $ k l


data Free f k
  = Pure' k
  | Impure (f (Free f k))
  deriving (Functor, Applicative) via WrappedMonad (Free f)

instance Functor f => Monad (Free f) where
  return = Pure'
  Pure' k  >>= f = f k
  Impure z >>= f = Impure $ fmap (\x -> x >>= f) z


data Teletype' a
  = WriteLine' String a
  | ReadLine' (String -> a)
  deriving Functor

writeLine :: String -> Free Teletype' ()
writeLine msg = Impure $ WriteLine' msg $ pure ()

readLine :: Free Teletype' String
readLine = Impure $ ReadLine' pure

echo' :: Free Teletype' ()
echo' = forever $ do
  msg <- readLine
  writeLine msg


runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
runFree _ (Pure' a)  = pure a
runFree f (Impure k) = f k >>= runFree f


runTeletypeInIO' :: Free Teletype' a -> IO a
runTeletypeInIO' = runFree $ \case
  WriteLine' msg k -> do
    putStrLn msg
    pure k
  ReadLine' k -> do
    msg <- getLine
    pure $ k msg


data Sum f g a
  = L (f a)
  | R (g a)
  deriving Functor


data Bell k
  = RingBell k
  deriving Functor



writeLine' :: String -> Free (Sum Teletype' Bell) ()
writeLine' msg = Impure $ L $ WriteLine' msg $ pure ()

readLine' :: Free (Sum Teletype' Bell) String
readLine' = Impure $ L $ ReadLine' pure

ringBell :: Free (Sum Teletype' Bell) ()
ringBell = Impure $ R $ RingBell $ pure ()


ringItSingIt :: Free (Sum Teletype' Bell) String
ringItSingIt = do
  msg <- readLine'
  when (msg == "ring the bell!") ringBell
  pure msg

type IndexOf a b c = c

data Union r a where
  Union :: (IndexOf r n) a -> Union r a
  -- functor whenever everything in r is

class Member f r where
  inj  :: f a       -> Union r a
  proj :: Union r a -> Maybe (f a)


writeLine'' :: Member Teletype' r => String -> Free (Union r) ()
writeLine'' msg = Impure $ inj $ WriteLine' msg $ pure ()

readLine'' :: Member Teletype' r => Free (Union r) String
readLine'' = Impure $ inj $ ReadLine' pure

ringBell'' :: Member Bell r => Free (Union r) ()
ringBell'' = Impure $ inj $ RingBell $ pure ()


-- runFree
--     :: Monad m
--     => (∀ x. f x -> m x)
--     -> Free f a
--     -> m a
-- runFree
--     :: Free f a
--     -> ∀ m. Monad m
--     => (∀ x. f x -> m x)
--     -> m a
-- runFree
--     :: Free r a
--     -> ∀ m. Monad m
--     => (∀ x. Union r x -> m x)
--     -> m a


newtype Freer r a = Freer
  { runFreer
        :: ∀ m. Monad m
        => (∀ x. Union r x -> m x)
        -> m a
  }
  deriving (Functor, Applicative) via WrappedMonad (Freer r)

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad (Freer f) where
  return a = Freer $ \nt -> pure a
  m >>= f = Freer $ \nt -> do
    a <- runFreer m nt
    runFreer (f a) nt


-- instance (Monad m) => Monad (ReaderT r m) where
--     return  = lift . return
--     m >>= k = ReaderT $ \ r -> do
--         a <- runReaderT m r
--         runReaderT (k a) r




liftFreer :: Member f r => f a -> Freer r a
liftFreer fa = Freer $ \nt -> nt $ inj fa

data Teletype'' a where
  WriteLine'' :: String -> Teletype'' ()
  ReadLine'' :: Teletype'' String

data Bell'' a where
  RingBell'' :: Bell'' ()

writeLine''' :: Member Teletype'' r => String -> Freer r ()
writeLine''' msg = liftFreer $ WriteLine'' msg

readLine''' :: Member Teletype'' r => Freer r String
readLine''' = liftFreer ReadLine''

ringBell''' :: Member Bell'' r => Freer r ()
ringBell''' = liftFreer RingBell''


----


data Error e m k
  = Throw e
  | ∀ x. Catch (m x)
               (e -> m x)
               (x -> k)


data State s (m :: * -> *) k
  = Get (s -> k)
  | Put s k

deriving instance Functor (Error e m)
deriving instance Functor (State s m)


class Effect e where
  weave :: Functor tk
        => tk ()
        -> (∀ x. tk (m x) -> n (tk x))
        -> e m a
        -> e n (tk a)

instance Effect (State s) where
  weave tk _ (Get k)   = Get   $ \s -> k s <$ tk
  weave tk _ (Put s k) = Put s $       k   <$ tk

instance Effect (Error e) where
  weave _ _ (Throw e) = Throw e
  weave tk distrib (Catch try handle k) =
    Catch (distrib $ try <$ tk)
          (\e -> distrib $ handle e <$ tk)
          (fmap k)

data Union' (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union' :: (IndexOf r n) m a -> Union' r m a

instance Effect (Union' r) where
  weave _ _ = undefined

class Member' e r where
  inj'  :: e m a        -> Union' r m a
  proj' :: Union' r m a -> Maybe (e m a)

decomp
    :: Union' (e ': r) m a
    -> Either (Union' r m a)
              (e m a)

decomp = undefined

data Free' r k
  = Pure'' k
  | Impure'' (Union' r (Free' r) k)
  deriving (Functor, Applicative) via WrappedMonad (Free' r)

instance Monad (Free' r)


runState :: s -> Free' (State s ': r) a -> Free' r (s, a)
runState s (Pure'' a) = pure (s, a)
runState s (Impure'' u) =
  case decomp u of
    Left other -> Impure'' $
      weave (s, ())
            (\(s', m) -> runState s' m)
            other
    Right (Get k)    -> pure (s,  k s)
    Right (Put s' k) -> pure (s', k)

runError :: Free' (Error e ': r) a -> Free' r (Either e a)
runError (Pure'' a) = pure $ Right a
runError (Impure'' u) =
  case decomp u of
    Left other -> Impure'' $
      weave (Right ())
            (\case
              Left e  -> pure $ Left e
              Right m -> runError m
            )
            other

    Right (Throw e) -> pure $ Left e
    Right (Catch try handle k) -> do
      tried <- runError try
      case tried of
        Left e -> do
          handled <- runError $ handle e
          case handled of
            Left e' -> pure $ Left e'
            Right a -> pure $ Right $ k a
        Right a -> pure $ Right $ k a





-- runFree'
--     :: Monad m
--     => (∀ x. Union' r (Free' r) x -> m x)
--     -> Free' r a
--     -> m a
-- runFree' _ = undefined


