:title: Don't Eff It Up
:data-transition-duration: 150
:css: presentation.css

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

.. raw:: html

  <pre>
  <span class="new">data Mode = ForReal | Test (IORef Int)</span>

  withdraw :: ( MonadIO     m
              , MonadLogger m
              )
           => <span class="new">Mode</span>
           -> Int
           -> m (Maybe Int)

  withdraw mode desired = do
    <span class="new">amount <- case mode of
                ForReal      -> </span>getCurrentBalance<span class="new">
                Test (ioref) -> liftIO $ readIORef ioref</span>
    if amount < desired
       then do
         log "not enough funds"
         return Nothing

       else do
         <span class="new">let putAction =
               case mode of
                 ForReal      -> </span>putCurrentBalance<span class="new">
                 Test (ioref) -> liftIO . writeIORef ioref</span>
         putAction $ amount - desired
         return $ Just amount
  </pre>

----

.. code:: haskell

  class Monad m => MonadBank m where
    getCurrentBalance :: m Int
    putCurrentBalance :: Int -> m ()

----

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

.. code:: haskell

  newtype IOBankT m a = IOBankT
    { runIOBankT :: IdentityT m a
    }

----

.. code:: haskell

  {-# LANGUAGE GeneralizedNewtypeDeriving #-}

  newtype IOBankT m a = IOBankT
    { runIOBankT :: IdentityT m a
    }
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

.. code:: haskell

  instance MonadIO m => MonadBank (IOBankT m) where
    getCurrentBalance = ...
    putCurrentBalance = ...

----

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

.. code:: haskell

  data Bank a = ...

  class Monad m => MonadBank m where
    liftBank :: Bank a -> m a

----

.. raw:: html

  <pre>
  withdraw :: ( <span class="new">Member Bank   effs</span>
              , <span class="new">Member Logger effs</span>
              )
           => Int
           -> <span class="new">Eff effs</span> (Maybe Int)

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

.. code:: haskell

  withdraw :: ( MonadBank   m
              , MonadLogger m
              )
           => Int
           -> m (Maybe Int)


  withdraw :: ( Member Bank   effs
              , Member Logger effs
              )
           => Int
           -> Eff effs (Maybe Int)

----

.. code:: haskell

  {-# LANGUAGE GADTs #-}

  data Bank a where
    GetCurrentBalance :: Bank Int
    PutCurrentBalance :: Int -> Bank ()


  getCurrentBalance :: Member Bank effs
                    => Eff effs Int
  getCurrentBalance = send GetCurrentBalance


  putCurrentBalance :: Member Bank effs
                    => Int
                    -> Eff effs ()
  putCurrentBalance amount = send $ PutCurrentBalance amount

----

.. code:: haskell

  {-# LANGUAGE TemplateHaskell #-}

  data Bank a where
    GetCurrentBalance :: Bank Int
    PutCurrentBalance :: Int -> Bank ()

  makeFreer ''Bank

----

.. code:: haskell

  data Logger a where
    Log :: String -> Logger ()

  makeFreer ''Logger

----

.. code:: haskell

  withdraw :: ( Member Bank   effs
              , Member Logger effs
              )
           => Int
           -> Eff effs (Maybe Int)

----

.. code:: haskell

  > :kind Eff

  Eff :: [* -> *] -> * -> *

----

.. code:: haskell

  StateT s (ReaderT r IO) a

.. code:: haskell

  Eff '[State s, Reader r, IO] a

----

.. code:: haskell

  runM :: Monad m => Eff '[m] a -> m a

----

.. code:: haskell

  run :: Eff '[] a -> a

----

.. code:: haskell

  -- TODO(sandy): split this up and highlight things
  runLogger :: Member IO effs
            => Eff (Logger ': effs) a
            -> Eff effs a

  runLogger = runNat nat
    where
      nat :: Logger x -> IO x
      nat (Log s) = putStrLn s

----

.. code:: haskell

  runBank :: Member IO effs
          => Eff (Bank ': effs) a
          -> Eff effs a

  runBank = runNat nat
    where
      nat :: Bank x -> IO x
      nat GetCurrentBalance            = -- do something in IO and return an Int
      nat (PutCurrentBalance newValue) = -- do something in IO and return ()

----

.. code:: haskell

  > :t runM . runLogger . runBank

  Eff '[Bank, Logger, IO] a -> IO a



  > :t runM . runLogger . runBank $ withdraw 50

  IO (Maybe Int)

----

.. code:: haskell

  ignoreLogger :: forall effs a
                . Eff (Logger ': effs) a
               -> Eff effs a

  ignoreLogger = handleRelay pure bind
    where
      bind :: forall x
            . Logger x
           -> (x -> Eff effs a)
           -> Eff effs a
      bind (Log _) continueWith = continueWith ()

----

.. code:: haskell

  testBank :: forall effs a
             . Int
            -> Eff (Bank ': effs) a
            -> Eff effs a

  testBank balance = handleRelayS balance pure bind
    where
      bind :: forall x
            . Int
           -> Bank x
           -> (Int -> x -> Eff effs a)
           -> Eff effs a
      bind s GetCurrentBalance      continueWith = continueWith s  s
      bind _ (PutCurrentBalance s') continueWith = continueWith s' ()

----

.. code:: haskell

  > :t run . ignoreLogger . testBank

  Eff '[Bank, Logger] a -> a



  > :t run . ignoreLogger . testBank $ withdraw 50

  Maybe Int

----

.. code:: haskell

  data Bank a where
    GetCurrentBalance :: Bank Int
    PutCurrentBalance :: Int -> Bank ()


  data Logger a where
    Log :: String -> Logger ()

----

.. raw:: html

  <pre>
  data <span class="new">State s</span> a where
    Get :: State s <span class="new">s</span>
    Put :: <span class="new">s</span> -> State s ()


  data <span class="new">Writer w</span> a where
    Tell :: <span class="new">w</span> -> Writer w ()
  </pre>

----

.. raw:: html

  <pre>
  <span class="new">{-# LANGUAGE ScopedTypeVariables #-}</span>

  withdraw :: ( Member <span class="new">(State Int)</span>     effs
              , Member <span class="new">(Writer String)</span> effs
              )
           => Int
           -> Eff effs (Maybe Int)

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

