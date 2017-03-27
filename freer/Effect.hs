module Effect where

------------------------------------------------------------------------------

withdrawMTL :: ( MonadIO m
               , MonadLogger m
               )
            => Int
            -> m (Maybe Int)
withdrawMTL desired = do
  amount <- getCurrentBalance
  if amount < desired
     then do
       log "not enough funds"
       return Nothing

     else do
       setCurrentBalance $ amount - desired
       return $ Just amount

------------------------------------------------------------------------------

-- how can we test this??

------------------------------------------------------------------------------

data Mode = ForReal | Test (IORef Int)

withdrawMTL :: ( MonadIO m
               , MonadLogger m
               )
            => Mode
            -> Int
            -> m (Maybe Int)
withdrawMTL mode desired = do
  amount <- case mode of
              ForReal      -> getCurrentBalance
              Test (ioref) -> liftIO $ readIORef ioref
  if amount < desired
     then do
       log "not enough funds"
       return Nothing

     else do
       let setAction =
             case mode of
               ForReal      -> setCurrentBalance
               Test (ioref) -> liftIO . writeIORef ioref
       setAction $ amount - desired
       return $ Just amount

------------------------------------------------------------------------------

problems:
* IO is directly exposed, even though we don't super care about IO, we just want to change our bank account
* we've mixed test code directly in with our real logic
* the compiler can't help in convincing ourselves that we mocked out all of the IO
* we had to use an IORef wtf

------------------------------------------------------------------------------

wouldn't it be nice if we could write the program we cared about, and then test them without having to jump through a bunch of stupid hoops?

------------------------------------------------------------------------------

turns out we can. and it's not even hard. it just requires us to do some more THINKING

------------------------------------------------------------------------------

indirection, as always, is the solution. we can push back deciding on how to make this change by making a new monad!

------------------------------------------------------------------------------

class Monad m => MonadBank m where
  getCurrentBalance :: m Int
  setCurrentBalance :: Int -> m ()

------------------------------------------------------------------------------

withdrawMTL2 :: ( MonadBank m
                , MonadLogger m
                )
             => Int
             -> m (Maybe Int)
withdrawMTL desired = do
  amount <- getCurrentBalance
  if amount < desired
     then do
       log "not enough funds"
       return Nothing

     else do
       setCurrentBalance $ amount - desired
       return $ Just amount

------------------------------------------------------------------------------

by keeping it polymorphic in `MonadBank m`, we can let `main` choose which specific instance of `MonadBank` we want, and we can use one for testing and one for the real code

sweet! this seems like it solves our problem. but we've swept the actual work under the rug. in order to get it to actually work, there's some boilerplate:

------------------------------------------------------------------------------

we need to make a carrier newtype to hold the instance:

newtype IOBankT m a = IOBankT { runIOBankT :: IdentityT m a } -- identityt necessary so we can derive MonadTrans

------------------------------------------------------------------------------

and it needs to lift other MTL classes up the ladder

deriving (Functor, Applicative, Monad, MonadReader r, MonadWriter w, MonadState s, MonadIO, ...)

------------------------------------------------------------------------------

and we need an our instance

instance MonadIO m => MonadBank (IOBankT m) where
  getCurrentBalance = ...
  setCurrentBalance = ...

------------------------------------------------------------------------------

AND we need other mtl types to lift OUR class up the ladder

instance MonadBank m => MonadBank (ReaderT r m) where
  getCurrentBalance = lift getCurrentBalance
  setCurrentBalance = lift . getCurrentBalance

instance MonadBank m => MonadBank (WriterT w m) where
  getCurrentBalance = lift getCurrentBalance
  setCurrentBalance = lift . getCurrentBalance

instance MonadBank m => MonadBank (StateT s m) where
  getCurrentBalance = lift getCurrentBalance
  setCurrentBalance = lift . getCurrentBalance

etc

------------------------------------------------------------------------------

luckily the lifting over other mtl types is a one time constant code fee, but the rest of it scales per interpretation of MonadBank

and this is work we need to do every time we'd define a new effect we care about. it's a huge amount of annoying, trivial, boilerplate work.

we all know what happens in that case - people are lazy and can't be fucked to actually write it, so they hardcode in IO and we're back where we started

whatever solution we come up with needs to be EASY and low maintainance if we want any hope of getting it adopted by day-to-day dev cycles

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

data State s result where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) effs => Eff effs s
get = send $ Get

put :: Member (State s) effs => s -> Eff effs ()
put s = send $ Put s

------------------------------------------------------------------------------

modify :: Member (State s) effs
       => (s -> s)
       -> Eff effs ()
modify f = do
  x <- get
  put $ f x

------------------------------------------------------------------------------

twoStates :: ( Member (State Bool)   effs
             , Member (State String) effs
             )
          => Eff effs ()
twoStates = do
  modify not
  modify (++ "hello")

------------------------------------------------------------------------------

data Writer w result where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell w = send $ Tell w

------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

withdraw :: ( Member (State Int)     effs
            , Member (Writer String) effs
            )
         => Int
         -> Eff effs (Maybe Int)
withdraw desired = do
  amount :: Int <- get
  if amount < desired
     then do
       tell "not enough funds"
       return Nothing

     else do
       modify (subtract desired)
       return $ Just amount

------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

writeToStdOut :: (Member IO effs)
              => (w -> String)
              -> Eff (Writer w ': effs) a
              -> Eff effs a
writeToStdOut toStr program = runNat nat program
  where
    nat :: ∀x. Tell w x -> IO x
    nat (Tell w) = putStrLn $ toStr w

------------------------------------------------------------------------------

handleRelay :: (a -> Eff effs b)
            -> (∀x. m x -> (x -> Eff effs b) -> Eff effs b)
            -> Eff (g ': effs) a
            -> Eff effs b

given `b ~ a`, and `m ~ Eff effs`

handleRelay :: (a -> m a)
            -> (∀x. g x -> (x -> m a) -> m a)
            -> Eff (g ': effs) a
            -> m a

------------------------------------------------------------------------------

                                               class Monad m where
handleRelay :: (a -> m a)                        return :: a -> m a
            -> (∀x. g x -> (x -> m a) -> m a)    bind   :: m x -> (x -> m a) -> m a
            -> Eff (g ': effs) a
            -> m a

------------------------------------------------------------------------------

pretendToWrite :: ∀w effs a
                . Eff (Writer w ': effs) a
               -> Eff effs a
pretendToWrite = handleRelay return bind
  where
    bind :: ∀x
          . Writer w x
         -> (x -> Eff effs a)
         -> Eff effs a
    bind (Tell _) continueWith = continueWith ()

------------------------------------------------------------------------------

evalState :: ∀s effs a
           . s
          -> Eff (State s ': effs) a
          -> Eff effs a
evalState s = handleRelayS pure bind
  where
    bind :: ∀x
          . s
         -> State s x
         -> (s -> x -> Eff effs a)
         -> Eff effs a
    bind s Get      cont = cont s s
    bind _ (Put s') cont = cont s' ()


------------------------------------------------------------------------------

main :: IO ()
main = runM . evalState (0 :: Int) . pretendToWrite


