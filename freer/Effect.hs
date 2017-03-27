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

if it isn't a newtype, you'll also need to define your own instance of Monad and friends, AND prove that they follow the laws

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

a brief note: there is another widely used approach in the vein of `MonadIO` - make a datatype and let your TC lift into it

data Bank a = ...

class MonadBank m where
  liftBank :: Bank a -> m a

this is used to avoid the O(n^2) mtl instances, because it means you only need to write a MonadBank instance for your monomorphic monad stack

BUT that comes at a cost! by writing monomorphic instances we can't be polymorphic anymore. it's less effort per-interpreter at the cost of losing composability

------------------------------------------------------------------------------

these are the only approaches i'm aware of commonly used in the hs ecosystem, and it's pretty obvious that none of them is particularly good.

------------------------------------------------------------------------------

the problem, as it turns out, is pretty fundamental. the problem is that monads don't compose, and that monad transformers are our hack on top of them in order to get something that DOES compose.

monad transformers are the problem, which kind of makes sense when we think about it; each of our approaches was stymied by the boilerplate and/or lack of polymorphism

------------------------------------------------------------------------------

what we really want is a single monad (not a transformer) that allows us to inject functionality at will, and determine what that functionality MEANS when we want to run it.

it's what we've been trying to do this whole time; our tools just weren't powerful enough to give it to us.

------------------------------------------------------------------------------

enter the freer monad.

our first try makes a few syntactic changes:

------------------------------------------------------------------------------

withdraw :: ( Member Bank   effs
            , Member Logger effs
            )
         => Int
         -> Eff effs (Maybe Int)
withdraw desired = do
  amount <- getCurrentBalance
  if amount < desired
     then do
       log "not enough funds"
       return Nothing

     else do
       setCurrentBalance $ amount - desired
       return $ Just amount

------------------------------------------------------------------------------

besides the type signature, this definition is identical to the code we would
have written in the mtl MonadBank case

let's compare type signatures

------------------------------------------------------------------------------

withdraw :: ( MonadBank m
            , MonadLogger m
            )
         => Int
         -> m (Maybe Int)

withdraw :: ( Member Bank   effs
            , Member Logger effs
            )
         => Int
         -> Eff effs (Maybe Int)

------------------------------------------------------------------------------

it's pretty clear just from the types here that there is less ad-hoc stuff going on

instead of being polymorphic over `m`, we're now polymorphic over some parameter to Eff

and we've decomposed `MonadBank m` into `Member Bank effs`

things that used to be nominally typed are now legitimate type applications, which feels like it should buy us something

------------------------------------------------------------------------------

and in fact, it does. let's look at the boilerplate necessary to implement the `Bank` type, which is the only thing missing

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

data Bank a where
  GetCurrentBalance :: Bank Int
  SetCurrentBalance :: Int -> Bank ()

getCurrentBalance :: Member Bank effs => Eff effs Int
getCurrentBalance = send GetCurrentBalance

setCurrentBalance :: Member Bank effs => Int -> Eff effs ()
setCurrentBalance amount = send $ SetCurrentBalance amount

------------------------------------------------------------------------------

that's it! you're done. so what's going on here?

------------------------------------------------------------------------------

we create a GADT for our effect, whose constructors describe the actions we're allowed to do

------------------------------------------------------------------------------

and then we create "action" kleisli functions to create Eff monadic actions out of our constructors

------------------------------------------------------------------------------

and if this is still too much work for you, I wrote some TH (yet to be merged) to generate them for you:

data Bank a where
  GetCurrentBalance :: Bank Int
  SetCurrentBalance :: Int -> Bank ()

makeFreer ''Bank

------------------------------------------------------------------------------

for completeness, we'll define the logger too:

data Logger a where
  Log :: String -> Logger ()

makeFreer ''Logger

------------------------------------------------------------------------------

recall our signature:

withdraw :: ( Member Bank   effs
            , Member Logger effs
            )
         => Int
         -> Eff effs (Maybe Int)

as it turns out, all of this stuff will compile now. but the question remains: what's going on?

------------------------------------------------------------------------------

it can be instructive to look at the kind of Eff:

Eff :: [* -> *] -> * -> *

wat

if you're not familiar with promoted types and their friends data kinds:

Eff takes a type-level list of type constructors of kind * -> *

------------------------------------------------------------------------------

this is what our `effs` thing has been all along.

this list corresponds exactly to your monad transformer stack

StateT s (ReaderT r IO) <==> '[State s, Reader r, IO]

------------------------------------------------------------------------------

when we say `Member Bank effs` we're saying we don't care exactly where `Bank` is in effs, so long as its somewhere. in this way, we keep our "stack" polymorphic and can add constraints for things that must be in it

------------------------------------------------------------------------------

great. but we still have the problem that `main` is in IO, and not in Eff. so how do we go from one to the other?

------------------------------------------------------------------------------

freer gives us two special functions:

runM :: Monad m => Eff '[m] a -> m a

which says that if `m` is the only thing in our eff stack, then we can lose the Eff shell and just get back that monad

------------------------------------------------------------------------------

often times in testing, you actually don't want any effectful monads whatsoever, and so freer also gives us

run :: Eff '[] a -> a

which says that if we have an eff stack with no effects, we can just get the computed value out of it

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


