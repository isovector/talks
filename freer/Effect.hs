module Effect where

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


