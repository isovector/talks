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

data Writer w result where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell w = send $ Tell w

------------------------------------------------------------------------------

withdraw :: ( Member (State Int) effs
            , Member (Writer String) effs
            )
         => Int
         -> Eff effs (Maybe Int)
withdraw desired = do
  amount <- get
  if amount < desired
     then do
       tell "not enough funds"
       return Nothing

     else do
       modify (subtract desired)
       return $ Just amount

------------------------------------------------------------------------------

