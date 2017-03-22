{-# LANGUAGE GADTs #-}

module Effect where

data State s result where
  Get :: State s s
  Put :: s -> State s ()


get :: Member (State s) effs => Eff effs s
get = send $ Get

put :: Member (State s) effs => s -> Eff effs ()
put s = send $ Put s

