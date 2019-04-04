{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Slide where

import Polysemy
import Polysemy.Effect.New
import Polysemy.Utils

data Slide m a
  = ∀ x. Slide String (m x) (x -> a)

deriving instance Functor (Slide m)

instance Effect Slide where
  weave s distrib (Slide t c k) = Slide t (distrib $ c <$ s) (fmap k)
  hoist = defaultHoist

makeSemantic ''Slide


runPrintSlides :: Member (Lift IO) r => InterpreterOf r Slide
runPrintSlides = interpret $ \case
  Slide t m k -> do
    sendM $ putStrLn $ "# " ++ t ++ "\n"
    a <- runPrintSlides m
    pure $ k a

