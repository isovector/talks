{-# LANGUAGE QuasiQuotes #-}

module Main where

import Polysemy
import Polysemy.Slide
import Polysemy.Code
import Polysemy.Trace


slideDeck
    :: ( Member Code r
       , Member Slide r
       , Member Trace r
       )
    => Semantic r ()
slideDeck = do
  slide "Free Monads" $ do
    [decs|
      data Free f a
        = Pure a | Impure (f (Free f a))
      |]

  slide "Free Monads" $ do
    [decs|
      instance Functor f => Functor (Free f) where
        fmap f (Pure a) = Pure $ f a
        fmap f (Impure z) = Impure $ fmap (fmap f) z
      |]

main :: IO ()
main = runM . runPrintSlides . runPrintCode . runTraceIO $ slideDeck