{-# LANGUAGE QuasiQuotes #-}

module Main where

import Polysemy
import Polysemy.Bullets
import Polysemy.Code
import Polysemy.Slide
import Polysemy.Trace


slideDeck
    :: ( Member Bullets r
       , Member Code r
       , Member Slide r
       , Member Trace r
       )
    => Semantic r ()
slideDeck = do
  slide "" $ do
    [bullets|
      Making a Splash

      A talk by Sandy Maguire

      https://reasonablypolymorphic.com/talks/making-a-splash
    |]

  slide "" [bullets|
    "I'm not SPJ, so who am I to work on this?"
  |]

  slide "" [bullets|
    But who is SPJ other than a guy who decided to do it?
  |]

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

  slide "Test Prose" $ do
    [bullets|
      So there I was.

      Just waiting.

      Cool. It seems to work.
      I'm great.
    |]


main :: IO ()
main = runM . runPrintSlides . runPrintCode . runTraceIO . runPrintBullets $ slideDeck

