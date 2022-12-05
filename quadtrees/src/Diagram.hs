{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Diagram where

import NonNaive hiding (scale)
import Diagrams.Prelude hiding (atLeast, E)
import Diagrams.Backend.Rasterific
import Naive hiding (fuse, fill)
import Data.Ratio (approxRational)
import Debug.Trace (traceShowId, traceM)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Test.QuickCheck (arbitrary, oneof, generate, vectorOf, Arbitrary, resize, choose)
import Data.List (tails, zip5, inits)
import Test.QuickCheck.Modifiers


draw :: Tree (Colour Double) -> Diagram B
draw (Fill co) = rect 1 1 # fc co # lwL 0.08
draw (Split (Quad tl tr bl br)) =
  let redraw q =  draw q # scale 0.5
   in
    (redraw tl # alignBR ||| redraw tr # alignBL)
    ===
    (redraw bl # alignTR ||| redraw br # alignTL)

drawQT :: QT (Colour Double) -> Diagram B
drawQT (QT co n qt) = mconcat
  [ texterific (show $ 2 ^ (n+1)) # scale 0.07 # center # translateY (0.43)
  , texterific (show $ 2 ^ (n+1)) # scale 0.07 # center # rotate (90 @@ deg) # translateX (-0.43)
  , draw qt # scale 0.7
  , rect 1 1 # fc co
  ]

orbits :: IO ()
orbits = animatedGif "/tmp/yo.gif" (dims 512) LoopingForever 10 $ do
  let n :: Num a => a
      n = 72
  t <- [ 0 .. n - 1 ]
  let f amp eccx eccy =
          Quad (traceShowId $ approxRational (amp * (cos $ fromIntegral t * pi / eccx)) 0.5)
               (traceShowId $ approxRational (amp * (sin $ fromIntegral t * pi / eccy)) 0.5)
      sq = fill Nothing (Just blue) $ f 10 12 18 2 2
      ci = fill Nothing (Just green) $ f 10 6 6 4 4
  pure $ drawQT $ fmap (fromMaybe white) $ fuse $ atLeast 4 (liftA2 (<|>) sq ci)

instance Arbitrary (Colour Double) where
  arbitrary = sRGB <$> choose (0, 1) <*> choose (0, 1) <*> choose (0, 1)
    -- [ red
    -- , green, blue, yellow, orange, purple, aquamarine, aqua, brown, teal, pink
    -- ]

main :: IO ()
main = do
  let n = 50
  xs <- generate $ vectorOf n $ choose (-32, 16)
  ys <- generate $ vectorOf n $ choose (-32, 16)
  ws <- generate $ vectorOf n $ choose (1, 16)
  hs <- generate $ vectorOf n $ choose (1, 16)
  cols <- generate $ vectorOf n $ arbitrary @(Colour Double)
  let res = zip5 xs ys ws hs cols


  animatedGif "/tmp/yo.gif" (dims 512) LoopingForever 25 $ do
    stuff <- drop 1 $ inits res
    pure
      $ drawQT
      $ fmap (fromMaybe white)
      $ fuse
      $ foldr overlay (pure Nothing)
      $ fmap (\(x, y, w, h, c) -> fill Nothing (Just c) $ fmap (fromIntegral @Int) $ Quad x y w h
             ) stuff

