{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.Bullets where

import Data.Foldable
import Polysemy
import Polysemy.Effect.New
import Polysemy.Utils



data Bullets m a
  = BulletPoints [String] a
  deriving (Functor, Effect)

makeSemantic ''Bullets


bullets :: QuasiQuoter
bullets = sendQQ 'bulletPoints paragraphs

runPrintBullets :: Member (Lift IO) r => InterpreterOf r Bullets
runPrintBullets = interpret $ \case
  BulletPoints points k -> do
    for_ points $ \p ->
      sendM . putStrLn $ "* " ++ p
    pure k

