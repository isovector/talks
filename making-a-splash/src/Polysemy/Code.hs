{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.Code where

import qualified Language.Haskell.Exts.QQ as QQ
import qualified Language.Haskell.Exts.Simple as HSE
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import           Polysemy
import           Polysemy.Effect.New
import           Polysemy.Utils



data Code m a
  = CodeType HSE.Type a
  | CodeDecs [HSE.Decl] a
  deriving (Functor, Effect)

makeSemantic ''Code


sendQQ :: TH.Name -> QuasiQuoter -> QuasiQuoter
sendQQ n qq = QuasiQuoter
  { quoteExp = \str -> do
      z <- quoteExp qq str
      pure $ TH.AppE (TH.VarE n) z
  , quotePat = error "bad qq"
  , quoteType = error "bad qq"
  , quoteDec = error "bad qq"
  }


ty :: QuasiQuoter
ty = sendQQ 'codeType QQ.ty

decs :: QuasiQuoter
decs = sendQQ 'codeDecs QQ.decs


codeHerald :: IO () -> IO ()
codeHerald m = do
  putStrLn "\\begin{haskell}"
  m
  putStrLn "\\end{haskell}\n"


runPrintCode
    :: Member (Lift IO) r
    => InterpreterOf r Code
runPrintCode = interpret $ sendM .  \case
  CodeType t k   -> do
    codeHerald $ putStrLn $ HSE.prettyPrint t
    pure k
  CodeDecs d k -> do
    codeHerald $ putStr $ unlines $ fmap HSE.prettyPrint d
    pure k







-- -- slideDeck
-- --     :: ( Member Code r
-- --        , Member Slide r
-- --        , Member Trace r
-- --        )
-- --     => Semantic r ()
-- -- slideDeck = do
-- --   newSlide "Free Monads" $ do
-- --     codeDecs [decs|
-- --       data Free f a
-- --         = Pure a | Impure (f (Free f a))
-- --       |]

-- --   newSlide "Free Monads" $ do
-- --     codeDecs [decs|
-- --       instance Functor f => Functor (Free f) where
-- --         fmap f (Pure a) = Pure $ f a
-- --         fmap f (Impure z) = Impure $ fmap (fmap f) z
-- --       |]


-- -- main :: IO ()
-- -- main = runM . runTraceIO . runSlides . runPrettyCodeLiteral $ slideDeck
