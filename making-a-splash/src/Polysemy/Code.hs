{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.Code where

import qualified Language.Haskell.Exts.QQ as QQ
import qualified Language.Haskell.Exts.Simple as HSE
import           Polysemy
import           Polysemy.Effect.New
import           Polysemy.Utils



data Code m a
  = CodeType HSE.Type a
  | CodeDecs [HSE.Decl] a
  deriving (Functor, Effect)

makeSemantic ''Code


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

