{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( main
    ) where

import Data.Text (Text)
import Magic
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Servant.Docs (ToParam (..), ToSample (..), singleSample, DocQueryParam (..), ParamKind(..))
import Servant.JS
import Servant.Server


globalCounter :: Variable Int
globalCounter = newVariable 0


type API = "counter" :> Get '[JSON] Int
      :<|> "counter" :> QueryParam "set" Int :> Post '[JSON] Int


type Annotated api
      = "api" :> "js"      :> Get '[PlainText] Text
   :<|> "api" :> "swagger" :> Get '[PlainText] Text
   :<|> "api" :> "docs"    :> Get '[PlainText] Text
   :<|> api


getHandler :: Handler Int
getHandler = getVariable globalCounter


modifyHandler :: Maybe Int -> Handler Int
modifyHandler Nothing = getHandler
modifyHandler (Just i) = do
  modifyVariable globalCounter (+ i)
  getHandler

getClient :: ClientM Int
modifyClient :: Maybe Int -> ClientM Int
getClient :<|> modifyClient = client myAPI

myAPI :: Proxy API
myAPI = Proxy

serveAnnotated
    :: forall api
     . HasServer (Annotated api) '[]
    => Proxy api
    -> ServerT api Handler
    -> Application
serveAnnotated api handlers =
  serve (Proxy @(Annotated api))
     $ (pure $ jsForAPI myAPI jquery)
  :<|> (pure $ swaggerForAPI myAPI)
  :<|> (pure $ docsForAPI myAPI)
  :<|> handlers

main :: IO ()
main = run 8080
     . serveAnnotated myAPI
     $ getHandler
  :<|> modifyHandler

{-# NOINLINE globalCounter #-}


instance ToParam (QueryParam "set" Int) where
  toParam _ =
    DocQueryParam
      "set"
      ["integer"]
      "The amount to set the internal counter to"
      Normal

instance ToSample Int where
  toSamples _ = singleSample 0

instance ToSample () where
