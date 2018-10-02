{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

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

-- type API = ..

-- {{{

-- getHandler :: Handler Int
-- getHandler = _

-- {{{

-- myAPI :: Proxy API
-- myAPI = Proxy

-- main :: IO ()
-- main = run 8080
--      . serve myAPI
--      $ getHandler

-- {{{

-- incrHandler :: Handler ()
-- incrHandler = _

-- {{{

-- getClient, modifyClient

-- {{{

-- incrHandler

-- {{{

-- type Annotated api

-- {{{

-- serveAnnotated
--     :: forall api
--      . HasServer (Annotated api) '[]
--     => Proxy api
--     -> ServerT api Handler
--     -> Application

-- {{{
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
-- }}}
-- }}}}}}}}}}}}}}}}}}}}}

