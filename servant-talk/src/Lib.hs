{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Lib
    ( main
    ) where

import Servant
import Servant.Server
import Network.Wai.Handler.Warp
import Data.IORef
import System.IO.Unsafe
import Control.Monad.IO.Class
import Debug.Trace

showTrace = trace =<< show

globalCounter :: IORef Int
globalCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE globalCounter #-}

type API = "counter" :> Get '[JSON] Int
      :<|> "counter" :> QueryParam "set" Int :> Post '[JSON] ()

getHandler :: Handler Int
getHandler = liftIO $ readIORef globalCounter

putHandler :: Maybe Int -> Handler ()
putHandler Nothing = pure ()
putHandler (Just i) = liftIO $ writeIORef globalCounter $ showTrace i


main :: IO ()
main = do
  run 8080 $ serve (Proxy @API) $ getHandler :<|> putHandler
