module Magic
  ( module Magic
  , BaseUrl (..)
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson (encode)
import Data.IORef
import Data.Proxy
import Data.String.Conv (toS)
import Data.Text (Text, pack)
import Network.HTTP.Client hiding (Proxy)
import Servant.Client
import Servant.Docs (markdown, docs, HasDocs)
import Servant.Swagger
import System.IO.Unsafe

type Variable = IORef

newVariable :: a -> Variable a
newVariable = unsafePerformIO . newIORef

getVariable :: MonadIO m => Variable a -> m a
getVariable = liftIO . readIORef

modifyVariable :: MonadIO m => Variable a -> (a -> a) -> m ()
modifyVariable = fmap liftIO . modifyIORef

setVariable :: MonadIO m => Variable a -> a -> m ()
setVariable = fmap liftIO . writeIORef

swaggerForAPI :: HasSwagger api => Proxy api -> Text
swaggerForAPI = toS . encode . toSwagger

docsForAPI :: HasDocs api => Proxy api -> Text
docsForAPI = pack . markdown . docs

runClient :: BaseUrl -> ClientM a -> IO a
runClient url m = do
  mgr <- newManager defaultManagerSettings
  e <- runClientM m $ mkClientEnv mgr url
  either throw pure e

