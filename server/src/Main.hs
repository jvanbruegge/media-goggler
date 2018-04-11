module Main where

import Protolude

import Data.Default (def)
import Servant (serve, hoistServer, Handler)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Database.Bolt (BoltCfg(..))

import MediaGoggler.API (MediaGogglerAPI)
import MediaGoggler.Server (server)
import MediaGoggler.Monads (AppConfig(..), AppT)
import MediaGoggler.Database (constructState)

readerToHandler :: forall a. AppConfig -> AppT Handler a -> Handler a
readerToHandler = flip runReaderT

getApp :: AppConfig -> Application
getApp s = serve api $ hoistServer api (readerToHandler s) server
    where api = Proxy @MediaGogglerAPI

main :: IO ()
main = do
    serverState <- constructState $ def { user = "neo4j", password = "password" }
    run 3000 $ getApp serverState
