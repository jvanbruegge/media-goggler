module Main where

import Protolude

import Data.Default (def)
import Servant hiding (Server)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Database.Bolt (BoltCfg(..))

import MediaGoggler.API (MediaGogglerAPI)
import MediaGoggler.Server (server)
import MediaGoggler.Monads (AppConfig(..), AppT, Server)
import MediaGoggler.Database (constructState)

import MediaGoggler.Frontend.Index (FrontendAPI, serveFrontend)

readerToHandler :: forall a. AppConfig -> AppT Handler a -> Handler a
readerToHandler = flip runReaderT

api :: Proxy (("api" :> MediaGogglerAPI) :<|> FrontendAPI)
api = Proxy

getApp :: AppConfig -> Application
getApp s = serve api $ hoistServer api (readerToHandler s) (server :<|> serveFrontend)

main :: IO ()
main = do
    serverState <- constructState $ def { user = "neo4j", password = "password" }
    run 3000 $ getApp serverState
