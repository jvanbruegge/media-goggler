module Main where

import Protolude

import Data.Default (def)
import Data.Tagged (Tagged(..))
import Servant hiding (Server)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, ssIndices)
import WaiAppStatic.Types (unsafeToPiece)
import Database.Bolt (BoltCfg(..))

import MediaGoggler.API (MediaGogglerAPI)
import MediaGoggler.Server (server)
import MediaGoggler.Monads (AppConfig(..), AppT, Server)
import MediaGoggler.Database (constructState)

readerToHandler :: forall a. AppConfig -> AppT Handler a -> Handler a
readerToHandler = flip runReaderT

api :: Proxy (("api" :> MediaGogglerAPI) :<|> Raw)
api = Proxy

getApp :: AppConfig -> Application
getApp s = serve api $ hoistServer api (readerToHandler s) (server :<|> servePublic)

servePublic :: Server Raw
servePublic = Tagged $ staticApp $ (defaultWebAppSettings "/root/public") { ssIndices = [unsafeToPiece "index.html"] }

main :: IO ()
main = do
    serverState <- constructState $ def { host="neo4j", user = "neo4j", password = "password" }
    run 3000 $ getApp serverState
