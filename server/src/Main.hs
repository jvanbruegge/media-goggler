module Main where

import Protolude
import Servant hiding (Server)
import Network.Wai.Handler.Warp (run)

import API (MediaGogglerAPI)
import Server (ServerState(..), server)

readerToHandler :: forall a. ServerState -> ReaderT ServerState Handler a -> Handler a
readerToHandler = flip runReaderT

mediaGogglerAPI :: Proxy MediaGogglerAPI
mediaGogglerAPI = Proxy

main :: IO ()
main = do
    let serverState = ServerState { boltPort = 3333, dbUser = "neo4j", dbPassword = "neo4j"
        }
    let hoisted = hoistServer mediaGogglerAPI (readerToHandler serverState) server
    let app = serve mediaGogglerAPI hoisted
    run 3000 app
