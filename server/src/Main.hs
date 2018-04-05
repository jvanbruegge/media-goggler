module Main where

import Protolude
import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import API (MediaGogglerAPI)
import Server (server)

data ServerState = ServerState
    { boltPort :: Int,
      dbUser :: Text,
      dbPassword :: Text
    } deriving Generic

mediaGogglerAPI :: Proxy MediaGogglerAPI
mediaGogglerAPI = Proxy

app :: Application
app = serve mediaGogglerAPI server

main :: IO ()
main = run 3000 app
