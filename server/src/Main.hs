module Main where

import Protolude

import Data.Default (def)
import Servant (serve, hoistServer, Handler)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Database.Bolt (BoltCfg(..))

import MediaGoggler.API (MediaGogglerAPI)
import MediaGoggler.Server (ServerState, server)
import MediaGoggler.Database (constructState, addFileToDb)
import MediaGoggler.Filesystem (getNewFiles)
import MediaGoggler.Datatypes (FileType(..))

readerToHandler :: forall a. ServerState -> ReaderT ServerState Handler a -> Handler a
readerToHandler = flip runReaderT

getApp :: ServerState -> Application
getApp s = serve api $ hoistServer api (readerToHandler s) server
    where api = Proxy @MediaGogglerAPI

main :: IO ()
main = do
    serverState <- constructState $ def { user = "neo4j", password = "password" }
    newPaths <- runReaderT getNewFiles serverState
    putStrLn $ "found " ++ (show . length) newPaths ++ " new videos"
    runReaderT (mapM_ (addFileToDb Video) newPaths) serverState
    run 3000 $ getApp serverState
