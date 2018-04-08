module MediaGoggler.Database (
    MonadDB,
    fileExistsInDb,
    addFileToDb,
    constructState
    ) where

import Protolude

import Data.Text (pack)
import Data.Map (Map, fromList)
import Data.Pool (createPool, withResource)
import Database.Bolt (BoltActionT, Value(T), BoltCfg, Record, queryP, run, connect, close)
import Path (Path, Abs, File, toFilePath)

import MediaGoggler.Server (ServerState(..))
import MediaGoggler.Datatypes (FileType(..))

type MonadDB a = forall m . MonadIO m => ReaderT ServerState m a

queryDB :: Text -> Map Text Value -> MonadDB [Record]
queryDB c p = runQuery $ queryP c p

runQuery :: BoltActionT IO a -> MonadDB a
runQuery action = do
    ServerState{ pool } <- ask
    liftIO $ withResource pool (`run` action)

paramsFromPath :: Path Abs File -> Map Text Value
paramsFromPath path = fromList [("path", (T . pack . toFilePath) path)]

fileExistsInDb :: Path Abs File -> MonadDB Bool
fileExistsInDb path = not . null <$> queryDB cypher (paramsFromPath path)
    where cypher = "MATCH (f:File) WHERE f.path = {path} RETURN *"

addFileToDb :: FileType -> Path Abs File -> MonadDB ()
addFileToDb file path = queryDB cypher (paramsFromPath path) *> pure ()
    where cypher = "CREATE (f:File:" `mappend` label `mappend` ") SET f.path = {path}"
          label = case file of
              Video -> "Video"

constructState :: BoltCfg -> IO ServerState
constructState cfg = ServerState <$> createPool (connect cfg) close 4 500 1
