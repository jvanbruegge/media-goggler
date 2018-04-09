module MediaGoggler.Database (
    MonadDB,
    fileExistsInDb,
    addFileToDb,
    constructState,
    saveLibrary
    ) where

import Protolude hiding (intercalate)

import Data.Text (pack, intercalate)
import Data.Map (Map, fromList, keys)
import Data.Pool (createPool, withResource)
import Database.Bolt (BoltActionT, Value(T), BoltCfg, Record, queryP, run, connect, close)
import Path (Path, Abs, File, toFilePath)

import MediaGoggler.Config (ServerState(..))
import MediaGoggler.Datatypes

type MonadDB a = forall m . MonadIO m => ReaderT ServerState m a

class DatabaseParams a where
    databaseParams :: a -> Map Text Value

instance DatabaseParams Library where
    databaseParams MovieLibrary{..} = fromList [("name", T name)]
    databaseParams SeriesLibrary{..} = fromList [("name", T name)]

paramsToCypher :: DatabaseParams a => a -> Text
paramsToCypher params = intercalate " " $ process <$> keys (databaseParams params)
    where process key = "SET l." <> key <> " = {" <> key <> "}"

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
    where cypher = "CREATE (f:File:" <> label <> ") SET f.path = {path}"
          label = case file of
              Video -> "Video"

saveLibrary :: Library -> MonadDB ()
saveLibrary lib = queryDB cypher (databaseParams lib) *> pure ()
    where cypher = "CREATE (l:Library:" <> (libraryType lib)
                <> ") Set l.name = {name} " <> (paramsToCypher lib)

constructState :: BoltCfg -> IO ServerState
constructState cfg = ServerState <$> createPool (connect cfg) close 4 500 1
