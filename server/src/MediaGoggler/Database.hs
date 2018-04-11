module MediaGoggler.Database (
    MonadDB,
    fileExistsInDb,
    addFileToDb,
    constructState,
    saveLibrary,
    getLibraries
    ) where

import Protolude hiding (intercalate, empty, head)
import Prelude (head)

import Database.Bolt hiding (Path, pack, unpack)
import Data.Either (rights)
import Data.Text (pack, intercalate)
import Data.Map (Map, fromList, keys, empty, insert)
import Data.Pool (createPool, withResource)
import Path (Path, Abs, File, toFilePath)

import MediaGoggler.Config (ServerState(..))
import MediaGoggler.Datatypes
import MediaGoggler.DBEntry (DBEntry(..))
import MediaGoggler.Generics (RecordSerializable(..))
import MediaGoggler.Label (HasLabel(..))

type MonadDB a = forall m . MonadIO m => ReaderT ServerState m a

convertRecord :: RecordSerializable a => Text -> Record -> Either Text a
convertRecord label rec = do
    Node{ nodeIdentity, nodeProps } <- rec `at` label >>= exact
    fromRecord $ insert "id" (I nodeIdentity) nodeProps

paramsToCypher :: Record -> Text
paramsToCypher params = intercalate " " $ process <$> keys params
    where process key = "SET l." <> key <> " = {" <> key <> "}"

createNode :: (RecordSerializable a, HasLabel a) => a -> MonadDB (DBEntry a)
createNode r = queryDB cypher params >>= pure . head . rights . fmap (convertRecord "n") --TODO: Error handling
    where cypher = "CREATE (n" <> getLabel r <> ") "
                   <> (paramsToCypher params) <> " RETURN n"
          params = toRecord r

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

saveLibrary :: Library -> MonadDB (DBEntry Library)
saveLibrary = createNode

getLibraries :: Int -> MonadDB [DBEntry Library]
getLibraries limit = do
    records <- queryDB cypher empty
    pure $ rights $ fmap (convertRecord "l") records --TODO: Better errors handling
    where cypher = "MATCH (l:Library) RETURN l LIMIT " <> (pack $ show limit)

constructState :: BoltCfg -> IO ServerState
constructState cfg = ServerState <$> createPool (connect cfg) close 4 500 1
