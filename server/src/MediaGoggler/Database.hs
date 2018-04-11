module MediaGoggler.Database (
    MonadDB,
    fileExistsInDb,
    addFileToDb,
    constructState,
    saveLibrary,
    getLibraries
    ) where

import Protolude hiding (intercalate, empty)

import Database.Bolt (at, exact, Value(..), BoltCfg(..), Record, Node(..), connect, close)
import Data.Text (pack, intercalate)
import Data.Map (Map, fromList, keys, empty, insert)
import Data.Pool (createPool)
import Path (Path, Abs, File, toFilePath)

import MediaGoggler.Datatypes
import MediaGoggler.Monads (MonadBolt(..), MonadID(..), AppConfig(..), MonadDBError(..))
import MediaGoggler.DBEntry (DBEntry(..))
import MediaGoggler.Generics (RecordSerializable(..), Serializable(..))
import MediaGoggler.Label (HasLabel(..))

type MonadDB m = (MonadBolt m, MonadID m, MonadDBError m)
type DBSerializeable a = (RecordSerializable a, HasLabel a)

convertRecord :: RecordSerializable a => Text -> Record -> Either Text a
convertRecord label rec = do
    Node{ nodeProps } <- rec `at` label >>= exact
    fromRecord nodeProps

paramsToCypher :: Text -> Record -> Text
paramsToCypher label params = intercalate " " $ process <$> keys params
    where process key = "SET " <> label <> "." <> key <> " = {" <> key <> "}"

createNode :: forall m a . (DBSerializeable a, MonadDB m) => a -> m (DBEntry a)
createNode r = paramM >>= queryDB cypher >>= exceptHead . mapM (convertRecord "n")
    where cypher = "CREATE (n" <> getLabel r <> ") SET n.id = {id}"
                <> (paramsToCypher "n" params) <> " RETURN n"
          paramM = getNewID >>= pure . flip (insert "id") params . serialize
          params = toRecord r
          exceptHead (Right (x:_)) = pure x
          exceptHead (Left e) = throwDBError e
          exceptHead _ = throwDBError "Error while getting value from DB"

--getNode :: RecordSerializable a => Id -> MonadDB (DBEntry a)
--getNode (Id i) = queryDB cypher

paramsFromPath :: Path Abs File -> Map Text Value
paramsFromPath path = fromList [("path", (T . pack . toFilePath) path)]

fileExistsInDb :: MonadDB m => Path Abs File -> m Bool
fileExistsInDb path = not . null <$> queryDB cypher (paramsFromPath path)
    where cypher = "MATCH (f:File) WHERE f.path = {path} RETURN *"

addFileToDb :: MonadDB m => FileType -> Path Abs File -> m ()
addFileToDb file path = queryDB cypher (paramsFromPath path) *> pure ()
    where cypher = "CREATE (f:File:" <> label <> ") SET f.path = {path}"
          label = case file of
              Video -> "Video"

saveLibrary :: MonadDB m => Library -> m (DBEntry Library)
saveLibrary = createNode

getLibraries :: MonadDB m => Int -> m [DBEntry Library]
getLibraries limit = do
    records <- queryDB cypher empty
    pure $ rights $ fmap (convertRecord "l") records --TODO: Better errors handling
    where cypher = "MATCH (l:Library) RETURN l LIMIT " <> (pack $ show limit)

constructState :: BoltCfg -> IO AppConfig
constructState cfg = AppConfig <$> createPool (connect cfg) close 4 500 1
