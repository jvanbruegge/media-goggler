module MediaGoggler.Database (
    MonadDB,
    fileExistsInDb,
    addFileToDb,
    constructState,
    saveLibrary,
    getLibraries,
    getLibrary,
    saveMovie,
    getMovie,
    getMovies
    ) where

import Protolude hiding (intercalate, empty, toList)

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

toSingle :: (RecordSerializable a, MonadDB m) => Text -> [Record] -> m a
toSingle label = exceptHead . mapM (convertRecord label)
    where exceptHead (Right (x:_)) = pure x
          exceptHead (Left e) = throwDBError e
          exceptHead _ = throwDBError "Error while getting value from DB"

toList :: (RecordSerializable a, MonadDB m) => Text -> [Record] -> m [a]
toList label = except . mapM (convertRecord label)
    where except (Right x) = pure x
          except (Left e) = throwDBError e

getLimit :: Maybe Int -> Text
getLimit Nothing = ""
getLimit (Just n) = "LIMIT " <> (pack $ show n)

createNode :: (DBSerializeable a, MonadDB m) => a -> m (DBEntry a)
createNode r = paramM >>= queryDB cypher >>= toSingle "n"
    where cypher = "CREATE (n" <> getLabel r <> ") SET n.id = {id}"
                <> (paramsToCypher "n" params) <> " RETURN n"
          paramM = getNewID >>= pure . flip (insert "id") params . serialize
          params = toRecord r

createRelation :: MonadDB m => Id -> Id -> Text -> Text -> m ()
createRelation parent child parentLabel childLabel = queryDB cypher params *> pure ()
    where cypher = "MATCH (n" <> parentLabel <> " {id: {parent}}), "
                <> "(m" <> childLabel <>" {id: {child}}) CREATE (n)-[:CONTAINS]->(m)"
          params = fromList [("parent", serialize parent), ("child", serialize child)]

getNode :: (DBSerializeable a, MonadDB m) => Text -> Id -> m (DBEntry a)
getNode label (Id i) = queryDB cypher params >>= toSingle "n"
    where cypher = "MATCH (n" <> label <> ") WHERE n.id = {id} RETURN n"
          params = fromList [("id", serialize i)]

getNodes :: (DBSerializeable a, MonadDB m) => Text -> Maybe Int -> m [DBEntry a]
getNodes label limit = queryDB cypher empty >>= toList "n"
    where cypher = "MATCH (n" <> label <> ") RETURN n " <> getLimit limit

getNodesInRelation :: (DBSerializeable a, MonadDB m) => Text -> Text -> Id -> Maybe Int -> m [DBEntry a]
getNodesInRelation parentLabel label parent limit = queryDB cypher params >>= toList "m"
    where cypher = "MATCH (n" <> parentLabel <> " {id: {id}})-[:CONTAINS]->(m"
                <> label <> ") RETURN m " <> getLimit limit
          params = fromList [("id", serialize parent)]

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

getLibrary :: MonadDB m => Id -> m (DBEntry Library)
getLibrary = getNode ":Library"

getLibraries :: MonadDB m => Maybe Int -> m [DBEntry Library]
getLibraries = getNodes ":Library"

saveMovie :: MonadDB m => Id -> Movie -> m (DBEntry Movie)
saveMovie id m = do
    movie@(DBEntry i _) <- createNode m
    createRelation id i ":Library:MovieType" ":Movie"
    pure movie

getMovie :: MonadDB m => Id -> m (DBEntry Movie)
getMovie = getNode ":Movie"

getMovies :: MonadDB m => Id -> Maybe Int -> m [DBEntry Movie]
getMovies = getNodesInRelation ":Library" ":Movie"

constructState :: BoltCfg -> IO AppConfig
constructState cfg = AppConfig <$> createPool (connect cfg) close 4 500 1
