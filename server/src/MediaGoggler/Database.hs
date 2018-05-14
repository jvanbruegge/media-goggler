module MediaGoggler.Database (
    MonadDB,
    constructState,
    saveLibrary,
    getLibraries,
    getLibrary,
    saveMovie,
    getMovie,
    getMovies,
    saveVideoFile,
    getVideoFile,
    getVideoFiles
    ) where

import Protolude hiding (intercalate, empty, toList)

import Database.Bolt (at, exact, BoltCfg(..), Record, Node(..), connect, close)
import Data.Text (pack, intercalate)
import Data.Map (fromList, keys, empty, insert)
import Data.Pool (createPool)

import MediaGoggler.Datatypes
import MediaGoggler.Monads (MonadBolt(..), MonadID(..), AppConfig(..))
import MediaGoggler.DBEntry (DBEntry(..))
import MediaGoggler.Generics (RecordSerializable(..), Serializable(..))
import MediaGoggler.Label (HasLabel(..))

type MonadDB m = (MonadBolt m, MonadID m)
type DBSerializeable a = (RecordSerializable a, HasLabel a)

convertRecord :: RecordSerializable a => Text -> Record -> Either Text a
convertRecord label rec = do
    Node{ nodeProps } <- rec `at` label >>= exact
    fromRecord nodeProps

paramsToCypher :: Text -> Record -> Text
paramsToCypher label params = intercalate " " $ process <$> keys params
    where process key = "SET " <> label <> "." <> key <> " = {" <> key <> "}"

toSingle :: (RecordSerializable a, MonadDB m) => Text -> [Record] -> m (Either Text a)
toSingle label = eitherHead . mapM (convertRecord label)
    where eitherHead (Right (x:_)) = pure $ Right x
          eitherHead (Left e) = pure $ Left e
          eitherHead _ = pure $ Left "Error: empty list"

toList :: (RecordSerializable a, MonadDB m) => Text -> [Record] -> m (Either Text [a])
toList label = pure . mapM (convertRecord label)

getLimit :: Maybe Int -> Text
getLimit Nothing = ""
getLimit (Just n) = "LIMIT " <> (pack $ show n)

createNode :: (DBSerializeable a, MonadDB m) => a -> m (Either Text (DBEntry a))
createNode r = paramM >>= queryDB cypher >>= toSingle "n"
    where cypher = "CREATE (n" <> getLabel r <> ") SET n.id = {id}"
                <> (paramsToCypher "n" params) <> " RETURN n"
          paramM = getNewID >>= pure . flip (insert "id") params . serialize
          params = toRecord r

createRelation :: MonadDB m => Text -> Id -> Id -> Text -> Text -> m ()
createRelation rel parent child parentLabel childLabel = queryDB cypher params *> pure ()
    where cypher = "MATCH (n" <> parentLabel <> " {id: {parent}}), "
                <> "(m" <> childLabel <>" {id: {child}}) CREATE (n)-[" <> rel <> "]->(m)"
          params = fromList [("parent", serialize parent), ("child", serialize child)]

queryNode :: MonadDB m => Text -> Id -> m [Record]
queryNode label (Id i) = queryDB cypher params
    where cypher = "MATCH (n" <> label <> ") WHERE n.id = {id} RETURN n"
          params = fromList [("id", serialize i)]

getNode :: (DBSerializeable a, MonadDB m) =>
    Text -> Id -> m (Either Text (DBEntry a))
getNode label id = queryNode label id >>= toSingle "n"

existsNode :: MonadDB m => Text -> Id -> m Bool
existsNode label id = queryNode label id >>= pure . not . null

getNodes :: (DBSerializeable a, MonadDB m) => Text -> Maybe Int -> m (Either Text [DBEntry a])
getNodes label limit = queryDB cypher empty >>= toList "n"
    where cypher = "MATCH (n" <> label <> ") RETURN n " <> getLimit limit

getNodesInRelation ::
    (DBSerializeable a, MonadDB m) =>
    Text -> Text -> Text -> Id -> Maybe Int -> m (Either Text [DBEntry a])
getNodesInRelation rel parentLabel label parent limit = existsNode parentLabel parent >>= \case
        True -> queryDB cypher params >>= toList "m"
        False -> pure $ Left "Parent node not found"
    where cypher = "MATCH (n" <> parentLabel <> " {id: {id}})-[" <> rel <> "]->(m"
                <> label <> ") RETURN m " <> getLimit limit
          params = fromList [("id", serialize parent)]

saveNode :: (DBSerializeable a, MonadDB m) => Text -> Text -> Text -> Id -> a -> m (Either Text (DBEntry a))
saveNode rel parentLabel childLabel id n = createNode n >>= \case
    Left e -> pure $ Left e
    Right node@(DBEntry i _) -> do
        createRelation rel id i parentLabel childLabel
        pure $ Right node

saveLibrary :: MonadDB m => Library -> m (Either Text (DBEntry Library))
saveLibrary = createNode

getLibrary :: MonadDB m => Id -> m (Either Text (DBEntry Library))
getLibrary = getNode ":Library"

getLibraries :: MonadDB m => Maybe Int -> m (Either Text [DBEntry Library])
getLibraries = getNodes ":Library"

saveMovie :: MonadDB m => Id -> Movie -> m (Either Text (DBEntry Movie))
saveMovie = saveNode ":CONTAINS" ":Library:MovieType" ":Movie"

getMovie :: MonadDB m => Id -> m (Either Text (DBEntry Movie))
getMovie = getNode ":Movie"

getMovies :: MonadDB m => Id -> Maybe Int -> m (Either Text [DBEntry Movie])
getMovies = getNodesInRelation ":CONTAINS" ":Library" ":Movie"

saveVideoFile :: MonadDB m => Id -> VideoFile -> m (Either Text (DBEntry VideoFile))
saveVideoFile = saveNode ":FILE" ":Movie" ":File:Video"

getVideoFile :: MonadDB m => Id -> m (Either Text (DBEntry VideoFile))
getVideoFile = getNode ":File:Video"

getVideoFiles :: MonadDB m => Id -> Maybe Int -> m (Either Text [DBEntry VideoFile])
getVideoFiles = getNodesInRelation ":FILE" ":Movie" ":File:Video"

constructState :: BoltCfg -> IO AppConfig
constructState cfg = AppConfig <$> createPool (connect cfg) close 4 500 1
