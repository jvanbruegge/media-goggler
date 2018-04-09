module MediaGoggler.Database (
    MonadDB,
    fileExistsInDb,
    addFileToDb,
    constructState,
    saveLibrary,
    getLibraries
    ) where

import Protolude hiding (intercalate, empty)
import Prelude (error)

import Data.Text (pack, unpack, intercalate)
import Data.Map (Map, fromList, keys, empty, (!), insert)
import Data.Pool (createPool, withResource)
import Database.Bolt (BoltActionT, Value(..), Structure(..), BoltCfg, Record, queryP, run, connect, close)
import Path (Path, Abs, File, toFilePath)

import MediaGoggler.Config (ServerState(..))
import MediaGoggler.Datatypes
import MediaGoggler.Generics (fromRecord, serialize)

type MonadDB a = forall m . MonadIO m => ReaderT ServerState m a

cleanRecord :: Text -> Record -> Map Text Value
cleanRecord k rec = insert "id" (getId _fields) (getProps _fields)
    where _fields = getFields (rec ! k)
          getFields (S Structure{ fields }) = fields
          getFields _ = error "Fuck off"
          getId (id:_) = id
          getId _ = error "FFS"
          getProps (_:_:(M m):_) = m
          getProps _ = error "still shitty code"

paramsToCypher :: Record -> Text
paramsToCypher params = intercalate " " $ process <$> keys params
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
saveLibrary Library{ libraryType, name } = queryDB cypher params *> pure ()
    where cypher = "CREATE (l:Library:" <> (getLibraryType $ serialize libraryType)
                <> ") Set l.name = {name} " <> (paramsToCypher params)
          params = fromList [("name", T name), ("libraryType", serialize libraryType)]
          getLibraryType (T lib) = lib
          getLibraryType _ = error "shouldnt happen"

getLibraries :: Int -> MonadDB [Library]
getLibraries limit = do
    records <- fmap (cleanRecord "l") <$> queryDB cypher empty
    pure $ fromRecordPure <$> records
    where cypher = "MATCH (l:Library) RETURN l LIMIT " <> (pack $ show limit)
          fromRecordPure rec = case (fromRecord rec) of
              Left e -> error (unpack e)
              Right a -> a

constructState :: BoltCfg -> IO ServerState
constructState cfg = ServerState <$> createPool (connect cfg) close 4 500 1
