module MediaGoggler.Monads where

import Protolude

import Data.Map (Map)
import Data.Pool (Pool, withResource)
import Data.String.Conversions (cs)
import Data.UUID.V4 (nextRandom)
import Database.Bolt (Value, Record, Pipe, queryP, run)
import Servant (ServantErr(..), err500)

import MediaGoggler.Datatypes (Id(..))

data AppConfig = AppConfig { pool :: Pool Pipe }

type AppT m = ReaderT AppConfig m

class Monad m => MonadBolt m where
    queryDB :: Text -> Map Text Value -> m [Record]

instance MonadIO m => MonadBolt (AppT m) where
    queryDB cypher params = do
        AppConfig{ pool } <- ask
        liftIO $ withResource pool (`run` queryP cypher params)

class Monad m => MonadID m where
    getNewID :: m Id

instance MonadIO m => MonadID (AppT m) where
    getNewID = Id <$> liftIO nextRandom

class Monad m => MonadDBError m where
    throwDBError :: Text -> m a

instance MonadError ServantErr m => MonadDBError (AppT m) where
    throwDBError e = throwError $ err500 { errBody = cs e }
