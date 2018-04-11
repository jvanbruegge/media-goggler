module MediaGoggler.DBEntry where

import Protolude hiding (Show, show)
import Prelude (error, Show(..))
import Data.Aeson
import Data.Map ((!?))
import Data.UUID (fromText, toText)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Database.Bolt (exact, Value(T))

import MediaGoggler.Generics (RecordSerializable(..))
import MediaGoggler.Datatypes (Id(..))

data DBEntry a = DBEntry Id a deriving (Generic)

instance Show a => Show (DBEntry a) where
    show (DBEntry (Id i) a) = "DBEntry (Id " <> show i <> ") " <> show a

instance RecordSerializable a => RecordSerializable (DBEntry a) where
    fromRecord rec = do
        id <- toEither (rec !? "id") >>= exact @Text >>= toEither . fromText
        record <- fromRecord rec
        pure $ DBEntry (Id id) record
            where toEither = maybeToEither "Error while decoding ID"
    toRecord (DBEntry (Id i) record) = M.insert "id" (T $ toText i) $ toRecord record

instance ToJSON a => ToJSON (DBEntry a) where
    toJSON (DBEntry i rec) = Object $ HM.insert "id" (toJSON i) (getObject $ toJSON rec)
        where getObject (Object m) = m
              getObject _          = error "Child instance has to return an Object"

instance FromJSON a => FromJSON (DBEntry a) where
    parseJSON = withObject "DBEntry" $ \v -> do
        id <- v .: "id"
        rec <- parseJSON (Object v)
        pure $ DBEntry (Id id) rec
