module MediaGoggler.Generics (
    Serializable,
    RecordSerializable,
    fromRecord,
    toRecord,
    serialize,
    deserialize) where

import Data.Map (Map, lookup, empty, union, singleton)
import Data.Text (pack, unpack)
import Data.UUID (UUID, toText, fromText)
import Database.Bolt (Record, Value(..))
import GHC.Generics
import Path (Path, Rel, File, Dir, toFilePath, parseRelFile, parseRelDir)
import Prelude (undefined)
import Protolude hiding (empty, map, undefined)

class Serializable a where
    serialize :: a -> Value
    deserialize :: Value -> Either Text a

instance Serializable Text where
    serialize = T
    deserialize (T s) = Right s
    deserialize _ = Left "Not a text value"

instance Serializable Int where
    serialize = I
    deserialize (I i) = Right i
    deserialize _ = Left "Not an integer value"

instance Serializable UUID where
    serialize = T . toText
    deserialize (T x) = maybeToEither "Not a UUID value" (fromText x)
    deserialize _ = Left "Not a UUID value"

instance Serializable (Path Rel File) where
    serialize = T . pack . toFilePath
    deserialize (T p) = maybeToEither "Error while parsing path" (parseRelFile $ unpack p)
    deserialize _ = Left "Not a Path value"

instance Serializable (Path Rel Dir) where
    serialize = T . pack . toFilePath
    deserialize (T p) = maybeToEither "Error while parsing path" (parseRelDir $ unpack p)
    deserialize _ = Left "Not a Path value"

instance (Serializable a, Serializable b) => Serializable (a, b) where
    serialize (a, b) = L [serialize a, serialize b]
    deserialize (L [a, b]) = (,) <$> deserialize a <*> deserialize b
    deserialize _ = Left "Not a Tuple type"

class RecordSerializable a where
    fromRecord :: Record -> Either Text a
    toRecord :: a -> Record

    default fromRecord :: (Generic a, GRecordSerializable () (Rep a)) => Record -> Either Text a
    fromRecord rec = to <$> gFromRecord () rec

    default toRecord :: (Generic a, GRecordSerializable () (Rep a)) => a -> Record
    toRecord x = gToRecord () (from x)

-- Black Magic from https://stackoverflow.com/questions/44494286/how-to-write-a-generic-function-that-can-serialise-deserialize-any-record-from-a
class GRecordSerializable k f where
    gFromRecord :: k -> Record -> Either Text (f a)
    gToRecord :: k -> f a -> Record

instance Serializable a => GRecordSerializable Text (K1 i a) where
    gFromRecord key map = K1 <$> (lookupE key map >>= deserialize)
    gToRecord key (K1 x) = singleton key (serialize x)

lookupE :: (Ord k, Show k) => k -> Map k v -> Either Text v
lookupE k = maybe (Left $ "Key not found: " <> pack (show k)) Right . lookup k

instance (Selector c, GRecordSerializable Text f) => GRecordSerializable k (M1 S c f) where
    gFromRecord _ map = fixProxy $ \proxy -> M1 <$> gFromRecord (pack $ selName proxy) map
    gToRecord _ m@(M1 x) = gToRecord (pack $ selName m) x

fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

instance GRecordSerializable k f => GRecordSerializable k (M1 D c f) where
    gFromRecord key map = M1 <$> gFromRecord key map
    gToRecord key (M1 x) = gToRecord key x

instance GRecordSerializable k f => GRecordSerializable k (M1 C c f) where
    gFromRecord key map = M1 <$> gFromRecord key map
    gToRecord key (M1 x) = gToRecord key x

instance (GRecordSerializable k f, GRecordSerializable k g) => GRecordSerializable k (f :*: g) where
    gFromRecord key map = (:*:) <$> gFromRecord key map <*> gFromRecord key map
    gToRecord key (a :*: b) = union (gToRecord key a) (gToRecord key b)

instance GRecordSerializable k U1 where
    gFromRecord _ _ = pure U1
    gToRecord _ U1 = empty
