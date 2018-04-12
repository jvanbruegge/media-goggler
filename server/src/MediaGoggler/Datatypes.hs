module MediaGoggler.Datatypes where

import Protolude
import Servant (FromHttpApiData)
import Data.Text (pack, unpack)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Path (Path, Rel, File, toFilePath, parseRelFile)
import Database.Bolt (Value(..))

import MediaGoggler.Generics (Serializable(..), RecordSerializable(..))

newtype Id = Id UUID
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)
    deriving newtype (FromHttpApiData, Serializable)

data LibraryType = MovieType | SeriesType deriving (Generic, Show, FromJSON, ToJSON)

instance Serializable LibraryType where
    serialize MovieType = T "MovieType"
    serialize SeriesType = T "SeriesType"
    deserialize (T "MovieType") = Right MovieType
    deserialize (T "SeriesType") = Right SeriesType
    deserialize _ = Left "Not a LibraryType value"

instance Serializable (Path Rel File) where
    serialize = T . pack . toFilePath
    deserialize (T p) = maybeToEither "Error while parsing path" (parseRelFile $ unpack p)
    deserialize _ = Left "Not a Path value"

instance (Serializable a, Serializable b) => Serializable (a, b) where
    serialize (a, b) = L [serialize a, serialize b]
    deserialize (L [a, b]) = (,) <$> deserialize a <*> deserialize b
    deserialize _ = Left "Not a Tuple type"

data Library = Library
    { name :: Text
    , libraryType :: LibraryType
    } deriving (Generic, FromJSON, ToJSON, RecordSerializable)

data Movie = Movie
    { title :: Text
    , summary :: Text
    } deriving (Generic, FromJSON, ToJSON, RecordSerializable)

data Person = Person
    { name :: Text,
      age :: Int,
      bio :: Text
    } deriving (Generic, FromJSON, ToJSON, RecordSerializable)

data VideoFile = VideoFile
    { path :: Path Rel File,
      resolution :: (Int, Int),
      container :: Text
    } deriving (Generic, FromJSON, ToJSON, RecordSerializable)
