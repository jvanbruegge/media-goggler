module MediaGoggler.Datatypes where

import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import Database.Bolt (Value(..))
import GHC.Generics (Generic)
import Path (Path, Rel, File, Dir)
import Protolude
import Servant (FromHttpApiData)

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

data Library = Library
    { name :: Text
    , rootDir :: Path Rel Dir
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
