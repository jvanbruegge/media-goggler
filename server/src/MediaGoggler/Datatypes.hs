module MediaGoggler.Datatypes where

import Protolude
import Servant (FromHttpApiData)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Path (Path, Rel, File)
import Database.Bolt (Value(..))

import MediaGoggler.Generics (Serializable(..), RecordSerializable(..))

newtype Id = Id Int
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)
    deriving newtype (FromHttpApiData, Serializable)

data FileType = Video deriving (Generic, FromJSON, ToJSON)
data LibraryType = MovieType | SeriesType deriving (Generic, Show, FromJSON, ToJSON)

instance Serializable LibraryType where
    serialize MovieType = T "MovieType"
    serialize SeriesType = T "SeriesType"
    deserialize (T "MovieType") = Right MovieType
    deserialize (T "SeriesType") = Right SeriesType
    deserialize _ = Left "Not a LibraryType value"

data Library = Library
    { name :: Text
    , libraryType :: LibraryType
    } deriving (Generic, FromJSON, ToJSON, RecordSerializable)

data Metadata = Metadata
    { summary :: Text
    } deriving (Generic, FromJSON, ToJSON)

data Person = Person
    { name :: Text,
      age :: Int,
      bio :: Text
    } deriving (Generic, FromJSON, ToJSON)

data VideoFile = VideoFile
    { path :: Path Rel File,
      resolution :: (Int, Int),
      container :: Text
    } deriving (Generic, FromJSON, ToJSON)
