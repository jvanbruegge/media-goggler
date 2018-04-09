module MediaGoggler.Datatypes where

import Protolude
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Path (Path, Rel, File)

data FileType = Video deriving (Generic, FromJSON, ToJSON)

class LibraryType a where
    libraryType :: a -> Text

instance LibraryType Library where
    libraryType MovieLibrary{} = "MovieLibrary"
    libraryType SeriesLibrary{} = "SeriesLibrary"

data Library = MovieLibrary
    { name :: Text,
      movies :: [Movie]
    } | SeriesLibrary
    { name :: Text,
      specials :: Season,
      seasons :: [Season]
    } deriving (Generic, FromJSON, ToJSON)

data Movie = Movie
    { actors :: [Person],
      directors :: [Person],
      file :: VideoFile
    } deriving (Generic, FromJSON, ToJSON)

data Season = Season
    { episodes :: [Episode]
    } deriving (Generic, FromJSON, ToJSON)

data Episode = Episode
    { file :: VideoFile
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
