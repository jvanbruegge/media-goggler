module Datatypes where

import Protolude
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Path (Path, Rel, File)

data Library = Library
    { movies :: [Movie]
    } deriving Generic

instance ToJSON Library

data Movie = Movie
    { actors :: [Person],
      directors :: [Person],
      file :: VideoFile
    } deriving Generic

instance ToJSON Movie

data Person = Person
    { name :: Text,
      age :: Int,
      bio :: Text
    } deriving Generic

instance ToJSON Person

data VideoFile = VideoFile
    { path :: Path Rel File,
      resolution :: (Int, Int),
      container :: Text
    } deriving Generic

instance ToJSON VideoFile
