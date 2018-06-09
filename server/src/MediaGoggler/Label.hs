module MediaGoggler.Label where

import Data.Text (pack)
import Protolude

import MediaGoggler.Datatypes

class HasLabel a where
    getLabel :: a -> Text

instance HasLabel Library where
    getLabel Library{ libraryType } = ":Library:" <> pack (show libraryType)

instance HasLabel Movie where
    getLabel Movie{} = ":Movie"

instance HasLabel VideoFile where
    getLabel VideoFile{} = ":File:Video"
