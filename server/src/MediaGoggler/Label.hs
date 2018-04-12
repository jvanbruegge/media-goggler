module MediaGoggler.Label where

import Protolude
import Data.Text (pack)

import MediaGoggler.Datatypes

class HasLabel a where
    getLabel :: a -> Text

instance HasLabel Library where
    getLabel Library{ libraryType } = ":Library:" <> (pack $ show libraryType)

instance HasLabel Movie where
    getLabel Movie{} = ":Movie"
