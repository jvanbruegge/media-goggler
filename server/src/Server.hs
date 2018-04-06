module Server where

import Protolude
import Servant hiding (Server)

import API
import Datatypes

data ServerState = ServerState
    { boltPort :: Int,
      dbUser :: Text,
      dbPassword :: Text
    } deriving Generic

type Server api = ServerT api (ReaderT ServerState Handler)

server :: Server MediaGogglerAPI
server = (getLibraries :<|> getLibrary) :<|> (getPersons :<|> getPerson)
    where
        getLibrary = undefined
        getPerson = undefined
        getPersons = undefined


getLibraries :: Server LibrariesAPI
getLibraries _ = return [Library { movies = [], name = "Filme", libraryType = MovieType }]
