module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)
import Data.Pool (Pool)
import Database.Bolt (Pipe)

import MediaGoggler.API
import MediaGoggler.Datatypes

data ServerState = ServerState
    { pool :: Pool Pipe
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
