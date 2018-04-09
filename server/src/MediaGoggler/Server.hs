module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)
import Data.Pool (Pool)
import Database.Bolt (Pipe)

import MediaGoggler.API
import MediaGoggler.Datatypes
import MediaGoggler.Database (saveLibrary)
import MediaGoggler.Config (ServerState)

type Server api = ServerT api (ReaderT ServerState Handler)

server :: Server MediaGogglerAPI
server = (getLibrary :<|> getLibraries :<|> postLibrary)
    :<|> (getPerson :<|> getPersons :<|> postPerson)
    where
        getLibrary = undefined
        getPerson = undefined
        getPersons = undefined
        postPerson = undefined


getLibraries :: Server (GetAll Library)
getLibraries _ = return [MovieLibrary { movies = [], name = "Filme" }]

postLibrary :: Server (PostSingle Library)
postLibrary = saveLibrary
