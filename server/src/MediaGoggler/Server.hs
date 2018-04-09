module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)

import MediaGoggler.API
import MediaGoggler.Datatypes
import qualified MediaGoggler.Database as DB
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
getLibraries limit = DB.getLibraries $ fromMaybe 100 limit

postLibrary :: Server (PostSingle Library)
postLibrary = DB.saveLibrary
