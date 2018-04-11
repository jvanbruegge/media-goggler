module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)

import MediaGoggler.API
import MediaGoggler.Datatypes
import qualified MediaGoggler.Database as DB
import MediaGoggler.Monads (AppT)

type Server api = ServerT api (AppT Handler)

server :: Server MediaGogglerAPI
server = (getLibrary :<|> getLibraries :<|> postLibrary)
    :<|> (getPerson :<|> getPersons :<|> postPerson)
    where
        getPerson = undefined
        getPersons = undefined
        postPerson = undefined

getLibrary :: Server LibraryAPI
getLibrary id = getSingleLibrary id :<|> (getMovie id :<|> getMovies id :<|> postMovie id)
    where getMovie = undefined
          getMovies = undefined
          postMovie = undefined
          getSingleLibrary = undefined

getSingleLibrary :: Id -> Server (SimpleGet Library)
getSingleLibrary = undefined

getLibraries :: Server (GetAll Library)
getLibraries limit = DB.getLibraries $ fromMaybe 100 limit

postLibrary :: Server (PostSingle Library)
postLibrary = DB.saveLibrary
