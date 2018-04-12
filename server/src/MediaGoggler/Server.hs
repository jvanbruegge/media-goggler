module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)

import MediaGoggler.API
import MediaGoggler.Datatypes
import qualified MediaGoggler.Database as DB
import MediaGoggler.Monads (AppT)

type Server api = ServerT api (AppT Handler)

server :: Server MediaGogglerAPI
server = (libraryServer :<|> getLibraries :<|> postLibrary)
    :<|> (getPerson :<|> getPersons :<|> postPerson)
    :<|> movieServer
    where
        getPerson = undefined
        getPersons = undefined
        postPerson = undefined

libraryServer :: Server LibraryAPI
libraryServer id = getLibrary id :<|> (getMovies id :<|> postMovie id)

getLibrary :: Id -> Server (SimpleGet Library)
getLibrary = DB.getLibrary

getLibraries :: Server (GetAll Library)
getLibraries = DB.getLibraries

postLibrary :: Server (PostSingle Library)
postLibrary = DB.saveLibrary

movieServer :: Server MovieAPI
movieServer id = getMovie id :<|> videoFileServer id

postMovie :: Id -> Server (PostSingle Movie)
postMovie = DB.saveMovie

getMovies :: Id -> Server (GetAll Movie)
getMovies = DB.getMovies

getMovie :: Id -> Server (SimpleGet Movie)
getMovie = DB.getMovie

videoFileServer :: Id -> Server (SimpleEndpoint VideoFile)
videoFileServer id = (getFile :<|> getFiles id :<|> postFile id)

getFile :: Server (GetSingle VideoFile)
getFile = DB.getVideoFile

getFiles :: Id -> Server (GetAll VideoFile)
getFiles = DB.getVideoFiles

postFile :: Id -> Server (PostSingle VideoFile)
postFile = DB.saveVideoFile
