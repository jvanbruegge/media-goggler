module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)

import MediaGoggler.API
import MediaGoggler.Datatypes
import MediaGoggler.Raw (serveFile, serveRange)
import MediaGoggler.Monads (Server)
import MediaGoggler.Error (except404, except500)
import qualified MediaGoggler.Database as DB

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

server :: Server MediaGogglerAPI
server = (libraryServer :<|> getLibraries :<|> postLibrary)
    :<|> personServer
    :<|> movieServer
    :<|> fileServer
    where
        personServer = undefined

libraryServer :: Server LibraryAPI
libraryServer id = getLibrary id :<|> (getMovies id :<|> postMovie id)

getLibrary :: Id -> Server (SimpleGet Library)
getLibrary = except404 . DB.getLibrary

getLibraries :: Server (GetAll Library)
getLibraries = except404 . DB.getLibraries

postLibrary :: Server (PostSingle Library)
postLibrary = except500 . DB.saveLibrary

movieServer :: Server MovieAPI
movieServer id = getMovie id :<|> videoFileServer id

postMovie :: Id -> Server (PostSingle Movie)
postMovie = except500 ... DB.saveMovie

getMovies :: Id -> Server (GetAll Movie)
getMovies = except404 ... DB.getMovies

getMovie :: Server (GetSingle Movie)
getMovie = except404 . DB.getMovie

videoFileServer :: Id -> Server (Endpoint VideoFile)
videoFileServer id = (getFiles id :<|> postFile id)

fileServer :: Server FileAPI
fileServer id = (getFile id :<|> serveRange id :<|> serveFile id)

getFile :: Server (GetSingle VideoFile)
getFile = except404 . DB.getVideoFile

getFiles :: Id -> Server (GetAll VideoFile)
getFiles = except404 ... DB.getVideoFiles

postFile :: Id -> Server (PostSingle VideoFile)
postFile = except500 ... DB.saveVideoFile
