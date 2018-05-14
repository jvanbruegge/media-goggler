module MediaGoggler.Server where

import Protolude
import Servant hiding (Server)
import Data.String.Conversions (cs)

import MediaGoggler.API
import MediaGoggler.Datatypes
import qualified MediaGoggler.Database as DB
import MediaGoggler.Monads (AppT)

type Server api = ServerT api (AppT Handler)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

server :: Server MediaGogglerAPI
server = (libraryServer :<|> getLibraries :<|> postLibrary)
    :<|> personServer
    :<|> movieServer
    :<|> getFile
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

getFile :: Server (GetSingle VideoFile)
getFile = except404 . DB.getVideoFile

getFiles :: Id -> Server (GetAll VideoFile)
getFiles = except404 ... DB.getVideoFiles

postFile :: Id -> Server (PostSingle VideoFile)
postFile = except500 ... DB.saveVideoFile

except500 :: (DB.MonadDB m, MonadError ServantErr m) => m (Either Text a) -> m a
except500 = eitherToExcept err500

except404 :: (DB.MonadDB m, MonadError ServantErr m) => m (Either Text a) -> m a
except404 = eitherToExcept err404

eitherToExcept :: ServantErr -> (DB.MonadDB m, MonadError ServantErr m) => m (Either Text a) -> m a
eitherToExcept err = (>>= \case
    Right x -> pure x
    Left e -> throwError $ err { errBody = cs e })
