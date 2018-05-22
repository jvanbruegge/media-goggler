module MediaGoggler.API where

import Protolude hiding (empty)
import Servant
import Conduit (ConduitT, ResourceT)
import Data.ByteString.Lazy (empty)
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Media as M

import MediaGoggler.Datatypes (Library, Person, Id, Movie, VideoFile)
import MediaGoggler.DBEntry (DBEntry)

type SimpleGet res = Get '[JSON] (DBEntry res)

type GetAll res = QueryParam "count" Int :> Get '[JSON] [DBEntry res]
type PostSingle res = ReqBody '[JSON] res :> Post '[JSON] (DBEntry res)
type GetSingle res = Capture "id" Id :> Get '[JSON] (DBEntry res)

type Endpoint res = GetAll res :<|> PostSingle res
type SimpleEndpoint res = GetSingle res :<|> Endpoint res

type MediaGogglerAPI = "libraries" :> (LibraryAPI :<|> Endpoint Library)
    :<|> "persons" :> SimpleEndpoint Person
    :<|> "movies" :> MovieAPI
    :<|> "files" :> FileAPI

type LibraryAPI = Capture "id" Id :> (
        SimpleGet Library
        :<|> "movies" :> Endpoint Movie
    )

type MovieAPI = Capture "id" Id :> (
        Get '[JSON] (DBEntry Movie)
        :<|> "files" :> Endpoint VideoFile
    )

type FileStream = ConduitT () ByteString (ResourceT IO) ()

type FileAPI = Capture "id" Id :> (
        Get '[JSON] (DBEntry VideoFile)
        :<|> "raw" :> StreamGet NoFraming OggVideo FileStream
    )

data NoFraming

instance FramingRender NoFraming a where
    header   _ _ = empty
    boundary _ _ = BoundaryStrategyGeneral identity
    trailer  _ _ = empty

data OggVideo deriving Typeable

instance Accept OggVideo where
    contentType _ = "video" M.// "ogg"

instance MimeRender OggVideo ByteString where
    mimeRender _ = BL.fromStrict
