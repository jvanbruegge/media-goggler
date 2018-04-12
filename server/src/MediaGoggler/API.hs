module MediaGoggler.API where

import Protolude
import Servant

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

type LibraryAPI = Capture "id" Id :> (
        SimpleGet Library
        :<|> "movies" :> Endpoint Movie
    )

type MovieAPI = Capture "id" Id :> (
        Get '[JSON] (DBEntry Movie)
        :<|> "files" :> SimpleEndpoint VideoFile
    )
