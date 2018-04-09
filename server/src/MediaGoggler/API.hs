module MediaGoggler.API where

import Protolude
import Servant
import GHC.Generics (Generic)

import MediaGoggler.Datatypes (Library, Movie, Person)

newtype Id = Id Int deriving (Generic, FromHttpApiData)

type GetAll res = QueryParam "count" Int :> Get '[JSON] [res]
type PostSingle res = ReqBody '[JSON] res :> Post '[JSON] ()
type GetSingle res = Capture "id" Id :> Get '[JSON] res

type Endpoint res = GetAll res :<|> PostSingle res
type SimpleEndpoint res = GetSingle res :<|> Endpoint res

type MediaGogglerAPI = "libraries" :> (LibraryAPI :<|> Endpoint Library)
    :<|> "persons" :> SimpleEndpoint Person

type LibraryAPI = Capture "id" Id :> (
        Get '[JSON] Library
        :<|> "movies" :> SimpleEndpoint Movie
    )
