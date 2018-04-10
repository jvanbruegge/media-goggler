module MediaGoggler.API where

import Protolude
import Servant

import MediaGoggler.Datatypes (Library, Person, Id)
import MediaGoggler.DBEntry (DBEntry)

type GetAll res = QueryParam "count" Int :> Get '[JSON] [DBEntry res]
type PostSingle res = ReqBody '[JSON] res :> Post '[JSON] (DBEntry res)
type GetSingle res = Capture "id" Id :> Get '[JSON] (DBEntry res)

type Endpoint res = GetAll res :<|> PostSingle res
type SimpleEndpoint res = GetSingle res :<|> Endpoint res

type MediaGogglerAPI = "libraries" :> (LibraryAPI :<|> Endpoint Library)
    :<|> "persons" :> SimpleEndpoint Person

type LibraryAPI = Capture "id" Id :> (
        Get '[JSON] (DBEntry Library)
        -- :<|> "movies" :> SimpleEndpoint Movie
    )
