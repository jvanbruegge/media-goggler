module API where

import Protolude
import Servant
import GHC.Generics (Generic)

import Datatypes (Library, Movie, Person)

newtype Id = Id Int deriving (Generic, FromHttpApiData)

type MediaGogglerAPI = "libraries" :> (LibrariesAPI :<|> LibraryAPI)
    :<|> "persons" :> (PersonsAPI :<|> PersonAPI)

type PersonsAPI = QueryParam "count" Int :> Get '[JSON] [Person]
type PersonAPI = Capture "id" Id :> Get '[JSON] Person

type LibrariesAPI = QueryParam "count" Int :> Get '[JSON] [Library]
type LibraryAPI = Capture "id" Id :> (
        "movies" :> (MoviesAPI :<|> MovieAPI)
    )

type MoviesAPI = QueryParam "count" Int :> Get '[JSON] [Movie]
type MovieAPI = Capture "id" Id :> Get '[JSON] Movie
