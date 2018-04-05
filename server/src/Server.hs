module Server where

import Protolude
import Servant

import API
import Datatypes

server :: Server MediaGogglerAPI
server = getLibraries :<|> getLibrary
    where
        getLibrary :: Server LibraryAPI
        getLibrary = undefined

getLibraries :: Server LibrariesAPI
getLibraries _ = return [Library { movies = [] }]
