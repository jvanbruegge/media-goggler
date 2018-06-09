module MediaGoggler.Frontend.Index (FrontendAPI, serveFrontend) where

import Protolude
import Lucid
import Servant hiding (Server)
import Servant.HTML.Lucid

import MediaGoggler.Monads (Server)
import MediaGoggler.Frontend.Style (StyleAPI, serveStyle)

type FrontendAPI = Get '[HTML] (Html ()) :<|> StyleAPI

serveFrontend :: Server FrontendAPI
serveFrontend = pure html :<|> serveStyle

api :: Proxy FrontendAPI
api = Proxy

styleApi :: Proxy StyleAPI
styleApi = Proxy

html :: Html ()
html = doctypehtml_ $ do
    head_ $ do
        title_ "MediaGoggler"
        link_ [rel_ "stylesheet"
              , href_ $ toUrlPiece $ safeLink api styleApi]

    body_ $ do
        h1_ "Media Goggler"
        p_ "To stream your media with haskell"
        with video_
            [ autoplay_ ""
            , controls_ ""
            , src_ "/api/files/3413857a-0bd0-4335-8757-863261c69dc6/raw"
            , height_ "600"]
            ""
