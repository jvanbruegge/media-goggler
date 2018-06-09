module MediaGoggler.Frontend.Index where

import Protolude
import Lucid
import Servant hiding (Server)
import Servant.HTML.Lucid

import MediaGoggler.Monads (Server)

type FrontendAPI = Get '[HTML] (Html ())

serveFrontend :: Server FrontendAPI
serveFrontend = pure html
    where html :: Html ()
          html = do
              h1_ "Media Goggler"
              p_ "To stream your media with haskell"
              with video_
                  [ autoplay_ ""
                  , controls_ ""
                  , src_ "/api/files/3413857a-0bd0-4335-8757-863261c69dc6/raw"
                  , height_ "600"]
                  ""
