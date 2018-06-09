module MediaGoggler.Frontend.Style (StyleAPI, serveStyle) where

import Clay
import Protolude
import Servant (Get, (:>))
import Servant.CSS.Clay

import MediaGoggler.Monads (Server)

type StyleAPI = "styles.css" :> Get '[CSS] Css

serveStyle :: Server StyleAPI
serveStyle = pure dashboardStyle

dashboardStyle :: Css
dashboardStyle = body ? background darkgrey
