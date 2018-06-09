module MediaGoggler.Frontend.Style (StyleAPI, serveStyle) where

import Protolude
import Clay
import Servant.CSS.Clay
import Servant (Get, (:>))

import MediaGoggler.Monads (Server)

type StyleAPI = "styles.css" :> Get '[CSS] Css

serveStyle :: Server StyleAPI
serveStyle = pure dashboardStyle

dashboardStyle :: Css
dashboardStyle = body ? background darkgrey
