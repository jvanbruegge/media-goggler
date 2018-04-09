module MediaGoggler.Config where

import Data.Pool (Pool)
import Database.Bolt (Pipe)
import GHC.Generics (Generic)

data ServerState = ServerState
    { pool :: Pool Pipe
    } deriving Generic
