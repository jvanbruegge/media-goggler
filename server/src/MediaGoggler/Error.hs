module MediaGoggler.Error where

import Protolude
import Servant hiding (Server)
import Data.String.Conversions (cs)

import qualified MediaGoggler.Database as DB

except500 :: (DB.MonadDB m, MonadError ServantErr m) => m (Either Text a) -> m a
except500 = eitherToExcept err500

except404 :: (DB.MonadDB m, MonadError ServantErr m) => m (Either Text a) -> m a
except404 = eitherToExcept err404

eitherToExcept :: ServantErr -> (DB.MonadDB m, MonadError ServantErr m) => m (Either Text a) -> m a
eitherToExcept err = (>>= \case
    Right x -> pure x
    Left e -> throwError $ err { errBody = cs e })
