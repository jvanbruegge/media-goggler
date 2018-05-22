{-# OPTIONS_GHC -fno-warn-orphans #-}

module MediaGoggler.Raw (serveFile) where

import Protolude
import Servant hiding (Server)
import System.FilePath (FilePath)
import Path (toFilePath)
import Conduit (ConduitT, runConduitRes, (.|), sourceFile, mapM_C, takeC)
import Control.Monad.Trans.Resource (ResourceT)

import MediaGoggler.API (FileStream, NoFraming, OggVideo)
import MediaGoggler.Datatypes (Id, VideoFile(..))
import MediaGoggler.DBEntry (DBEntry(..))
import MediaGoggler.Monads (Server)
import MediaGoggler.Error (except404)
import MediaGoggler.Filesystem (prefixPath)
import qualified MediaGoggler.Database as DB

instance ToStreamGenerator (ConduitT () o (ResourceT IO) ()) o where
    toStreamGenerator s = StreamGenerator $ \a b -> runConduitRes $ s .| op a b
        where op :: (o -> IO ()) -> (o -> IO ()) -> ConduitT o Void (ResourceT IO) ()
              op start rest = do
                takeC 1 .| mapM_C (liftIO . start)
                mapM_C $ liftIO . rest


serveFile :: Id -> Server (StreamGet NoFraming OggVideo FileStream)
serveFile id = except404 (getFilePath id) >>= pure . Conduit.sourceFile

getFilePath :: (DB.MonadDB m) => Id -> m (Either Text FilePath)
getFilePath id = DB.getVideoFile id >>= \case
    Left e -> pure $ Left e
    Right (DBEntry _ VideoFile{ path }) -> pure . Right . toFilePath . prefixPath $ path


