{-# OPTIONS_GHC -fno-warn-orphans #-}

module MediaGoggler.Raw (serveFile, serveRange) where

import Protolude
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.ByteString (hGetSome)
import qualified Data.Text as T
import Servant hiding (Server)
import System.FilePath (FilePath)
import Path (toFilePath)
import Conduit (ConduitT, runConduitRes, (.|), mapM_C, takeC, bracketP, yield)
import Control.Monad.Trans.Resource (ResourceT, MonadResource)
import System.IO (hSeek, SeekMode(AbsoluteSeek), hClose, openBinaryFile)
import System.PosixCompat.Files (fileSize, getFileStatus)
import System.Posix.Types (COff(..))

import MediaGoggler.API (VideoStream)
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

serveFile :: Id -> Server (VideoStream 200)
serveFile = flip serveRange Nothing

serveRange :: Id -> Maybe Text -> Server (VideoStream 206)
serveRange id range = do
        path <- except404 (getFilePath id)
        COff len <- fileSize <$> liftIO (getFileStatus path)
        let (s, e) = let (a, b) = fromMaybe (Nothing, Nothing) (parseRange <$> range) in (fromMaybe 0 a, fromMaybe (len - 1) b)
        pure . addHeader "bytes" . addHeader (e - s + 1) .
                addHeader ("bytes " <> (show s) <> "-" <> (show e) <> "/" <> (show len)) $ sourceFilePart path s e

parseRange :: Text -> (Maybe Int64, Maybe Int64)
parseRange s = let (a, b) = (T.breakOn "-" (T.drop 6 s)) in
        (readMaybe (T.unpack a), readMaybe . T.unpack . T.drop 1 $ b)

getFilePath :: (DB.MonadDB m) => Id -> m (Either Text FilePath)
getFilePath id = DB.getVideoFile id >>= \case
    Left e -> pure $ Left e
    Right (DBEntry _ VideoFile{ path }) -> pure . Right . toFilePath . prefixPath $ path

sourceFilePart :: MonadResource m =>
    FilePath -> Int64 -> Int64 -> ConduitT i ByteString m ()
sourceFilePart fp start len = bracketP (openBinaryFile fp ReadMode) hClose (\h -> liftIO (hSeek h AbsoluteSeek (toInteger start)) >> loop h 0)
    where d = toInteger defaultChunkSize
          l = toInteger len
          loop h acc = do
                bs <- liftIO $ hGetSome h defaultChunkSize
                unless (acc >= l) $ do
                    yield bs
                    loop h (acc + d)
