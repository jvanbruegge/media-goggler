{-# LANGUAGE QuasiQuotes #-}

module MediaGoggler.Filesystem where

import Protolude
import Path (Path, Abs, Dir, File, absdir)
import Path.IO (walkDirAccum)

import MediaGoggler.Database (MonadDB, fileExistsInDb)

root :: Path Abs Dir
root = [absdir|/media|]

getNewFiles :: (MonadDB m, MonadIO m) => m [Path Abs File]
getNewFiles = walkDirAccum Nothing writer root
    where writer _ _ files = mapM checkFile files >>= pure . concat
          checkFile path = fileExistsInDb path >>= \case
              True -> pure mempty
              False -> pure [path]
