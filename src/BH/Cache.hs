{-# LANGUAGE FlexibleContexts #-}

module BH.Cache
 ( readYaml,
   writeYaml,
 )
where

import qualified Data.Yaml as Y
import Control.Monad.Except
import Data.Either.Combinators
import System.Directory


-- FIXME: Use generic yaml reading func.
readYaml :: (Y.FromJSON a, MonadIO m, MonadError String m) => FilePath -> m a
readYaml file = do
  b <- liftIO (doesFileExist file)
  if b
    then (liftIO (Y.decodeFileEither file) >>= liftEither . mapLeft show)
    else throwError ("File with switch info not found " <> file)

writeYaml :: (Y.ToJSON a, MonadIO m, MonadError String m) => FilePath -> a -> m ()
writeYaml file x = liftIO $ Y.encodeFile file x
