{-# LANGUAGE CPP #-}
#ifdef EMBED_DATA_FILES
{-# LANGUAGE TemplateHaskell #-}
#endif
module Profiteur.Data where

import           Data.ByteString.Builder

#ifdef EMBED_DATA_FILES
import Data.FileEmbed

includeFilePure :: FilePath -> Builder
includeFilePure dataFile =
  case lookup dataFile myDir of
    Just bs -> byteString bs
    Nothing -> error $ "No file " ++ dataFile ++ " found"
  where
    myDir = map (\(a,b) -> ("data/"++a, b)) $(embedDir "data")

includeFile :: FilePath -> IO Builder
includeFile = return . includeFilePure

#else

import           Paths_profiteur            (getDataFileName)
import qualified Data.ByteString.Lazy       as BL

includeFile :: FilePath -> IO Builder
includeFile dataFile = do
    fileName <- getDataFileName dataFile
    lazyByteString <$> BL.readFile fileName

#endif
