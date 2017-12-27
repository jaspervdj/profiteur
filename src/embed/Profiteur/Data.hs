{-# LANGUAGE TemplateHaskell #-}
module Profiteur.Data where

import Profiteur.DataType
import System.IO (Handle)
import qualified Data.ByteString       as B
import Data.FileEmbed
import Control.Arrow
import qualified Language.Javascript.JQuery as JQuery
import Language.Haskell.TH (runIO)
import Data.Maybe

includeFile :: Handle -> DataType -> IO ()
includeFile h filePath = B.hPutStr h $ data' filePath
  where
    data' JQuery = $(embedFile =<< runIO JQuery.file)
    data' (DataFile fp) =
      fromMaybe (error $ "No such datafile: " ++ fp) $ lookup fp dataDirContents
    dataDirContents = map (first ("data/" ++)) $(embedDir "data")
