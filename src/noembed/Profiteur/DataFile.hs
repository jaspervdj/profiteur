module Profiteur.DataFile (
  includeFile,
  module Profiteur.DataFile.Internal
  ) where

import           Paths_profiteur            (getDataFileName)
import System.IO (Handle)
import qualified Data.ByteString.Lazy       as BL
import qualified Language.Javascript.JQuery as JQuery
import Profiteur.DataFile.Internal

includeFile :: Handle -> DataType -> IO ()
includeFile h JQueryFile =
    BL.hPutStr h =<< BL.readFile =<< JQuery.file
includeFile h (DataFile filePath) =
    BL.hPutStr h =<< BL.readFile =<< getDataFileName filePath
