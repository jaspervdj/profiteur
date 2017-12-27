module Profiteur.Data where

import           Paths_profiteur            (getDataFileName)
import System.IO (Handle)
import qualified Data.ByteString.Lazy       as BL
import qualified Language.Javascript.JQuery as JQuery
import Profiteur.DataType

includeFile :: Handle -> DataType -> IO ()
includeFile h JQuery =
    BL.hPutStr h =<< BL.readFile =<< JQuery.file
includeFile h (DataFile filePath) =
    BL.hPutStr h =<< BL.readFile =<< getDataFileName filePath
