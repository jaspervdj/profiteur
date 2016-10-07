{-# LANGUAGE CPP #-}
module Profiteur where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Functor.Identity

import           Profiteur.Core
import           Profiteur.Parser
import           Profiteur.Data
import           Profiteur.Report

htmlReport :: FilePath -> NodeMap -> IO BL.ByteString
htmlReport = htmlReportBase includeFile

#if EMBED_DATA_FILES
htmlReportPure :: FilePath -> NodeMap -> BL.ByteString
htmlReportPure = (runIdentity .) . htmlReportBase (return . includeFilePure)
#endif

readProfFile :: FilePath -> IO (Either String NodeMap)
readProfFile fp = do
  src <- B.readFile fp
  return $
    case AP.parseOnly parseFile src of
        Right prof -> Right $ nodeMapFromCostCentre prof
        Left err   -> Left err
