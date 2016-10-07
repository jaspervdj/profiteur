{-# LANGUAGE CPP #-}
module Profiteur where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
#if EMBED_DATA_FILES
import           Data.Functor.Identity
#endif

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
readProfFile fp = parseProfFile <$> B.readFile fp

parseProfFile :: B.ByteString -> Either String NodeMap
parseProfFile src =
  case AP.parseOnly parseFile src of
      Right prof -> Right $ nodeMapFromCostCentre prof
      Left err   -> Left err
