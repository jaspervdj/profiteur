--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson                 as Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeBaseName)
import qualified System.IO                  as IO


--------------------------------------------------------------------------------
import           Paths_profiteur            (getDataFileName)
import           Profiteur.Core
import           Profiteur.Parser


--------------------------------------------------------------------------------
includeFile :: IO.Handle -> FilePath -> IO ()
includeFile h dataFile = do
    fileName <- getDataFileName dataFile
    BL.hPutStr h =<< BL.readFile fileName


--------------------------------------------------------------------------------
writeReport :: String -> NodeMap -> IO ()
writeReport profFile prof = IO.withBinaryFile htmlFile IO.WriteMode $ \h -> do
    BC8.hPutStrLn h $
        "<!DOCTYPE html>\n\
        \<html>\n\
        \  <head>\n\
        \    <meta charset=\"UTF-8\">\n\
        \    <title>" `mappend` T.encodeUtf8 title `mappend` "</title>"

    BC8.hPutStr h "<script type=\"text/javascript\">var $prof = "
    BL.hPutStr h $ Aeson.encode prof
    BC8.hPutStrLn h ";</script>"

    BC8.hPutStrLn h "<style>"
    includeFile h "data/css/main.css"
    BC8.hPutStrLn h "</style>"

    includeJs h "data/lib/jquery-1.11.0.min.js"
    includeJs h "data/js/unicode.js"
    includeJs h "data/js/model.js"
    includeJs h "data/js/resizing-canvas.js"
    includeJs h "data/js/node.js"
    includeJs h "data/js/individual-node.js"
    includeJs h "data/js/cost-centre-node.js"
    includeJs h "data/js/selection.js"
    includeJs h "data/js/zoom.js"
    includeJs h "data/js/details.js"
    includeJs h "data/js/sorting.js"
    includeJs h "data/js/tree-map.js"
    includeJs h "data/js/tree-browser.js"
    includeJs h "data/js/main.js"

    BC8.hPutStrLn h
        "  </head>\n\
        \  <body>"
    includeFile h "data/html/body.html"
    BC8.hPutStrLn h
        "  </body>\
        \</html>"

    putStrLn $ "Wrote " ++ htmlFile
  where
    htmlFile = profFile ++ ".html"
    title    = T.pack $ takeBaseName profFile

    includeJs h file = do
        BC8.hPutStrLn h "<script type=\"text/javascript\">"
        includeFile h file
        BC8.hPutStrLn h "</script>"


--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        [profFile] -> do
            profOrErr <- AP.parseOnly parseFile <$> B.readFile profFile
            case profOrErr of
                Right prof ->
                    writeReport profFile $ mkNodeMap $ nodesFromCostCentre prof
                Left err   -> do
                    putStrLn $ profFile ++ ": " ++ err
                    exitFailure
        _          -> do
            putStrLn $ "Usage: " ++ progName ++ " <prof file>"
            exitFailure
