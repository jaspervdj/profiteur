--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.IO          as TL
import qualified Language.Javascript.JQuery as JQuery
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
includeFile h filePath =
    BL.hPutStr h =<< BL.readFile filePath


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
    includeFile h =<< getDataFileName "data/css/main.css"
    BC8.hPutStrLn h "</style>"

    includeJs h =<< JQuery.file
    includeJs h =<< getDataFileName "data/js/unicode.js"
    includeJs h =<< getDataFileName "data/js/model.js"
    includeJs h =<< getDataFileName "data/js/resizing-canvas.js"
    includeJs h =<< getDataFileName "data/js/node.js"
    includeJs h =<< getDataFileName "data/js/selection.js"
    includeJs h =<< getDataFileName "data/js/zoom.js"
    includeJs h =<< getDataFileName "data/js/details.js"
    includeJs h =<< getDataFileName "data/js/sorting.js"
    includeJs h =<< getDataFileName "data/js/tree-map.js"
    includeJs h =<< getDataFileName "data/js/tree-browser.js"
    includeJs h =<< getDataFileName "data/js/main.js"

    BC8.hPutStrLn h
        "  </head>\n\
        \  <body>"
    includeFile h =<< getDataFileName "data/html/body.html"
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
            profOrErr <- decode <$> TL.readFile profFile
            case profOrErr of
                Right prof ->
                    writeReport profFile $ nodeMapFromCostCentre prof
                Left err   -> do
                    putStrLn $ profFile ++ ": " ++ err
                    exitFailure
        _          -> do
            putStrLn $ "Usage: " ++ progName ++ " <prof file>"
            exitFailure
