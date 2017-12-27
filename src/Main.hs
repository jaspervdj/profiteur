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
import           Data.Version               (showVersion)
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeBaseName)
import qualified System.IO                  as IO


--------------------------------------------------------------------------------
import           Paths_profiteur            (version)
import           Profiteur.Core
import           Profiteur.Parser
import           Profiteur.DataFile


--------------------------------------------------------------------------------
writeReport :: IO.Handle -> String -> NodeMap -> IO ()
writeReport h profFile prof = do
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

    includeJs JQueryFile
    includeJs "data/js/unicode.js"
    includeJs "data/js/model.js"
    includeJs "data/js/resizing-canvas.js"
    includeJs "data/js/node.js"
    includeJs "data/js/selection.js"
    includeJs "data/js/zoom.js"
    includeJs "data/js/details.js"
    includeJs "data/js/sorting.js"
    includeJs "data/js/tree-map.js"
    includeJs "data/js/tree-browser.js"
    includeJs "data/js/main.js"

    BC8.hPutStrLn h
        "  </head>\n\
        \  <body>"
    includeFile h "data/html/body.html"
    BC8.hPutStrLn h
        "  </body>\
        \</html>"
  where
    title    = T.pack $ takeBaseName profFile

    includeJs file = do
        BC8.hPutStrLn h "<script type=\"text/javascript\">"
        includeFile h file
        BC8.hPutStrLn h "</script>"

--------------------------------------------------------------------------------
makeReport :: IO.Handle -> FilePath -> IO ()
makeReport h profFile = do
    profOrErr <- decode <$> TL.readFile profFile
    case profOrErr of
        Right prof ->
            writeReport h profFile $ nodeMapFromCostCentre prof
        Left err   -> do
            putStrLnErr $ profFile ++ ": " ++ err
            exitFailure

--------------------------------------------------------------------------------
putStrLnErr :: String -> IO ()
putStrLnErr = IO.hPutStrLn IO.stderr

--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        _ | "--version" `elem` args ->
            putStrLnErr (showVersion version)
        [profFile] ->
            let htmlFile = profFile ++ ".html"
            in IO.withBinaryFile htmlFile IO.WriteMode $ \h ->
                  makeReport h profFile
        [profFile, "-"] ->
            makeReport IO.stdout profFile
        [profFile, htmlFile] ->
            IO.withBinaryFile htmlFile IO.WriteMode $ \h ->
                makeReport h profFile
        _ -> do
            putStrLnErr $ "Usage: " ++ progName ++ " <prof file> [<output file>]"
            putStrLnErr   "   <output file> \"-\" means STDOUT"
            exitFailure
