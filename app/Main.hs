--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy       as BL
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)

--------------------------------------------------------------------------------
import           Profiteur


--------------------------------------------------------------------------------
writeReport :: FilePath -> BL.ByteString -> IO ()
writeReport = BL.writeFile . (++ ".html")


--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        [profFile] -> do
            profOrErr <- readProfFile profFile
            case profOrErr of
                Right prof ->
                    writeReport profFile =<< htmlReport profFile prof
                Left err   -> do
                    putStrLn $ profFile ++ ": " ++ err
                    exitFailure
        _          -> do
            putStrLn $ "Usage: " ++ progName ++ " <prof file>"
            exitFailure
