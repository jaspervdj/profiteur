module Profiteur.Report where

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Builder
import           System.FilePath            (takeBaseName)

import           Profiteur.Core
import           Data.Monoid ((<>))

htmlReportBase :: Monad m => (FilePath -> m Builder) -> FilePath -> NodeMap -> m BL.ByteString
htmlReportBase includeFile' profFile prof = toLazyByteString <$> do
    js <- mapM includeJs [
                       "data/lib/jquery-1.11.0.min.js"
                     , "data/js/unicode.js"
                     , "data/js/model.js"
                     , "data/js/resizing-canvas.js"
                     , "data/js/node.js"
                     , "data/js/selection.js"
                     , "data/js/zoom.js"
                     , "data/js/details.js"
                     , "data/js/sorting.js"
                     , "data/js/tree-map.js"
                     , "data/js/tree-browser.js"
                     , "data/js/main.js"
                    ]
    css <- includeFile' "data/css/main.css"
    body <- includeFile' "data/html/body.html"

    return $
      string8
          "<!DOCTYPE html>\n\
          \<html>\n\
          \  <head>\n\
          \    <meta charset=\"UTF-8\">\n\
          \    <title>" <> stringUtf8 title <> string8 "</title>"

      <> string8 "<script type=\"text/javascript\">var $prof = "
      <> lazyByteString (Aeson.encode prof)
      <> string8 ";</script>"

      <> string8 "<style>"
      <> css
      <> string8 "</style>"

      <> mconcat js

      <> string8
          "  </head>\n\
          \  <body>"
      <> body
      <> string8
            "  </body>\
            \</html>"
  where
    title    = takeBaseName profFile

    includeJs file = do
      f <- includeFile' file
      return $
           string8 "<script type=\"text/javascript\">"
        <> f
        <> string8 "</script>"
