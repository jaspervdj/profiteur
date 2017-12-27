module Profiteur.DataFile.Internal where

import Data.String

data DataType = JQuery | DataFile FilePath deriving (Show)

instance IsString DataType where
  fromString = DataFile
