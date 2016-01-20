--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Profiteur.Parser
    ( parseFile
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                    (replicateM_)
import           Data.Attoparsec.ByteString       as AP
import           Data.Attoparsec.ByteString.Char8 as AP8
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Vector                      as V


--------------------------------------------------------------------------------
import           Profiteur.Core


--------------------------------------------------------------------------------
parseFile :: AP.Parser CostCentre
parseFile = do
    -- Hacky stuff.
    _ <- AP.manyTill AP8.anyChar (AP.try $ AP8.string "COST CENTRE")
    _ <- AP.manyTill AP8.anyChar (AP.try $ AP8.string "COST CENTRE")
    _ <- AP.skipWhile (not . AP8.isEndOfLine)
    _ <- AP8.skipSpace
    paresCostCentre 0


--------------------------------------------------------------------------------
paresCostCentre :: Int -> AP.Parser CostCentre
paresCostCentre indent = do
    replicateM_ indent $ AP8.char8 ' '
    canonical <- identifier
    skipHorizontalSpace
    module' <- identifier
    skipHorizontalSpace
    id' <- T.pack . show <$> (AP8.decimal :: AP.Parser Int)

    skipHorizontalSpace
    entries <- AP8.decimal
    skipHorizontalSpace
    individualTime <- AP8.double
    skipHorizontalSpace
    individualAlloc <- AP8.double
    skipHorizontalSpace
    inheritedTime <- AP8.double
    skipHorizontalSpace
    inheritedAlloc <- AP8.double
    skipToEol

    children <- AP.many' $ paresCostCentre (indent + 1)

    return CostCentre
        { ccName            = canonical
        , ccModule          = module'
        , ccId              = id'
        , ccEntries         = entries
        , ccIndividualTime  = individualTime
        , ccIndividualAlloc = individualAlloc
        , ccInheritedTime   = inheritedTime
        , ccInheritedAlloc  = inheritedAlloc
        , ccChildren        = V.fromList children
        }


--------------------------------------------------------------------------------
identifier :: AP.Parser Text
identifier = T.decodeUtf8 <$> AP8.takeWhile (not . AP8.isSpace)


--------------------------------------------------------------------------------
skipHorizontalSpace :: AP.Parser ()
skipHorizontalSpace = AP.skipWhile AP8.isHorizontalSpace


--------------------------------------------------------------------------------
skipToEol :: AP.Parser ()
skipToEol = do
    AP.skipWhile (not . AP8.isEndOfLine)
    AP8.endOfLine
