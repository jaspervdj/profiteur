--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Profiteur.Parser
    ( parseProf
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative              ((<$>))
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
parseProf :: AP.Parser Prof
parseProf = fmap mkProf $ do
    -- Hacky stuff.
    _ <- AP.manyTill AP8.anyChar (AP.try $ AP8.string "COST CENTRE")
    _ <- AP.manyTill AP8.anyChar (AP.try $ AP8.string "COST CENTRE")
    _ <- AP.skipWhile (not . AP8.isEndOfLine)
    _ <- AP8.skipSpace
    parseContCentreNode 0


--------------------------------------------------------------------------------
parseContCentreNode :: Int -> AP.Parser CostCentreNode
parseContCentreNode indent = do
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

    children <- AP.many' $ parseContCentreNode (indent + 1)

    return CostCentreNode
        { ccnName     = Name
            { nCanonical = canonical
            , nModule    = module'
            }
        , ccnId       = id'
        , ccnInfo     = CostCentreInfo
            { cciEntries         = entries
            , cciIndividualTime  = individualTime
            , cciIndividualAlloc = individualAlloc
            , cciInheritedTime   = inheritedTime
            , cciInheritedAlloc  = inheritedAlloc
            }
        , ccnChildren = V.fromList children
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
