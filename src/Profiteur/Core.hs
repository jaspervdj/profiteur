--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Profiteur.Core
    ( Prof
    , mkProf

    , CostCentreMap
    , mkCostCentreMap

    , CostCentreNode (..)
    , Name (..)
    , CostCentreInfo (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Aeson          ((.=))
import qualified Data.Aeson          as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V


--------------------------------------------------------------------------------
data Prof = Prof
    { pNodes :: !CostCentreMap
    , pRoot  :: !T.Text
    } deriving (Show)


--------------------------------------------------------------------------------
instance A.ToJSON Prof where
    toJSON p = A.object
        [ "nodes" .= pNodes p
        , "root"  .= pRoot  p
        ]


--------------------------------------------------------------------------------
mkProf :: CostCentreNode -> Prof
mkProf ccn = Prof
    { pNodes = mkCostCentreMap ccn
    , pRoot  = ccnId ccn
    }


--------------------------------------------------------------------------------
type CostCentreMap = HashMap Text CostCentreNode


--------------------------------------------------------------------------------
mkCostCentreMap :: CostCentreNode -> CostCentreMap
mkCostCentreMap = go HMS.empty
  where
    go :: CostCentreMap -> CostCentreNode -> CostCentreMap
    go ccm !ccn =
        HMS.insert (ccnId ccn) ccn $
        V.foldl' go ccm (ccnChildren ccn)


--------------------------------------------------------------------------------
data CostCentreNode = CostCentreNode
    { ccnName     :: !Name
    , ccnId       :: !T.Text
    , ccnInfo     :: !CostCentreInfo
    , ccnChildren :: !(Vector CostCentreNode)
    } deriving (Show)


--------------------------------------------------------------------------------
instance A.ToJSON CostCentreNode where
    toJSON ccn = A.object
        [ "name"     .= ccnName ccn
        , "id"       .= ccnId   ccn
        , "info"     .= ccnInfo ccn
        , "children" .= V.map ccnId (ccnChildren ccn)
        ]


--------------------------------------------------------------------------------
data Name = Name
    { nCanonical :: !Text
    , nModule    :: !Text
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
instance A.ToJSON Name where
    toJSON n = A.object
        [ "canonical" .= nCanonical n
        , "module"    .= nModule    n
        ]


--------------------------------------------------------------------------------
data CostCentreInfo = CostCentreInfo
    { cciEntries         :: !Int
    , cciIndividualTime  :: !Double
    , cciIndividualAlloc :: !Double
    , cciInheritedTime   :: !Double
    , cciInheritedAlloc  :: !Double
    } deriving (Show)


--------------------------------------------------------------------------------
instance A.ToJSON CostCentreInfo where
    toJSON cci = A.object
        [ "entries"         .= cciEntries         cci
        , "individualTime"  .= cciIndividualTime  cci
        , "individualAlloc" .= cciIndividualAlloc cci
        , "inheritedTime"   .= cciInheritedTime   cci
        , "inheritedAlloc"  .= cciInheritedAlloc  cci
        ]
