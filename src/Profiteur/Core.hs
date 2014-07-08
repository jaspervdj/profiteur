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
    , pRoot  :: !Int
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
        forIndiv (HMS.insert (T.pack $ show $ ccnId indiv) indiv) $
        HMS.insert (T.pack $ show $ ccnId ccn) ccn{ccnChildren=forIndiv (V.cons indiv) $ ccnChildren ccn} $
        V.foldl' go ccm $ ccnChildren ccn
      where forIndiv f | cciIndividualTime i < 1e-6 && cciIndividualAlloc i < 1e-6 = id
                       | otherwise = f
            indiv = CostCentreNode
              { ccnName     = new_name
              , ccnId       = 1000000 + ccnId ccn
              , ccnInfo     = new_info
              , ccnChildren = V.empty
              }
            new_name = let n = ccnName ccn
                           t = T.pack " (indiv)" `T.append` nCanonical n in
              n{nCanonical=t}
            i = ccnInfo ccn
            new_info = i{cciInheritedTime=cciIndividualTime i,cciInheritedAlloc=cciIndividualAlloc i}


--------------------------------------------------------------------------------
data CostCentreNode = CostCentreNode
    { ccnName     :: !Name
    , ccnId       :: !Int
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
