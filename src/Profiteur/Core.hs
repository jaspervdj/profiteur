--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Profiteur.Core
    ( CostCentre (..)
    , Node (..)
    , nodesFromCostCentre
    , NodeMap (..)
    , nodeMapFromNodes
    , nodeMapFromCostCentre
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (guard)
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HMS
import           Data.List           (foldl')
import           Data.Maybe          (maybeToList)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import qualified Data.Vector         as V


--------------------------------------------------------------------------------
type Id = T.Text


--------------------------------------------------------------------------------
data CostCentre = CostCentre
    { ccName            :: !T.Text
    , ccModule          :: !T.Text
    , ccId              :: !Id
    , ccEntries         :: !Int
    , ccIndividualTime  :: !Double
    , ccIndividualAlloc :: !Double
    , ccInheritedTime   :: !Double
    , ccInheritedAlloc  :: !Double
    , ccChildren        :: !(V.Vector CostCentre)
    } deriving (Show)


--------------------------------------------------------------------------------
data Node = Node
    { nId       :: !Id
    , nName     :: !T.Text
    , nModule   :: !T.Text
    , nEntries  :: !Int
    , nTime     :: !Double
    , nAlloc    :: !Double
    , nChildren :: !(V.Vector Id)
    } deriving (Show)


--------------------------------------------------------------------------------
nodesFromCostCentre :: CostCentre -> [Node]
nodesFromCostCentre cc =
    self : maybeToList indiv ++
    concatMap nodesFromCostCentre (V.toList $ ccChildren cc)
  where
    self = Node
        { nId       = ccId cc
        , nName     = ccName cc
        , nModule   = ccModule cc
        , nEntries  = ccEntries cc
        , nTime     = ccInheritedTime cc
        , nAlloc    = ccInheritedAlloc cc
        , nChildren = V.fromList $
            maybeToList (nId <$> indiv) ++ map ccId (V.toList $ ccChildren cc)
        }

    indiv = do
        guard $ ccIndividualTime cc > 0 || ccIndividualAlloc cc > 0
        return Node
            { nId       = nId self <> ".indiv"
            , nName     = ccName cc <> " (indiv)"
            , nModule   = ccModule cc
            , nEntries  = ccEntries cc
            , nTime     = ccIndividualTime cc
            , nAlloc    = ccIndividualAlloc cc
            , nChildren = V.empty
            }


--------------------------------------------------------------------------------
instance A.ToJSON Node where
    toJSON Node {..} = A.toJSON
        [ A.toJSON nName
        , A.toJSON nModule
        , A.toJSON nEntries
        , A.toJSON nTime
        , A.toJSON nAlloc
        , A.toJSON nChildren
        ]


--------------------------------------------------------------------------------
data NodeMap = NodeMap
    { nmNodes :: !(HMS.HashMap Id Node)
    , nmRoot  :: !Id
    } deriving (Show)


--------------------------------------------------------------------------------
instance A.ToJSON NodeMap where
    toJSON NodeMap {..} = A.toJSON
        [ A.toJSON nmNodes
        , A.toJSON nmRoot
        ]


--------------------------------------------------------------------------------
nodeMapFromNodes :: Id -> [Node] -> NodeMap
nodeMapFromNodes root nodes = NodeMap
    { nmNodes = foldl' (\acc n -> HMS.insert (nId n) n acc) HMS.empty nodes
    , nmRoot  = root
    }


--------------------------------------------------------------------------------
nodeMapFromCostCentre :: CostCentre -> NodeMap
nodeMapFromCostCentre root =
    nodeMapFromNodes (ccId root) (nodesFromCostCentre root)
