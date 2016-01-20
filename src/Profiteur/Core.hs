--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Profiteur.Core
    ( CostCentre (..)
    , Node (..)
    , nodesFromCostCentre
    , NodeMap (..)
    , mkNodeMap
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
data CostCentre = CostCentre
    { ccName            :: !T.Text
    , ccModule          :: !T.Text
    , ccId              :: !Int
    , ccEntries         :: !Int
    , ccIndividualTime  :: !Double
    , ccIndividualAlloc :: !Double
    , ccInheritedTime   :: !Double
    , ccInheritedAlloc  :: !Double
    , ccChildren        :: !(V.Vector CostCentre)
    } deriving (Show)


--------------------------------------------------------------------------------
type Id = T.Text


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
nodesFromCostCentre CostCentre {..} =
    self : children
  where
    self = Node
        { nId       = T.pack (show ccId)
        , nName     = ccName
        , nModule   = ccModule
        , nEntries  = ccEntries
        , nTime     = ccInheritedTime
        , nAlloc    = ccInheritedAlloc
        , nChildren = V.fromList (map nId children)
        }

    children =
        maybeToList indiv ++ concatMap nodesFromCostCentre (V.toList ccChildren)

    indiv = do
        guard $ ccIndividualTime > 0 || ccIndividualAlloc > 0
        return Node
            { nId       = nId self <> ".indiv"
            , nName     = ccName <> " (indiv)"
            , nModule   = ccModule
            , nEntries  = ccEntries
            , nTime     = ccIndividualTime
            , nAlloc    = ccIndividualAlloc
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
newtype NodeMap = NodeMap (HMS.HashMap Id Node)
    deriving (A.ToJSON)


--------------------------------------------------------------------------------
mkNodeMap :: [Node] -> NodeMap
mkNodeMap = NodeMap . foldl' (\acc n -> HMS.insert (nId n) n acc) HMS.empty
