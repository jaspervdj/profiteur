--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Profiteur.Parser
    ( decode
    , profileToCostCentre
    ) where


--------------------------------------------------------------------------------
import qualified Data.IntMap     as IM
import qualified Data.Scientific as Scientific
import qualified Data.Set        as Set
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import qualified Data.Vector     as V
import qualified GHC.Prof        as Prof
import qualified GHC.Prof.Types  as Prof

import           Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
import           Profiteur.Core

--------------------------------------------------------------------------------
decode :: TL.Text -> Either String CostCentre
decode txt = Prof.decode txt >>= profileToCostCentre


--------------------------------------------------------------------------------
profileToCostCentre :: Prof.Profile -> Either String CostCentre
profileToCostCentre prof = do
    rootNo <- findRoot
    toCostCentreByNo rootNo
  where
    tree :: Prof.CostCentreTree
    tree = Prof.profileCostCentreTree prof

    findRoot :: Either String Prof.CostCentreNo
    findRoot = case IM.toList (Prof.costCentreParents tree) of
        []            -> Left "Could not find root node"
        ((_, no) : _) -> go no
      where
        go no = case IM.lookup no (Prof.costCentreParents tree) of
            Nothing  -> Right no
            Just par -> go par

    toCostCentreByNo :: Prof.CostCentreNo -> Either String CostCentre
    toCostCentreByNo no = do
        cc <- maybe (Left $ "Could not find CCN " ++ show no) Right $
            IM.lookup no (Prof.costCentreNodes tree)
        toCostCentreByNode cc

    toCostCentreByNode :: Prof.CostCentre -> Either String CostCentre
    toCostCentreByNode cc = do
        let no            = Prof.costCentreNo cc
            childrenNodes = maybe [] Set.toList $
                IM.lookup no (Prof.costCentreChildren tree)
        children <- V.mapM toCostCentreByNode (V.fromList childrenNodes)

        return CostCentre
            { ccName            = Prof.costCentreName cc
            , ccModule          = Prof.costCentreModule cc
            , ccSrc             = fromMaybe mempty $ Prof.costCentreSrc cc
            , ccId              = T.pack (show $ no)
            , ccEntries         = fromIntegral (Prof.costCentreEntries cc)
            , ccIndividualTime  = Scientific.toRealFloat (Prof.costCentreIndTime cc)
            , ccIndividualAlloc = Scientific.toRealFloat (Prof.costCentreIndAlloc cc)
            , ccInheritedTime   = Scientific.toRealFloat (Prof.costCentreInhTime cc)
            , ccInheritedAlloc  = Scientific.toRealFloat (Prof.costCentreInhAlloc cc)
            , ccChildren        = children
            }
