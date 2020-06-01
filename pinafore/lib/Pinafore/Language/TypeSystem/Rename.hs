{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.TypeSystem.Rename
    ( PinaforeTypeSystem
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.UVar
import Pinafore.Language.Shim
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

type TypeNamespace :: forall k. MapKind k -> Type -> (Polarity -> k -> Type) -> Type
type TypeNamespace cat ts w
     = forall t m polarity.
           (Monad m, Is PolarityType polarity) =>
                   Proxy polarity -> w polarity t -> VarNamespaceT ts (VarRenamerT ts m) (PShimWit cat w polarity t)

type PinaforeTypeNamespace :: forall k. (Polarity -> k -> Type) -> Type
type PinaforeTypeNamespace (w :: Polarity -> k -> Type) = TypeNamespace (PolyIso PinaforeShim k) PinaforeTypeSystem w

renameTypeArgs ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> PinaforeTypeNamespace (DolanArguments dv PinaforeType gt)
renameTypeArgs dvt dvm _ args =
    mapDolanArgumentsM @_ @(PolyIso PinaforeShim) (renamePinaforeTypeVars Proxy) dvt dvm args

renamePinaforeSingularTypeVars :: PinaforeTypeNamespace PinaforeSingularType
renamePinaforeSingularTypeVars proxy (GroundPinaforeSingularType gt args) = do
    MkShimWit args' bij <-
        renameTypeArgs (pinaforeGroundTypeVarianceType gt) (pinaforeGroundTypeVarianceMap gt) proxy args
    return $ MkShimWit (GroundPinaforeSingularType gt args') bij
renamePinaforeSingularTypeVars (_ :: _ polarity) (VarPinaforeSingularType namewit1) =
    renameUVar @_ @(PinaforeShim Type) varNamespaceTRename namewit1 $ \namewit2 bij ->
        return $
        MkShimWit (VarPinaforeSingularType namewit2) $
        MkPolarMap $
        case polarityType @polarity of
            PositiveType -> MkPolyMapT bij
            NegativeType -> MkPolyMapT $ invert bij

renamePinaforeTypeVars :: PinaforeTypeNamespace PinaforeType
renamePinaforeTypeVars (_ :: _ polarity) NilPinaforeType =
    return $
    case polarityType @polarity of
        PositiveType -> MkShimWit NilPinaforeType id
        NegativeType -> MkShimWit NilPinaforeType id
renamePinaforeTypeVars _ (ConsPinaforeType ta tb) = do
    MkShimWit ta' bija <- renamePinaforeSingularTypeVars Proxy ta
    MkShimWit tb' bijb <- renamePinaforeTypeVars Proxy tb
    return $ MkShimWit (ConsPinaforeType ta' tb') $ polarPolyIsoBimap bija bijb

instance Renamer (VarRenamerT PinaforeTypeSystem) where
    type RenamerNamespaceT (VarRenamerT PinaforeTypeSystem) = VarNamespaceT PinaforeTypeSystem
    type RenamerNegWitness (VarRenamerT PinaforeTypeSystem) = PinaforeType 'Negative
    type RenamerPosWitness (VarRenamerT PinaforeTypeSystem) = PinaforeType 'Positive
    type RenamerShim (VarRenamerT PinaforeTypeSystem) = PinaforeShim Type
    renameNegWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars Proxy t
        return $ MkShimWit t' $ polarPolyIsoForwards bij
    renamePosWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars Proxy t
        return $ MkShimWit t' $ polarPolyIsoForwards bij
    renameNewVar = do
        n <- varRenamerTGenerate
        valueToWitness n $ \wit ->
            return $
            MkNewVar
                (singlePinaforeShimWit $ mkShimWit $ VarPinaforeSingularType wit)
                (singlePinaforeShimWit $ mkShimWit $ VarPinaforeSingularType wit)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
