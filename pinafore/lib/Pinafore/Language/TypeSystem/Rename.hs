{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.TypeSystem.Rename
    ( PinaforeTypeSystem
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.UVar
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

type TypeNamespace (cat :: forall kc. kc -> kc -> Type) (ts :: Type) (w :: Polarity -> k -> Type) (polarity :: Polarity)
     = forall t m. Monad m => w polarity t -> VarNamespaceT ts (VarRenamerT ts m) (PShimWit cat w polarity t)

type PinaforeTypeNamespace w polarity = TypeNamespace JMIsoShim PinaforeTypeSystem w polarity

renameTypeArgs ::
       forall (polarity :: Polarity) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> PinaforeTypeNamespace (DolanArguments dv PinaforeType gt) polarity
renameTypeArgs dvt dvm args = mapDolanArgumentsM renamePinaforeTypeVars dvt dvm args

renamePinaforeSingularTypeVars ::
       forall polarity. Is PolarityType polarity
    => PinaforeTypeNamespace PinaforeSingularType polarity
renamePinaforeSingularTypeVars (GroundPinaforeSingularType gt args) = do
    MkShimWit args' bij <-
        renameTypeArgs @polarity (pinaforeGroundTypeVarianceType gt) (pinaforeGroundTypeVarianceMap gt) args
    return $ MkShimWit (GroundPinaforeSingularType gt args') bij
renamePinaforeSingularTypeVars (VarPinaforeSingularType namewit1) =
    renameUVar @_ @JMShim varNamespaceTRename namewit1 $ \namewit2 bij ->
        return $
        MkShimWit (VarPinaforeSingularType namewit2) $
        MkPolarMap $
        case polarityType @polarity of
            PositiveType -> MkJMIsoShim bij
            NegativeType -> MkJMIsoShim $ invert bij

renamePinaforeTypeVars ::
       forall polarity. Is PolarityType polarity
    => PinaforeTypeNamespace PinaforeType polarity
renamePinaforeTypeVars NilPinaforeType =
    return $
    case polarityType @polarity of
        PositiveType -> MkShimWit NilPinaforeType id
        NegativeType -> MkShimWit NilPinaforeType id
renamePinaforeTypeVars (ConsPinaforeType ta tb) = do
    MkShimWit ta' bija <- renamePinaforeSingularTypeVars ta
    MkShimWit tb' bijb <- renamePinaforeTypeVars tb
    return $ MkShimWit (ConsPinaforeType ta' tb') $ jmIsoBimap bija bijb

instance Renamer (VarRenamerT PinaforeTypeSystem) where
    type RenamerNamespaceT (VarRenamerT PinaforeTypeSystem) = VarNamespaceT PinaforeTypeSystem
    type RenamerNegWitness (VarRenamerT PinaforeTypeSystem) = PinaforeType 'Negative
    type RenamerPosWitness (VarRenamerT PinaforeTypeSystem) = PinaforeType 'Positive
    type RenamerShim (VarRenamerT PinaforeTypeSystem) = JMShim
    renameNegWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars t
        return $ MkShimWit t' $ jmIsoForwards bij
    renamePosWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars t
        return $ MkShimWit t' $ jmIsoForwards bij
    renameNewVar = do
        n <- varRenamerTGenerate
        valueToWitness n $ \wit ->
            return $
            MkNewVar
                (singlePinaforeShimWit $ mkShimWit $ VarPinaforeSingularType wit)
                (singlePinaforeShimWit $ mkShimWit $ VarPinaforeSingularType wit)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
