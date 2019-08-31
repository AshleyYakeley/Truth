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

type PinaforeTypeNamespace baseupdate w polarity = TypeNamespace JMIsoShim (PinaforeTypeSystem baseupdate) w polarity

renameTypeArgs ::
       forall baseupdate (polarity :: Polarity) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanVarianceMap JMShim dv gt
    -> PinaforeTypeNamespace baseupdate (DolanArguments dv (PinaforeType baseupdate) gt) polarity
renameTypeArgs dvt dvm args = mapDolanArgumentsM renamePinaforeTypeVars dvt (bijectDolanVarianceMap dvm) args

renamePinaforeSingularTypeVars ::
       forall baseupdate polarity. Is PolarityType polarity
    => PinaforeTypeNamespace baseupdate (PinaforeSingularType baseupdate) polarity
renamePinaforeSingularTypeVars (GroundPinaforeSingularType gt args) = do
    MkShimWit args' bij <-
        renameTypeArgs @baseupdate @polarity (pinaforeGroundTypeVarianceType gt) (pinaforeGroundTypeVarianceMap gt) args
    return $ MkShimWit (GroundPinaforeSingularType gt args') bij
renamePinaforeSingularTypeVars (VarPinaforeSingularType namewit1) =
    renameUVar @_ @JMShim varNamespaceTRename namewit1 $ \namewit2 bij ->
        return $
        MkShimWit (VarPinaforeSingularType namewit2) $
        case representative @_ @_ @polarity of
            PositiveType -> MkJMIsoShim bij
            NegativeType -> MkJMIsoShim $ invert bij

renamePinaforeTypeVars ::
       forall baseupdate polarity. Is PolarityType polarity
    => PinaforeTypeNamespace baseupdate (PinaforeType baseupdate) polarity
renamePinaforeTypeVars NilPinaforeType =
    return $
    case representative @_ @_ @polarity of
        PositiveType -> MkShimWit NilPinaforeType id
        NegativeType -> MkShimWit NilPinaforeType id
renamePinaforeTypeVars (ConsPinaforeType ta tb) = do
    MkShimWit ta' bija <- renamePinaforeSingularTypeVars ta
    MkShimWit tb' bijb <- renamePinaforeTypeVars tb
    return $
        MkShimWit (ConsPinaforeType ta' tb') $
        case representative @_ @_ @polarity of
            PositiveType -> MkJMIsoShim $ biJoinBimap (unJMIsoShim bija) (unJMIsoShim bijb)
            NegativeType -> MkJMIsoShim $ biMeetBimap (unJMIsoShim bija) (unJMIsoShim bijb)

instance Renamer (VarRenamerT (PinaforeTypeSystem baseupdate)) where
    type RenamerNamespaceT (VarRenamerT (PinaforeTypeSystem baseupdate)) = VarNamespaceT (PinaforeTypeSystem baseupdate)
    type RenamerNegWitness (VarRenamerT (PinaforeTypeSystem baseupdate)) = PinaforeType baseupdate 'Negative
    type RenamerPosWitness (VarRenamerT (PinaforeTypeSystem baseupdate)) = PinaforeType baseupdate 'Positive
    type RenamerShim (VarRenamerT (PinaforeTypeSystem baseupdate)) = JMShim
    renameNegWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars t
        return $ MkShimWit t' $ isoForwards $ unJMIsoShim bij
    renamePosWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars t
        return $ MkShimWit t' $ isoForwards $ unJMIsoShim bij
    renameNewVar = do
        n <- varRenamerTGenerate
        valueToWitness n $ \wit ->
            return $
            MkNewVar
                (MkShimWit (singlePinaforeType $ VarPinaforeSingularType wit) meet1)
                (MkShimWit (singlePinaforeType $ VarPinaforeSingularType wit) join1)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
