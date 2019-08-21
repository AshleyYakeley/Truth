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

type PinaforeTypeNamespace baseedit w polarity = TypeNamespace JMIsoShim (PinaforeTypeSystem baseedit) w polarity

renameTypeArgs ::
       forall baseedit (polarity :: Polarity) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanVarianceMap JMShim dv gt
    -> PinaforeTypeNamespace baseedit (DolanArguments dv (PinaforeType baseedit) gt) polarity
renameTypeArgs dvt dvm args = mapDolanArgumentsM renamePinaforeTypeVars dvt (bijectDolanVarianceMap dvm) args

renamePinaforeSingularTypeVars ::
       forall baseedit polarity. Is PolarityType polarity
    => PinaforeTypeNamespace baseedit (PinaforeSingularType baseedit) polarity
renamePinaforeSingularTypeVars (GroundPinaforeSingularType gt args) = do
    MkShimWit args' bij <-
        renameTypeArgs @baseedit @polarity (pinaforeGroundTypeVarianceType gt) (pinaforeGroundTypeVarianceMap gt) args
    return $ MkShimWit (GroundPinaforeSingularType gt args') bij
renamePinaforeSingularTypeVars (VarPinaforeSingularType namewit1) =
    renameUVar @_ @JMShim varNamespaceTRename namewit1 $ \namewit2 bij ->
        return $
        MkShimWit (VarPinaforeSingularType namewit2) $
        case representative @_ @_ @polarity of
            PositiveType -> MkJMIsoShim bij
            NegativeType -> MkJMIsoShim $ invert bij

renamePinaforeTypeVars ::
       forall baseedit polarity. Is PolarityType polarity
    => PinaforeTypeNamespace baseedit (PinaforeType baseedit) polarity
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

instance Renamer (VarRenamerT (PinaforeTypeSystem baseedit)) where
    type RenamerNamespaceT (VarRenamerT (PinaforeTypeSystem baseedit)) = VarNamespaceT (PinaforeTypeSystem baseedit)
    type RenamerNegWitness (VarRenamerT (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'Negative
    type RenamerPosWitness (VarRenamerT (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'Positive
    type RenamerShim (VarRenamerT (PinaforeTypeSystem baseedit)) = JMShim
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
