{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Rename
    ( PinaforeTypeSystem
    ) where

import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.UVar
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

type LiftBijection (f :: kp -> kq) = forall (a :: kp) (b :: kp). KindBijection kp a b -> KindBijection kq (f a) (f b)

vcBijection ::
       forall (v :: SingleVariance) k (f :: SingleVarianceKind v -> k). HasKindMorphism k
    => SingleVarianceType v
    -> SingleVarianceMap v f
    -> LiftBijection f
vcBijection CovarianceType conv (MkBijection ab ba) = mkKindBijection (conv ab) (conv ba)
vcBijection ContravarianceType conv (MkBijection ba ab) = mkKindBijection (conv $ MkCatDual ab) (conv $ MkCatDual ba)
vcBijection RangevarianceType conv (MkPairMorphism (MkBijection papb pbpa) (MkBijection qaqb qbqa)) =
    mkKindBijection (conv $ MkWithRange pbpa qaqb) (conv $ MkWithRange papb qbqa)

type TypeNamespace (ts :: Type) (w :: k -> Type)
     = forall t1 m r.
           Monad m =>
                   w t1 -> (forall t2. InKind t2 => w t2 -> KindBijection k t1 t2 -> VarNamespace ts (VarRenamer ts m) r) -> VarNamespace ts (VarRenamer ts m) r

type PinaforeTypeNamespace baseedit w = TypeNamespace (PinaforeTypeSystem baseedit) w

renamePinaforeRangeTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeRangeType baseedit polarity)
renamePinaforeRangeTypeVars (MkRangeType ta tb) cont =
    invertPolarity @polarity $
    renamePinaforeTypeVars ta $ \ta' bija ->
        renamePinaforeTypeVars tb $ \tb' bijb -> cont (MkRangeType ta' tb') $ MkPairMorphism bija bijb

renameTypeArg ::
       forall baseedit polarity v. IsTypePolarity polarity
    => SingleVarianceType v
    -> PinaforeTypeNamespace baseedit (SingleArgument v (PinaforeType baseedit) polarity)
renameTypeArg CovarianceType = renamePinaforeTypeVars
renameTypeArg ContravarianceType =
    case isInvertPolarity @polarity of
        Dict -> renamePinaforeTypeVars
renameTypeArg RangevarianceType = renamePinaforeRangeTypeVars

renameTypeArgs ::
       forall baseedit (polarity :: TypePolarity) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanKindVary dv t
    -> PinaforeTypeNamespace baseedit (DolanArguments dv (PinaforeType baseedit) t polarity)
renameTypeArgs NilListType NilDolanKindVary NilDolanArguments cont = cont NilDolanArguments id
renameTypeArgs (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments arg args) cont =
    renameTypeArg @baseedit @polarity svt arg $ \arg' bijarg ->
        case dolanVarianceHasKM dvt of
            Dict ->
                bijectTypeArguments (vcBijection svt svm bijarg) args $ \args' bijargs ->
                    renameTypeArgs dvt dvm args' $ \args'' bijargs' ->
                        cont (ConsDolanArguments arg' args'') $ bijargs' . bijargs

renamePinaforeSinglularTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeSingularType baseedit polarity)
renamePinaforeSinglularTypeVars (GroundPinaforeSingularType gt args) cont =
    renameTypeArgs @baseedit @polarity (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args $ \args' bij ->
        cont (GroundPinaforeSingularType gt args') bij
renamePinaforeSinglularTypeVars (VarPinaforeSingularType namewit1) cont =
    renameUVar varNamespaceRename namewit1 $ \namewit2 bij -> cont (VarPinaforeSingularType namewit2) bij

renamePinaforeTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeType baseedit polarity)
renamePinaforeTypeVars NilPinaforeType cont = cont NilPinaforeType id
renamePinaforeTypeVars (ConsPinaforeType ta tb) cont =
    renamePinaforeSinglularTypeVars ta $ \ta' bija ->
        renamePinaforeTypeVars tb $ \tb' bijb -> cont (ConsPinaforeType ta' tb') $ jmBiMap @polarity bija bijb

instance Renamer (VarRenamer (PinaforeTypeSystem baseedit)) where
    type RenamerNamespace (VarRenamer (PinaforeTypeSystem baseedit)) = VarNamespace (PinaforeTypeSystem baseedit)
    type RenamerNegWitness (VarRenamer (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'NegativePolarity
    type RenamerPosWitness (VarRenamer (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'PositivePolarity
    renameTSNegWitness = renamePinaforeTypeVars
    renameTSPosWitness = renamePinaforeTypeVars
    renameNewVar cont = do
        n <- varRenamerGenerate
        toSymbolWitness n $ \wit ->
            cont (singlePinaforeType $ VarPinaforeSingularType wit) (singlePinaforeType $ VarPinaforeSingularType wit) $
            join1 . meet1
    namespace = runVarNamespace
    runRenamer = runVarRenamer
