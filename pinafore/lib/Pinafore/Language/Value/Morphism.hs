module Pinafore.Language.Value.Morphism where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data PinaforeMorphism (pqa :: (Type, Type)) (pqb :: (Type, Type)) =
    forall a b. (Eq a, Eq b) =>
                    MkPinaforeMorphism (Range JMShim a pqa)
                                       (Range JMShim b pqb)
                                       (PinaforeLensMorphism PinaforeEntityUpdate a b)

instance CatFunctor (CatRange (->)) (->) (PinaforeMorphism a) where
    cfmap f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism ra (cfmap f rb) m

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) PinaforeMorphism where
    cfmap f = MkNestedMorphism $ \(MkPinaforeMorphism ra rb m) -> MkPinaforeMorphism (cfmap f ra) rb m

instance HasVariance 'Rangevariance (PinaforeMorphism) where
    varianceRepresentational = Nothing

instance HasVariance 'Rangevariance (PinaforeMorphism a) where
    varianceRepresentational = Nothing

pinaforeMorphismLens :: PinaforeMorphism '( a, a) '( b, b) -> PinaforeLensMorphism PinaforeEntityUpdate a b
pinaforeMorphismLens (MkPinaforeMorphism tra trb lm) =
    (bijectionPinaforeLensMorphism $ isoMapCat fromEnhanced $ rangeBijection trb) .
    lm . (bijectionPinaforeLensMorphism $ invert $ isoMapCat fromEnhanced $ rangeBijection tra)

pinaforeLensMorphism ::
       (Eq a, Eq b) => PinaforeLensMorphism PinaforeEntityUpdate a b -> PinaforeMorphism '( a, a) '( b, b)
pinaforeLensMorphism = MkPinaforeMorphism identityRange identityRange

pinaforeMorphismFunction ::
       PinaforeMorphism '( a, TopType) '( BottomType, b)
    -> PinaforeFunctionMorphism PinaforeEntityUpdate (Know a) (Know b)
pinaforeMorphismFunction (MkPinaforeMorphism tra trb pm) =
    proc ka -> do
        tb <- lensFunctionMorphism pm -< fmap (fromEnhanced $ rangeContra tra) ka
        returnA -< fmap (fromEnhanced $ rangeCo trb) tb

identityPinaforeMorphism :: forall t. PinaforeMorphism '( MeetType Entity t, t) '( MeetType Entity t, t)
identityPinaforeMorphism = MkPinaforeMorphism (coRange meet2) (coRange meet2) id

composePinaforeMorphism ::
       forall ap aq bp bq cp cq.
       PinaforeMorphism '( bq, bp) '( cp, cq)
    -> PinaforeMorphism '( aq, ap) '( bp, bq)
    -> PinaforeMorphism '( aq, ap) '( cp, cq)
composePinaforeMorphism (MkPinaforeMorphism tb1 tc1 m1) (MkPinaforeMorphism ta2 tb2 m2) =
    MkPinaforeMorphism ta2 tc1 $ m1 . bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges tb2 tb1) . m2

pairPinaforeMorphism ::
       forall ap aq bp bq cp cq.
       PinaforeMorphism '( ap, aq) '( bp, bq)
    -> PinaforeMorphism '( aq, ap) '( cp, cq)
    -> PinaforeMorphism '( ap, aq) '( (bp, cp), (bq, cq))
pairPinaforeMorphism (MkPinaforeMorphism ta1 tb1 m1) (MkPinaforeMorphism ta2 tc2 m2) =
    MkPinaforeMorphism ta1 (pairRange tb1 tc2) $
    pairPinaforeLensMorphism m1 $ m2 . bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges ta1 ta2)

eitherPinaforeMorphism ::
       forall ap aq bp bq cp cq.
       PinaforeMorphism '( ap, aq) '( cp, cq)
    -> PinaforeMorphism '( bp, bq) '( cq, cp)
    -> PinaforeMorphism '( Either ap bp, Either aq bq) '( cp, cq)
eitherPinaforeMorphism (MkPinaforeMorphism ta1 tc1 m1) (MkPinaforeMorphism tb2 tc2 m2) =
    MkPinaforeMorphism (eitherRange ta1 tb2) tc1 $
    eitherPinaforeLensMorphism m1 $ bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges tc2 tc1) . m2

pinaforeApplyMorphismRef ::
       forall ap aq bp bq. (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( aq, ap) '( bp, bq)
    -> PinaforeRef '( ap, aq)
    -> PinaforeRef '( bp, bq)
pinaforeApplyMorphismRef (MkPinaforeMorphism tra trb m) (MutablePinaforeRef tra' lv) =
    MutablePinaforeRef trb $
    applyPinaforeLens pinaforeSubEntity m $
    eaMap (bijectionWholeEditLens (cfmap $ isoMapCat fromEnhanced $ bijectRanges tra' tra)) lv
pinaforeApplyMorphismRef (MkPinaforeMorphism (MkRange fa _) trb m) (ImmutablePinaforeRef fv) =
    MutablePinaforeRef trb $
    applyPinaforeLens pinaforeSubEntity m $ immutableReferenceToRejectingValue $ fmap (fromEnhanced fa) fv

pinaforeApplyMorphismImmutRef ::
       forall a bp bq. (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( a, TopType) '( bp, bq)
    -> PinaforeImmutableReference a
    -> PinaforeRef '( bp, bq)
pinaforeApplyMorphismImmutRef m r = pinaforeApplyMorphismRef m $ pinaforeImmutableToRef r

pinaforeApplyMorphismSet ::
       forall a bp bq. (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( a, TopType) '( bp, bq)
    -> PinaforeFiniteSetRef '( BottomType, a)
    -> PinaforeFiniteSetRef '( bp, bq)
pinaforeApplyMorphismSet (MkPinaforeMorphism tra trb m) (MkPinaforeFiniteSetRef tra' ss) = let
    setm =
        proc st -> do
            skb <-
                cfmap $ lensFunctionMorphism m -<
                    fmap (Known . fromEnhanced (rangeContra tra) . fromEnhanced (rangeCo tra')) st
            returnA -< mapMaybe knowToMaybe skb
    in MkPinaforeFiniteSetRef trb $
       eaMap (convertEditLens . fromReadOnlyRejectingEditLens) $
       applyPinaforeFunction pinaforeSubEntity setm (eaToReadOnlyWhole ss)

pinaforeApplyInverseMorphismRef ::
       forall ap aq bp bq. (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( bp, bq) '( aq, ap)
    -> PinaforeRef '( ap, aq)
    -> PinaforeFiniteSetRef '( bp, bq)
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb tra m) (MutablePinaforeRef tra' lv) =
    MkPinaforeFiniteSetRef trb $
    applyInversePinaforeLens pinaforeSubEntity m $
    eaMap (bijectionWholeEditLens (cfmap $ isoMapCat fromEnhanced $ bijectRanges tra' tra)) lv
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb (MkRange fa _) m) (ImmutablePinaforeRef fv) =
    MkPinaforeFiniteSetRef trb $
    applyInversePinaforeLens pinaforeSubEntity m $ immutableReferenceToRejectingValue $ fmap (fromEnhanced fa) fv

pinaforeApplyInverseMorphismImmutRef ::
       forall a bp bq. (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( bp, bq) '( a, TopType)
    -> PinaforeImmutableReference a
    -> PinaforeFiniteSetRef '( bp, bq)
pinaforeApplyInverseMorphismImmutRef m r = pinaforeApplyInverseMorphismRef m $ pinaforeImmutableToRef r

pinaforeApplyInverseMorphismSet ::
       forall ap aq bp bq. (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( bp, bq) '( JoinType NewEntity aq, ap)
    -> PinaforeFiniteSetRef '( ap, aq)
    -> PinaforeFiniteSetRef '( bp, bq)
pinaforeApplyInverseMorphismSet (MkPinaforeMorphism trb trpa m) (MkPinaforeFiniteSetRef tra' set) = let
    trp = contraMapRange join1 trpa
    tra = contraMapRange join2 trpa
    in MkPinaforeFiniteSetRef trb $
       applyInversePinaforeLensSet pinaforeSubEntity (fmap (fromEnhanced (rangeContra trp) . MkNewEntity) newEntity) m $
       eaMap (bijectionFiniteSetEditLens $ isoMapCat fromEnhanced $ bijectRanges tra' tra) set
