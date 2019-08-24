module Pinafore.Language.Value.Morphism where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Pinafore.Language.Value.SetRef
import Shapes
import Truth.Core

data PinaforeMorphism baseedit (pqa :: (Type, Type)) (pqb :: (Type, Type)) =
    forall a b. (Eq a, Eq b) =>
                    MkPinaforeMorphism (Range JMShim a pqa)
                                       (Range JMShim b pqb)
                                       (PinaforeLensMorphism baseedit a b)

instance CatFunctor (CatRange (->)) (->) (PinaforeMorphism edit a) where
    cfmap f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism ra (cfmap f rb) m

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) (PinaforeMorphism edit) where
    cfmap f = MkNestedMorphism $ \(MkPinaforeMorphism ra rb m) -> MkPinaforeMorphism (cfmap f ra) rb m

instance HasVariance 'Rangevariance (PinaforeMorphism baseedit) where
    varianceRepresentational = Nothing

instance HasVariance 'Rangevariance (PinaforeMorphism baseedit a) where
    varianceRepresentational = Nothing

pinaforeMorphismLens :: PinaforeMorphism baseedit '( a, a) '( b, b) -> PinaforeLensMorphism baseedit a b
pinaforeMorphismLens (MkPinaforeMorphism tra trb lm) =
    (bijectionPinaforeLensMorphism $ isoMapCat fromEnhanced $ rangeBijection trb) .
    lm . (bijectionPinaforeLensMorphism $ invert $ isoMapCat fromEnhanced $ rangeBijection tra)

pinaforeLensMorphism :: (Eq a, Eq b) => PinaforeLensMorphism baseedit a b -> PinaforeMorphism baseedit '( a, a) '( b, b)
pinaforeLensMorphism = MkPinaforeMorphism identityRange identityRange

pinaforeMorphismFunction ::
       PinaforeMorphism baseedit '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism baseedit (Know a) (Know b)
pinaforeMorphismFunction (MkPinaforeMorphism tra trb pm) =
    proc ka -> do
        tb <- lensFunctionMorphism pm -< fmap (fromEnhanced $ rangeContra tra) ka
        returnA -< fmap (fromEnhanced $ rangeCo trb) tb

identityPinaforeMorphism ::
       forall baseedit t. PinaforeMorphism baseedit '( MeetType Entity t, t) '( MeetType Entity t, t)
identityPinaforeMorphism = MkPinaforeMorphism (coRange meet2) (coRange meet2) id

composePinaforeMorphism ::
       forall baseedit ap aq bp bq cp cq.
       PinaforeMorphism baseedit '( bq, bp) '( cp, cq)
    -> PinaforeMorphism baseedit '( aq, ap) '( bp, bq)
    -> PinaforeMorphism baseedit '( aq, ap) '( cp, cq)
composePinaforeMorphism (MkPinaforeMorphism tb1 tc1 m1) (MkPinaforeMorphism ta2 tb2 m2) =
    MkPinaforeMorphism ta2 tc1 $ m1 . bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges tb2 tb1) . m2

pairPinaforeMorphism ::
       forall baseedit ap aq bp bq cp cq.
       PinaforeMorphism baseedit '( ap, aq) '( bp, bq)
    -> PinaforeMorphism baseedit '( aq, ap) '( cp, cq)
    -> PinaforeMorphism baseedit '( ap, aq) '( (bp, cp), (bq, cq))
pairPinaforeMorphism (MkPinaforeMorphism ta1 tb1 m1) (MkPinaforeMorphism ta2 tc2 m2) =
    MkPinaforeMorphism ta1 (pairRange tb1 tc2) $
    pairPinaforeLensMorphism m1 $ m2 . bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges ta1 ta2)

eitherPinaforeMorphism ::
       forall baseedit ap aq bp bq cp cq.
       PinaforeMorphism baseedit '( ap, aq) '( cp, cq)
    -> PinaforeMorphism baseedit '( bp, bq) '( cq, cp)
    -> PinaforeMorphism baseedit '( Either ap bp, Either aq bq) '( cp, cq)
eitherPinaforeMorphism (MkPinaforeMorphism ta1 tc1 m1) (MkPinaforeMorphism tb2 tc2 m2) =
    MkPinaforeMorphism (eitherRange ta1 tb2) tc1 $
    eitherPinaforeLensMorphism m1 $ bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges tc2 tc1) . m2

pinaforeApplyMorphismRef ::
       forall baseedit ap aq bp bq.
       PinaforeMorphism baseedit '( aq, ap) '( bp, bq)
    -> PinaforeRef baseedit '( ap, aq)
    -> PinaforeRef baseedit '( bp, bq)
pinaforeApplyMorphismRef (MkPinaforeMorphism tra trb m) (LensPinaforeRef tra' lv) =
    LensPinaforeRef trb $
    applyPinaforeLens m $ bijectionWholeEditLens (cfmap $ isoMapCat fromEnhanced $ bijectRanges tra' tra) . lv
pinaforeApplyMorphismRef (MkPinaforeMorphism (MkRange fa _) trb m) (ImmutPinaforeRef fv) =
    LensPinaforeRef trb $ applyPinaforeLens m $ immutableReferenceToLens $ fmap (fromEnhanced fa) fv

pinaforeApplyMorphismSet ::
       forall baseedit a bp bq.
       PinaforeMorphism baseedit '( a, TopType) '( bp, bq)
    -> PinaforeSetRef baseedit '( BottomType, a)
    -> PinaforeSetRef baseedit '( bp, bq)
pinaforeApplyMorphismSet (MkPinaforeMorphism tra trb m) (MkPinaforeSetRef tra' ss) = let
    setm =
        proc st -> do
            skb <-
                cfmap $ lensFunctionMorphism m -<
                    fmap (Known . fromEnhanced (rangeContra tra) . fromEnhanced (rangeCo tra')) st
            returnA -< mapMaybe knowToMaybe skb
    in MkPinaforeSetRef trb $ readOnlyEditLens $ convertEditFunction . applyPinaforeFunction setm (lensFunctionValue ss)

pinaforeApplyInverseMorphismRef ::
       forall baseedit ap aq bp bq.
       PinaforeMorphism baseedit '( bp, bq) '( aq, ap)
    -> PinaforeRef baseedit '( ap, aq)
    -> PinaforeSetRef baseedit '( bp, bq)
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb tra m) (LensPinaforeRef tra' lv) =
    MkPinaforeSetRef trb $
    applyInversePinaforeLens m $ bijectionWholeEditLens (cfmap $ isoMapCat fromEnhanced $ bijectRanges tra' tra) . lv
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb (MkRange fa _) m) (ImmutPinaforeRef fv) =
    MkPinaforeSetRef trb $ applyInversePinaforeLens m $ immutableReferenceToLens $ fmap (fromEnhanced fa) fv

pinaforeApplyInverseMorphismSet ::
       forall baseedit ap aq bp bq.
       PinaforeMorphism baseedit '( bp, bq) '( JoinType NewEntity aq, ap)
    -> PinaforeSetRef baseedit '( ap, aq)
    -> PinaforeSetRef baseedit '( bp, bq)
pinaforeApplyInverseMorphismSet (MkPinaforeMorphism trb trpa m) (MkPinaforeSetRef tra' set) = let
    trp = contraMapRange join1 trpa
    tra = contraMapRange join2 trpa
    in MkPinaforeSetRef trb $
       applyInversePinaforeLensSet (fmap (fromEnhanced (rangeContra trp) . MkNewEntity) newEntity) m $
       (bijectionFiniteSetEditLens $ isoMapCat fromEnhanced $ bijectRanges tra' tra) . set
