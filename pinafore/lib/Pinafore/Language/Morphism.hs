module Pinafore.Language.Morphism where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.OpenEntity
import Pinafore.Language.Reference
import Pinafore.Language.SetRef
import Shapes
import Truth.Core

data PinaforeMorphism baseedit pqa pqb =
    forall a b. (Eq a, Eq b) =>
                    MkPinaforeMorphism (Range a pqa)
                                       (Range b pqb)
                                       (PinaforeLensMorphism baseedit a b)

instance IsoMapRange (PinaforeMorphism baseedit pqa)

instance MapRange (PinaforeMorphism baseedit pqa) where
    mapRange f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism ra (mapRange f rb) m

instance IsoMapRange' (PinaforeMorphism baseedit)

instance MapRange' (PinaforeMorphism baseedit) where
    mapRange' f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism (mapRange f ra) rb m

instance HasDolanVary '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit) where
    dolanVary =
        ConsDolanVarianceMap
            (mkRangevary $ \mapr ->
                 MkNestedMorphism $ \(MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism (mapr ra) rb lm) $
        ConsDolanVarianceMap (mkRangevary $ \mapr (MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism ra (mapr rb) lm) $
        NilDolanVarianceMap

pinaforeMorphismLens :: PinaforeMorphism baseedit '( a, a) '( b, b) -> PinaforeLensMorphism baseedit a b
pinaforeMorphismLens (MkPinaforeMorphism tra trb lm) =
    (bijectionPinaforeLensMorphism $ rangeBijection trb) .
    lm . (bijectionPinaforeLensMorphism $ invert $ rangeBijection tra)

pinaforeLensMorphism :: (Eq a, Eq b) => PinaforeLensMorphism baseedit a b -> PinaforeMorphism baseedit '( a, a) '( b, b)
pinaforeLensMorphism = MkPinaforeMorphism identityRange identityRange

pinaforeMorphismFunction ::
       PinaforeMorphism baseedit '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism baseedit (Know a) (Know b)
pinaforeMorphismFunction (MkPinaforeMorphism tra trb pm) =
    proc ka -> do
        tb <- lensFunctionMorphism pm -< fmap (rangeContra tra) ka
        returnA -< fmap (rangeCo trb) tb

identityPinaforeMorphism ::
       forall baseedit t. PinaforeMorphism baseedit '( MeetType Entity t, t) '( MeetType Entity t, t)
identityPinaforeMorphism = coMapRange' meet2 $ coMapRange meet2 $ pinaforeLensMorphism id

composePinaforeMorphism ::
       forall baseedit ap aq bp bq cp cq.
       PinaforeMorphism baseedit '( bq, bp) '( cp, cq)
    -> PinaforeMorphism baseedit '( aq, ap) '( bp, bq)
    -> PinaforeMorphism baseedit '( aq, ap) '( cp, cq)
composePinaforeMorphism (MkPinaforeMorphism tb1 tc1 m1) (MkPinaforeMorphism ta2 tb2 m2) =
    MkPinaforeMorphism ta2 tc1 $ m1 . bijectionPinaforeLensMorphism (bijectRanges tb2 tb1) . m2

pinaforeApplyMorphismRef ::
       forall baseedit ap aq bp bq.
       PinaforeMorphism baseedit '( aq, ap) '( bp, bq)
    -> PinaforeReference baseedit '( ap, aq)
    -> PinaforeReference baseedit '( bp, bq)
pinaforeApplyMorphismRef (MkPinaforeMorphism tra trb m) (LensPinaforeReference tra' lv) =
    LensPinaforeReference trb $ applyPinaforeLens m $ bijectionWholeEditLens (cfmap $ bijectRanges tra' tra) . lv
pinaforeApplyMorphismRef (MkPinaforeMorphism (MkRange fa _) trb m) (ImmutPinaforeReference fv) =
    LensPinaforeReference trb $ applyPinaforeLens m $ immutableReferenceToLens $ fmap fa fv

pinaforeApplyMorphismSet ::
       forall baseedit a bp bq.
       PinaforeMorphism baseedit '( a, TopType) '( bp, bq)
    -> PinaforeSetRef baseedit '( BottomType, a)
    -> PinaforeSetRef baseedit '( bp, bq)
pinaforeApplyMorphismSet (MkPinaforeMorphism tra trb m) (MkPinaforeSetRef tra' ss) = let
    setm =
        proc st -> do
            skb <- cfmap $ lensFunctionMorphism m -< fmap (Known . rangeContra tra . rangeCo tra') st
            returnA -< mapMaybe knowToMaybe skb
    in MkPinaforeSetRef trb $ readOnlyEditLens $ convertEditFunction . applyPinaforeFunction setm (lensFunctionValue ss)

pinaforeApplyInverseMorphismRef ::
       forall baseedit ap aq bp bq.
       PinaforeMorphism baseedit '( bp, bq) '( aq, ap)
    -> PinaforeReference baseedit '( ap, aq)
    -> PinaforeSetRef baseedit '( bp, bq)
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb tra m) (LensPinaforeReference tra' lv) =
    MkPinaforeSetRef trb $ applyInversePinaforeLens m $ bijectionWholeEditLens (cfmap $ bijectRanges tra' tra) . lv
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb (MkRange fa _) m) (ImmutPinaforeReference fv) =
    MkPinaforeSetRef trb $ applyInversePinaforeLens m $ immutableReferenceToLens $ fmap fa fv

pinaforeApplyInverseMorphismSet ::
       forall baseedit ap aq bp bq.
       PinaforeMorphism baseedit '( bp, bq) '( JoinType NewEntity aq, ap)
    -> PinaforeSetRef baseedit '( ap, aq)
    -> PinaforeSetRef baseedit '( bp, bq)
pinaforeApplyInverseMorphismSet (MkPinaforeMorphism trb trpa m) (MkPinaforeSetRef tra' set) = let
    (trp, tra) = unjoinRange trpa
    in MkPinaforeSetRef trb $
       applyInversePinaforeLensSet (fmap (rangeContra trp . MkNewEntity) newEntity) m $
       (bijectionFiniteSetEditLens $ bijectRanges tra' tra) . set
