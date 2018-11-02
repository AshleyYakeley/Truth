module Pinafore.Language.Morphism where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Shapes
import Truth.Core

data PinaforeMorphism baseedit pqa pqb =
    forall a b. (Eq a, Eq b) =>
                    MkPinaforeMorphism (TypeRange a pqa)
                                       (TypeRange b pqb)
                                       (PinaforeLensMorphism baseedit a b)

instance IsoMapTypeRange (PinaforeMorphism baseedit pqa)

instance MapTypeRange (PinaforeMorphism baseedit pqa) where
    mapTypeRange f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism ra (mapTypeRange f rb) m

instance IsoMapTypeRange' (PinaforeMorphism baseedit)

instance MapTypeRange' (PinaforeMorphism baseedit) where
    mapTypeRange' f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism (mapTypeRange f ra) rb m

instance HasDolanVary '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit) where
    dolanVary =
        ConsDolanKindVary
            (mkRangevary $ \mapr ->
                 MkNestedMorphism $ \(MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism (mapr ra) rb lm) $
        ConsDolanKindVary (mkRangevary $ \mapr (MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism ra (mapr rb) lm) $
        NilDolanKindVary

pinaforeMorphismLens :: PinaforeMorphism baseedit '( a, a) '( b, b) -> PinaforeLensMorphism baseedit a b
pinaforeMorphismLens (MkPinaforeMorphism tra trb lm) =
    (bijectionPinaforeLensMorphism $ typeRangeBijection trb) .
    lm . (bijectionPinaforeLensMorphism $ invert $ typeRangeBijection tra)

pinaforeLensMorphism :: (Eq a, Eq b) => PinaforeLensMorphism baseedit a b -> PinaforeMorphism baseedit '( a, a) '( b, b)
pinaforeLensMorphism = MkPinaforeMorphism identityTypeRange identityTypeRange

pinaforeMorphismFunction ::
       PinaforeMorphism baseedit '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism baseedit (Know a) (Know b)
pinaforeMorphismFunction (MkPinaforeMorphism tra trb pm) =
    proc ka -> do
        tb <- lensFunctionMorphism pm -< fmap (typeRangeContra tra) ka
        returnA -< fmap (typeRangeCo trb) tb

identityPinaforeMorphism ::
       forall baseedit t. PinaforeMorphism baseedit '( MeetType Entity t, t) '( MeetType Entity t, t)
identityPinaforeMorphism = coMapTypeRange' meet2 $ coMapTypeRange meet2 $ pinaforeLensMorphism id

composePinaforeMorphism ::
       forall baseedit ap aq bp bq cp cq.
       PinaforeMorphism baseedit '( bq, bp) '( cp, cq)
    -> PinaforeMorphism baseedit '( aq, ap) '( bp, bq)
    -> PinaforeMorphism baseedit '( aq, ap) '( cp, cq)
composePinaforeMorphism (MkPinaforeMorphism tb1 tc1 m1) (MkPinaforeMorphism ta2 tb2 m2) =
    MkPinaforeMorphism ta2 tc1 $ m1 . bijectionPinaforeLensMorphism (bijectTypeRanges tb2 tb1) . m2

pinaforeApplyMorphismRef ::
       forall baseedit pa qa pb qb.
       PinaforeMorphism baseedit '( qa, pa) '( pb, qb)
    -> PinaforeReference baseedit '( pa, qa)
    -> PinaforeReference baseedit '( pb, qb)
pinaforeApplyMorphismRef (MkPinaforeMorphism tra trb m) (MkPinaforeReference tra' lv) =
    MkPinaforeReference trb $ applyPinaforeLens m $ bijectionWholeEditLens (cfmap $ bijectTypeRanges tra' tra) . lv

pinaforeApplyMorphismSet ::
       forall baseedit a b.
       PinaforeMorphism baseedit '( a, TopType) '( BottomType, b)
    -> PinaforeSet baseedit '( BottomType, a)
    -> PinaforeSet baseedit '( BottomType, b)
pinaforeApplyMorphismSet (MkPinaforeMorphism tra trb m) (MkPinaforeSet tra' ss) = let
    setm =
        proc st -> do
            skb <- cfmap $ lensFunctionMorphism m -< fmap (Known . typeRangeContra tra . typeRangeCo tra') st
            returnA -< mapMaybe knowToMaybe skb
    in MkPinaforeSet (coTypeRange $ typeRangeCo trb) $
       readOnlyEditLens $ convertEditFunction . applyPinaforeFunction setm (lensFunctionValue ss)

pinaforeApplyInverseMorphismRef ::
       forall baseedit pa qa pb qb.
       PinaforeMorphism baseedit '( pb, qb) '( qa, pa)
    -> PinaforeReference baseedit '( pa, qa)
    -> PinaforeSet baseedit '( pb, qb)
pinaforeApplyInverseMorphismRef (MkPinaforeMorphism trb tra m) (MkPinaforeReference tra' lv) =
    MkPinaforeSet trb $ applyInversePinaforeLens m $ bijectionWholeEditLens (cfmap $ bijectTypeRanges tra' tra) . lv

pinaforeApplyInverseMorphismSet ::
       forall baseedit pa qa pb qb.
       PinaforeMorphism baseedit '( pb, qb) '( JoinType Point qa, pa)
    -> PinaforeSet baseedit '( pa, qa)
    -> PinaforeSet baseedit '( pb, qb)
pinaforeApplyInverseMorphismSet (MkPinaforeMorphism trb trpa m) (MkPinaforeSet tra' set) = let
    (trp, tra) = unjoinTypeRange trpa
    in MkPinaforeSet trb $
       applyInversePinaforeLensSet (fmap (typeRangeContra trp) newPoint) m $
       (bijectionFiniteSetEditLens $ bijectTypeRanges tra' tra) . set
