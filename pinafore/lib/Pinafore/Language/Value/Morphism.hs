module Pinafore.Language.Value.Morphism where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data LangMorphism baseupdate (pqa :: (Type, Type)) (pqb :: (Type, Type)) =
    forall a b. (Eq a, Eq b) =>
                    MkLangMorphism (Range JMShim a pqa)
                                   (Range JMShim b pqb)
                                   (PinaforeLensMorphism baseupdate a b)

instance CatFunctor (CatRange (->)) (->) (LangMorphism edit a) where
    cfmap f (MkLangMorphism ra rb m) = MkLangMorphism ra (cfmap f rb) m

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) (LangMorphism edit) where
    cfmap f = MkNestedMorphism $ \(MkLangMorphism ra rb m) -> MkLangMorphism (cfmap f ra) rb m

instance HasVariance 'Rangevariance (LangMorphism baseupdate) where
    varianceRepresentational = Nothing

instance HasVariance 'Rangevariance (LangMorphism baseupdate a) where
    varianceRepresentational = Nothing

langMorphismLens :: LangMorphism baseupdate '( a, a) '( b, b) -> PinaforeLensMorphism baseupdate a b
langMorphismLens (MkLangMorphism tra trb lm) =
    (bijectionPinaforeLensMorphism $ isoMapCat fromEnhanced $ rangeBijection trb) .
    lm . (bijectionPinaforeLensMorphism $ invert $ isoMapCat fromEnhanced $ rangeBijection tra)

pinaforeLensMorphism :: (Eq a, Eq b) => PinaforeLensMorphism baseupdate a b -> LangMorphism baseupdate '( a, a) '( b, b)
pinaforeLensMorphism = MkLangMorphism identityRange identityRange

langMorphismFunction ::
       LangMorphism baseupdate '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
langMorphismFunction (MkLangMorphism tra trb pm) =
    proc ka -> do
        tb <- lensFunctionMorphism pm -< fmap (fromEnhanced $ rangeContra tra) ka
        returnA -< fmap (fromEnhanced $ rangeCo trb) tb

identityLangMorphism :: forall baseupdate t. LangMorphism baseupdate '( MeetType Entity t, t) '( MeetType Entity t, t)
identityLangMorphism = MkLangMorphism (coRange meet2) (coRange meet2) id

composeLangMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       LangMorphism baseupdate '( bq, bp) '( cp, cq)
    -> LangMorphism baseupdate '( aq, ap) '( bp, bq)
    -> LangMorphism baseupdate '( aq, ap) '( cp, cq)
composeLangMorphism (MkLangMorphism tb1 tc1 m1) (MkLangMorphism ta2 tb2 m2) =
    MkLangMorphism ta2 tc1 $ m1 . bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges tb2 tb1) . m2

pairLangMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       LangMorphism baseupdate '( ap, aq) '( bp, bq)
    -> LangMorphism baseupdate '( aq, ap) '( cp, cq)
    -> LangMorphism baseupdate '( ap, aq) '( (bp, cp), (bq, cq))
pairLangMorphism (MkLangMorphism ta1 tb1 m1) (MkLangMorphism ta2 tc2 m2) =
    MkLangMorphism ta1 (pairRange tb1 tc2) $
    pairPinaforeLensMorphism m1 $ m2 . bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges ta1 ta2)

eitherLangMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       LangMorphism baseupdate '( ap, aq) '( cp, cq)
    -> LangMorphism baseupdate '( bp, bq) '( cq, cp)
    -> LangMorphism baseupdate '( Either ap bp, Either aq bq) '( cp, cq)
eitherLangMorphism (MkLangMorphism ta1 tc1 m1) (MkLangMorphism tb2 tc2 m2) =
    MkLangMorphism (eitherRange ta1 tb2) tc1 $
    eitherPinaforeLensMorphism m1 $ bijectionPinaforeLensMorphism (isoMapCat fromEnhanced $ bijectRanges tc2 tc1) . m2

applyLangMorphismRef ::
       forall baseupdate ap aq bp bq. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( aq, ap) '( bp, bq)
    -> LangRef '( ap, aq)
    -> LangRef '( bp, bq)
applyLangMorphismRef (MkLangMorphism tra trb m) (MutableLangRef tra' lv) =
    MutableLangRef trb $
    applyPinaforeLens pinaforeBase m $
    eaMap (bijectionWholeChangeLens (cfmap $ isoMapCat fromEnhanced $ bijectRanges tra' tra)) lv
applyLangMorphismRef (MkLangMorphism (MkRange fa _) trb m) (ImmutableLangRef fv) =
    MutableLangRef trb $ applyPinaforeLens pinaforeBase m $ immutableRefToRejectingValue $ fmap (fromEnhanced fa) fv

applyLangMorphismImmutRef ::
       forall baseupdate a bp bq. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, TopType) '( bp, bq)
    -> PinaforeImmutableRef a
    -> LangRef '( bp, bq)
applyLangMorphismImmutRef m r = applyLangMorphismRef m $ pinaforeImmutableToRef r

applyLangMorphismSet ::
       forall baseupdate a bp bq. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, TopType) '( bp, bq)
    -> LangFiniteSetRef '( BottomType, a)
    -> LangFiniteSetRef '( bp, bq)
applyLangMorphismSet (MkLangMorphism tra trb m) (MkLangFiniteSetRef tra' ss) = let
    setm =
        proc st -> do
            skb <-
                cfmap $ lensFunctionMorphism m -<
                    fmap (Known . fromEnhanced (rangeContra tra) . fromEnhanced (rangeCo tra')) st
            returnA -< mapMaybe knowToMaybe skb
    in MkLangFiniteSetRef trb $
       eaMap (convertChangeLens . fromReadOnlyRejectingChangeLens) $
       applyPinaforeFunction pinaforeBase setm (eaToReadOnlyWhole ss)

inverseApplyLangMorphismRef ::
       forall baseupdate ap aq bp bq. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( bp, bq) '( aq, ap)
    -> LangRef '( ap, aq)
    -> LangFiniteSetRef '( bp, bq)
inverseApplyLangMorphismRef (MkLangMorphism trb tra m) (MutableLangRef tra' lv) =
    MkLangFiniteSetRef trb $
    applyInversePinaforeLens pinaforeBase m $
    eaMap (bijectionWholeChangeLens (cfmap $ isoMapCat fromEnhanced $ bijectRanges tra' tra)) lv
inverseApplyLangMorphismRef (MkLangMorphism trb (MkRange fa _) m) (ImmutableLangRef fv) =
    MkLangFiniteSetRef trb $
    applyInversePinaforeLens pinaforeBase m $ immutableRefToRejectingValue $ fmap (fromEnhanced fa) fv

inverseApplyLangMorphismImmutRef ::
       forall baseupdate a bp bq. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( bp, bq) '( a, TopType)
    -> PinaforeImmutableRef a
    -> LangFiniteSetRef '( bp, bq)
inverseApplyLangMorphismImmutRef m r = inverseApplyLangMorphismRef m $ pinaforeImmutableToRef r

inverseApplyLangMorphismSet ::
       forall baseupdate ap aq bp bq. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( bp, bq) '( JoinType NewEntity aq, ap)
    -> LangFiniteSetRef '( ap, aq)
    -> LangFiniteSetRef '( bp, bq)
inverseApplyLangMorphismSet (MkLangMorphism trb trpa m) (MkLangFiniteSetRef tra' set) = let
    trp = contraMapRange join1 trpa
    tra = contraMapRange join2 trpa
    in MkLangFiniteSetRef trb $
       applyInversePinaforeLensSet pinaforeBase (fmap (fromEnhanced (rangeContra trp) . MkNewEntity) newEntity) m $
       eaMap (bijectionFiniteSetChangeLens $ isoMapCat fromEnhanced $ bijectRanges tra' tra) set
