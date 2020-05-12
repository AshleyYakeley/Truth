module Pinafore.Language.Value.Morphism where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

newtype LangMorphism baseupdate (a :: (Type, Type)) (b :: (Type, Type)) =
    -- forall a b. (Eq a, Eq b) =>
    MkLangMorphism (PinaforeLensMorphism baseupdate (Contra a) (Co a) (Contra b) (Co b))

instance CatFunctor (CatRange (->)) (->) (LangMorphism edit a) where
    cfmap (MkCatRange pp qq) (MkLangMorphism m) = MkLangMorphism $ cfmap1 (MkCatDual pp) $ fmap qq m

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) (LangMorphism edit) where
    cfmap (MkCatRange pp qq) =
        MkNestedMorphism $ \(MkLangMorphism m) ->
            MkLangMorphism $
            (unNestedMorphism $ unNestedMorphism $ (unNestedMorphism $ cfmap (MkCatDual pp)) . cfmap qq) m

instance HasVariance 'Rangevariance (LangMorphism baseupdate) where
    varianceRepresentational = Nothing

instance HasVariance 'Rangevariance (LangMorphism baseupdate a) where
    varianceRepresentational = Nothing

langMorphismLens :: LangMorphism baseupdate '( ap, aq) '( bp, bq) -> PinaforeLensMorphism baseupdate ap aq bp bq
langMorphismLens (MkLangMorphism lm) = lm

pinaforeLensMorphism :: PinaforeLensMorphism baseupdate ap aq bp bq -> LangMorphism baseupdate '( ap, aq) '( bp, bq) {-(Eq a, Eq b) =>-}
pinaforeLensMorphism = MkLangMorphism

langMorphismFunction ::
       LangMorphism baseupdate '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
langMorphismFunction (MkLangMorphism pm) = lensFunctionMorphism pm

identityLangMorphism :: forall baseupdate x y. LangMorphism baseupdate '( x, y) '( y, x)
identityLangMorphism = MkLangMorphism identityPinaforeLensMorphism

composeLangMorphism ::
       forall baseupdate ap aq bx by cp cq.
       LangMorphism baseupdate '( bx, by) '( cp, cq)
    -> LangMorphism baseupdate '( ap, aq) '( by, bx)
    -> LangMorphism baseupdate '( ap, aq) '( cp, cq)
composeLangMorphism (MkLangMorphism m1) (MkLangMorphism m2) = MkLangMorphism $ composePinaforeLensMorphism m1 m2

pairLangMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       LangMorphism baseupdate '( ap, MeetType Entity aq) '( bp, bq)
    -> LangMorphism baseupdate '( ap, MeetType Entity aq) '( cp, cq)
    -> LangMorphism baseupdate '( ap, aq) '( (bp, cp), (bq, cq))
pairLangMorphism (MkLangMorphism m1) (MkLangMorphism m2) =
    cfmap1 (MkCatRange (id @(->)) meet2) $ MkLangMorphism $ pairPinaforeLensMorphism m1 m2

eitherLangMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       LangMorphism baseupdate '( ap, aq) '( cp, cq)
    -> LangMorphism baseupdate '( bp, bq) '( cp, cq)
    -> LangMorphism baseupdate '( Either ap bp, Either aq bq) '( cp, cq)
eitherLangMorphism (MkLangMorphism m1) (MkLangMorphism m2) = MkLangMorphism $ eitherPinaforeLensMorphism m1 m2

applyLangMorphismRef ::
       forall baseupdate ap aq bp bq.
       (?pinafore :: PinaforeContext baseupdate, BaseChangeLens PinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( ap, aq) '( bp, bq)
    -> LangRef '( aq, ap)
    -> LangRef '( bp, bq)
applyLangMorphismRef (MkLangMorphism m) ref =
    MutableLangRef $ applyPinaforeLens pinaforeBase m $ langRefToBiWholeRef ref

applyLangMorphismImmutRef ::
       forall baseupdate a bp bq.
       (?pinafore :: PinaforeContext baseupdate, BaseChangeLens PinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, TopType) '( bp, bq)
    -> PinaforeImmutableRef a
    -> LangRef '( bp, bq)
applyLangMorphismImmutRef m r = applyLangMorphismRef m $ pinaforeImmutableToRef r

applyLangMorphismSet ::
       forall baseupdate a b. (?pinafore :: PinaforeContext baseupdate, BaseChangeLens PinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, TopType) '( BottomType, MeetType Entity b)
    -> LangFiniteSetRef '( BottomType, a)
    -> LangFiniteSetRef '( MeetType Entity b, b)
applyLangMorphismSet lm (MkLangFiniteSetRef (tr :: Range _ t _) ss) = let
    tbkm :: PinaforeFunctionMorphism baseupdate (Know t) (Know (MeetType Entity b))
    tbkm = ccontramap1 (fmap $ fromEnhanced $ rangeCo tr) $ langMorphismFunction lm
    tbskm :: PinaforeFunctionMorphism baseupdate (FiniteSet (Know t)) (FiniteSet (Know (MeetType Entity b)))
    tbskm = cfmap tbkm
    tbsm :: PinaforeFunctionMorphism baseupdate (FiniteSet t) (FiniteSet (MeetType Entity b))
    tbsm = ccontramap1 (fmap Known) $ fmap (mapMaybe knowToMaybe) tbskm
    tsetref :: PinaforeROWRef (FiniteSet t)
    tsetref = eaToReadOnlyWhole ss
    bsetref :: PinaforeROWRef (FiniteSet (MeetType Entity b))
    bsetref = applyPinaforeFunction pinaforeBase tbsm tsetref
    in MkLangFiniteSetRef (MkRange id meet2) $ eaMap (convertChangeLens . fromReadOnlyRejectingChangeLens) bsetref

inverseApplyLangMorphismRef ::
       forall baseupdate a bx by.
       (?pinafore :: PinaforeContext baseupdate, BaseChangeLens PinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, MeetType Entity a) '( bx, by)
    -> LangRef '( by, bx)
    -> LangFiniteSetRef '( MeetType Entity a, a)
inverseApplyLangMorphismRef (MkLangMorphism m) ref =
    MkLangFiniteSetRef (MkRange id meet2) $
    applyInversePinaforeLens pinaforeBase (cfmap3 (MkCatDual $ meet2 @(->)) m) $ langRefToBiWholeRef ref

inverseApplyLangMorphismImmutRef ::
       forall baseupdate a b. (?pinafore :: PinaforeContext baseupdate, BaseChangeLens PinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, MeetType Entity a) '( b, TopType)
    -> PinaforeImmutableRef b
    -> LangFiniteSetRef '( MeetType Entity a, a)
inverseApplyLangMorphismImmutRef m r = inverseApplyLangMorphismRef m $ pinaforeImmutableToRef r

inverseApplyLangMorphismSet ::
       forall baseupdate a bx by.
       (?pinafore :: PinaforeContext baseupdate, BaseChangeLens PinaforeEntityUpdate baseupdate)
    => LangMorphism baseupdate '( a, MeetType Entity a) '( bx, by)
    -> LangFiniteSetRef '( JoinType NewEntity by, bx)
    -> LangFiniteSetRef '( MeetType Entity a, a)
inverseApplyLangMorphismSet (MkLangMorphism m) (MkLangFiniteSetRef (tra :: Range _ t _) seta) = let
    byt :: by -> t
    byt = fromEnhanced $ rangeContra tra . join2
    nt :: NewEntity -> t
    nt = fromEnhanced $ rangeContra tra . join1
    tbx :: t -> bx
    tbx = fromEnhanced $ rangeCo tra
    m' :: PinaforeLensMorphism baseupdate (MeetType Entity a) (MeetType Entity a) t t
    m' = cfmap3 (MkCatDual $ meet2 @(->)) $ cfmap1 (MkCatDual tbx) $ fmap byt m
    newVal :: IO t
    newVal = fmap (nt . MkNewEntity) newEntity
    setb :: PinaforeRef (FiniteSetUpdate (MeetType Entity a))
    setb = applyInversePinaforeLensSet pinaforeBase newVal m' seta
    in MkLangFiniteSetRef (MkRange id meet2) setb
