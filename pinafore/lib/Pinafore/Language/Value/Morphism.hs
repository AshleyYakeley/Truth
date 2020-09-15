module Pinafore.Language.Value.Morphism where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.WholeRef
import Shapes

newtype LangMorphism (a :: (Type, Type)) (b :: (Type, Type)) =
    -- forall a b. (Eq a, Eq b) =>
    MkLangMorphism (PinaforeLensMorphism PinaforeStorageUpdate (Contra a) (Co a) (Contra b) (Co b))

instance CatFunctor (CatRange (->)) (->) (LangMorphism a) where
    cfmap (MkCatRange pp qq) (MkLangMorphism m) = MkLangMorphism $ cfmap1 (MkCatDual pp) $ fmap qq m

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) LangMorphism where
    cfmap (MkCatRange pp qq) =
        MkNestedMorphism $ \(MkLangMorphism m) ->
            MkLangMorphism $
            (unNestedMorphism $ unNestedMorphism $ (unNestedMorphism $ cfmap (MkCatDual pp)) . cfmap qq) m

instance HasVariance 'Rangevariance LangMorphism where
    varianceRepresentational = Nothing

instance HasVariance 'Rangevariance (LangMorphism a) where
    varianceRepresentational = Nothing

langMorphismLens :: LangMorphism '( ap, aq) '( bp, bq) -> PinaforeLensMorphism PinaforeStorageUpdate ap aq bp bq
langMorphismLens (MkLangMorphism lm) = lm

pinaforeLensMorphism :: PinaforeLensMorphism PinaforeStorageUpdate ap aq bp bq -> LangMorphism '( ap, aq) '( bp, bq) {-(Eq a, Eq b) =>-}
pinaforeLensMorphism = MkLangMorphism

langMorphismFunction ::
       LangMorphism '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) (Know b)
langMorphismFunction (MkLangMorphism pm) = lensFunctionMorphism pm

identityLangMorphism :: forall x y. LangMorphism '( x, y) '( y, x)
identityLangMorphism = MkLangMorphism identityPinaforeLensMorphism

composeLangMorphism ::
       forall ap aq bx by cp cq.
       LangMorphism '( bx, by) '( cp, cq)
    -> LangMorphism '( ap, aq) '( by, bx)
    -> LangMorphism '( ap, aq) '( cp, cq)
composeLangMorphism (MkLangMorphism m1) (MkLangMorphism m2) = MkLangMorphism $ composePinaforeLensMorphism m1 m2

pairLangMorphism ::
       forall ap aq bp bq cp cq.
       LangMorphism '( ap, MeetType Entity aq) '( bp, bq)
    -> LangMorphism '( ap, MeetType Entity aq) '( cp, cq)
    -> LangMorphism '( ap, aq) '( (bp, cp), (bq, cq))
pairLangMorphism (MkLangMorphism m1) (MkLangMorphism m2) =
    cfmap1 (MkCatRange (id @(->)) meet2) $ MkLangMorphism $ pairPinaforeLensMorphism m1 m2

eitherLangMorphism ::
       forall ap aq bp bq cp cq.
       LangMorphism '( ap, aq) '( cp, cq)
    -> LangMorphism '( bp, bq) '( cp, cq)
    -> LangMorphism '( Either ap bp, Either aq bq) '( cp, cq)
eitherLangMorphism (MkLangMorphism m1) (MkLangMorphism m2) = MkLangMorphism $ eitherPinaforeLensMorphism m1 m2

applyLangMorphismRef ::
       forall ap aq bp bq. (?pinafore :: PinaforeContext)
    => LangMorphism '( ap, aq) '( bp, bq)
    -> LangWholeRef '( aq, ap)
    -> LangWholeRef '( bp, bq)
applyLangMorphismRef (MkLangMorphism m) ref =
    MutableLangWholeRef $ applyPinaforeLens pinaforeEntityModel m $ langWholeRefToBiWholeRef ref

applyLangMorphismImmutRef ::
       forall a bp bq. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, TopType) '( bp, bq)
    -> PinaforeImmutableWholeRef a
    -> LangWholeRef '( bp, bq)
applyLangMorphismImmutRef m r = applyLangMorphismRef m $ pinaforeImmutableToWholeRef r

applyLangMorphismSet ::
       forall a b. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, TopType) '( BottomType, MeetType Entity b)
    -> LangFiniteSetRef '( BottomType, a)
    -> LangFiniteSetRef '( MeetType Entity b, b)
applyLangMorphismSet lm (MkLangFiniteSetRef (tr :: Range _ t _) ss) = let
    tbkm :: PinaforeFunctionMorphism PinaforeStorageUpdate (Know t) (Know (MeetType Entity b))
    tbkm = ccontramap1 (fmap $ shimToFunction $ rangeCo tr) $ langMorphismFunction lm
    tbskm :: PinaforeFunctionMorphism PinaforeStorageUpdate (FiniteSet (Know t)) (FiniteSet (Know (MeetType Entity b)))
    tbskm = cfmap tbkm
    tbsm :: PinaforeFunctionMorphism PinaforeStorageUpdate (FiniteSet t) (FiniteSet (MeetType Entity b))
    tbsm = ccontramap1 (fmap Known) $ fmap (mapMaybe knowToMaybe) tbskm
    tsetref :: PinaforeROWRef (FiniteSet t)
    tsetref = eaToReadOnlyWhole ss
    bsetref :: PinaforeROWRef (FiniteSet (MeetType Entity b))
    bsetref = applyPinaforeFunction pinaforeEntityModel tbsm tsetref
    in MkLangFiniteSetRef (MkRange id meet2) $ eaMap (convertChangeLens . fromReadOnlyRejectingChangeLens) bsetref

inverseApplyLangMorphismRef ::
       forall a bx by. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, MeetType Entity a) '( bx, by)
    -> LangWholeRef '( by, bx)
    -> LangFiniteSetRef '( MeetType Entity a, a)
inverseApplyLangMorphismRef (MkLangMorphism m) ref =
    MkLangFiniteSetRef (MkRange id meet2) $
    applyInversePinaforeLens pinaforeEntityModel (cfmap3 (MkCatDual $ meet2 @(->)) m) $ langWholeRefToBiWholeRef ref

inverseApplyLangMorphismImmutRef ::
       forall a b. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, MeetType Entity a) '( b, TopType)
    -> PinaforeImmutableWholeRef b
    -> LangFiniteSetRef '( MeetType Entity a, a)
inverseApplyLangMorphismImmutRef m r = inverseApplyLangMorphismRef m $ pinaforeImmutableToWholeRef r

inverseApplyLangMorphismSet ::
       forall a bx by. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, MeetType Entity a) '( bx, by)
    -> LangFiniteSetRef '( by, bx)
    -> LangFiniteSetRef '( MeetType Entity a, a)
inverseApplyLangMorphismSet (MkLangMorphism m) (MkLangFiniteSetRef (tra :: Range _ t _) seta) = let
    byt :: by -> t
    byt = shimToFunction $ rangeContra tra
    tbx :: t -> bx
    tbx = shimToFunction $ rangeCo tra
    m' :: PinaforeLensMorphism PinaforeStorageUpdate (MeetType Entity a) (MeetType Entity a) t t
    m' = cfmap3 (MkCatDual $ meet2 @(->)) $ cfmap1 (MkCatDual tbx) $ fmap byt m
    setb :: PinaforeRef (FiniteSetUpdate (MeetType Entity a))
    setb = applyInversePinaforeLensSet pinaforeEntityModel m' seta
    in MkLangFiniteSetRef (MkRange id meet2) setb
