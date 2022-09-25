module Pinafore.Language.Value.Morphism where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Value.FiniteSetModel
import Pinafore.Language.Value.WholeModel
import Shapes

newtype LangMorphism (a :: (Type, Type)) (b :: (Type, Type)) =
    MkLangMorphism (ModelMorphism (Contra a) (Co a) (Contra b) (Co b))

combineLangMorphisms ::
       (forall update.
                PinaforeLensMorphism (Contra a1) (Co a1) (Contra b1) (Co b1) update -> PinaforeLensMorphism (Contra a2) (Co a2) (Contra b2) (Co b2) update -> PinaforeLensMorphism (Contra a12) (Co a12) (Contra b12) (Co b12) update)
    -> LangMorphism a1 b1
    -> LangMorphism a2 b2
    -> LangMorphism a12 b12
combineLangMorphisms combine (MkLangMorphism mb1) (MkLangMorphism mb2) =
    MkLangMorphism $ combineModelBased combine mb1 mb2

instance CatFunctor (CatRange (->)) (->) (LangMorphism a) where
    cfmap (MkCatRange pp qq) (MkLangMorphism mb) =
        MkLangMorphism $ mapModelBased (\m -> cfmap2 (MkCatDual pp) $ cfmap1 qq m) mb

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) LangMorphism where
    cfmap (MkCatRange pp qq) =
        MkNestedMorphism $ \(MkLangMorphism mb) ->
            MkLangMorphism $ mapModelBased (\m -> cfmap4 (MkCatDual pp) $ cfmap3 qq m) mb

instance MaybeRepresentational LangMorphism where
    maybeRepresentational = Nothing

instance MaybeRepresentational (LangMorphism a) where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangMorphism

instance HasCCRVariance 'RangeCCRVariance (LangMorphism a)

identityLangMorphism :: forall x y. LangMorphism '( x, y) '( y, x)
identityLangMorphism = MkLangMorphism $ pureModelBased identityPinaforeLensMorphism

composeLangMorphism ::
       forall ap aq bx by cp cq. (?pinafore :: PinaforeContext)
    => LangMorphism '( bx, by) '( cp, cq)
    -> LangMorphism '( ap, aq) '( by, bx)
    -> LangMorphism '( ap, aq) '( cp, cq)
composeLangMorphism = combineLangMorphisms composePinaforeLensMorphism

pairLangMorphism ::
       forall ap aq bp bq cp cq. (?pinafore :: PinaforeContext)
    => LangMorphism '( ap, MeetType Entity aq) '( bp, bq)
    -> LangMorphism '( ap, MeetType Entity aq) '( cp, cq)
    -> LangMorphism '( ap, aq) '( (bp, cp), (bq, cq))
pairLangMorphism = combineLangMorphisms $ \m1 m2 -> cfmap3 (meet2 @(->)) $ pairPinaforeLensMorphism m1 m2

eitherLangMorphism ::
       forall ap aq bp bq cp cq. (?pinafore :: PinaforeContext)
    => LangMorphism '( ap, aq) '( cp, cq)
    -> LangMorphism '( bp, bq) '( cp, cq)
    -> LangMorphism '( Either ap bp, Either aq bq) '( cp, cq)
eitherLangMorphism = combineLangMorphisms eitherPinaforeLensMorphism

applyLangMorphismModel ::
       forall ap aq bp bq. (?pinafore :: PinaforeContext)
    => LangMorphism '( ap, aq) '( bp, bq)
    -> LangWholeModel '( aq, ap)
    -> LangWholeModel '( bp, bq)
applyLangMorphismModel (MkLangMorphism m) model =
    MutableLangWholeModel $ applyModelMorphism m $ langWholeModelToBiWholeModel model

applyLangMorphismImmutModel ::
       forall a bp bq. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, TopType) '( bp, bq)
    -> PinaforeImmutableWholeModel a
    -> LangWholeModel '( bp, bq)
applyLangMorphismImmutModel m r = applyLangMorphismModel m $ pinaforeImmutableToWholeModel r

applyLangMorphismSet ::
       forall a b. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, TopType) '( BottomType, MeetType Entity b)
    -> LangFiniteSetModel '( BottomType, a)
    -> LangFiniteSetModel '( MeetType Entity b, b)
applyLangMorphismSet (MkLangMorphism m) (MkLangFiniteSetModel (tr :: Range _ t _) ss) =
    modelBasedModel m $ \model (pm :: _ update) -> let
        tbkm :: PinaforeFunctionMorphism update (Know t) (Know (MeetType Entity b))
        tbkm = ccontramap1 (fmap $ shimToFunction $ rangeCo tr) $ lensFunctionMorphism pm
        tbskm :: PinaforeFunctionMorphism update (FiniteSet (Know t)) (FiniteSet (Know (MeetType Entity b)))
        tbskm = cfmap tbkm
        tbsm :: PinaforeFunctionMorphism update (FiniteSet t) (FiniteSet (MeetType Entity b))
        tbsm = ccontramap1 (fmap Known) $ fmap (mapMaybe knowToMaybe) tbskm
        tsetref :: PinaforeROWModel (FiniteSet t)
        tsetref = eaToReadOnlyWhole ss
        bsetref :: PinaforeROWModel (FiniteSet (MeetType Entity b))
        bsetref = applyPinaforeFunction model tbsm tsetref
        in MkLangFiniteSetModel (MkRange id meet2) $ eaMap (convertChangeLens . fromReadOnlyRejectingChangeLens) bsetref

inverseApplyLangMorphismModel ::
       forall a bx by. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, MeetType Entity a) '( bx, by)
    -> LangWholeModel '( by, bx)
    -> LangFiniteSetModel '( MeetType Entity a, a)
inverseApplyLangMorphismModel (MkLangMorphism m) model =
    MkLangFiniteSetModel (MkRange id meet2) $
    applyInverseModelMorphism (mapModelBased (cfmap4 (MkCatDual $ meet2 @(->))) m) $ langWholeModelToBiWholeModel model

inverseApplyLangMorphismImmutModel ::
       forall a b. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, MeetType Entity a) '( b, TopType)
    -> PinaforeImmutableWholeModel b
    -> LangFiniteSetModel '( MeetType Entity a, a)
inverseApplyLangMorphismImmutModel m r = inverseApplyLangMorphismModel m $ pinaforeImmutableToWholeModel r

inverseApplyLangMorphismSet ::
       forall a bx by. (?pinafore :: PinaforeContext)
    => LangMorphism '( a, MeetType Entity a) '( bx, by)
    -> LangFiniteSetModel '( by, bx)
    -> LangFiniteSetModel '( MeetType Entity a, a)
inverseApplyLangMorphismSet (MkLangMorphism m) (MkLangFiniteSetModel (tra :: Range _ t _) seta) = let
    byt :: by -> t
    byt = shimToFunction $ rangeContra tra
    tbx :: t -> bx
    tbx = shimToFunction $ rangeCo tra
    m' :: ModelMorphism (MeetType Entity a) (MeetType Entity a) t t
    m' = mapModelBased (cfmap4 (MkCatDual $ meet2 @(->)) . cfmap2 (MkCatDual tbx) . cfmap1 byt) m
    setb :: WModel (FiniteSetUpdate (MeetType Entity a))
    setb = applyInverseModelMorphismSet m' seta
    in MkLangFiniteSetModel (MkRange id meet2) setb
