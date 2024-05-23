module Pinafore.Language.Value.Property where

import Import
import Pinafore.Language.Value.Attribute
import Pinafore.Language.Value.FiniteSetModel
import Pinafore.Language.Value.WholeModel

newtype LangProperty (a :: (Type, Type)) (b :: (Type, Type)) =
    MkLangProperty (ModelProperty (Contra a) (Co a) (Contra b) (Co b))

langPropertyAttribute :: LangProperty a b -> LangAttribute a b
langPropertyAttribute (MkLangProperty prop) = MkLangAttribute $ modelPropertyAttribute prop

combineLangPropertys ::
       (forall update.
                StorageLensProperty (Contra a1) (Co a1) (Contra b1) (Co b1) update -> StorageLensProperty (Contra a2) (Co a2) (Contra b2) (Co b2) update -> StorageLensProperty (Contra a12) (Co a12) (Contra b12) (Co b12) update)
    -> LangProperty a1 b1
    -> LangProperty a2 b2
    -> LangProperty a12 b12
combineLangPropertys combine (MkLangProperty mb1) (MkLangProperty mb2) =
    MkLangProperty $ combineModelBased combine mb1 mb2

instance CatFunctor (CatRange (->)) (->) (LangProperty a) where
    cfmap (MkCatRange pp qq) (MkLangProperty mb) =
        MkLangProperty $ mapModelBased (\m -> cfmap2 (MkCatDual pp) $ cfmap1 qq m) mb

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) LangProperty where
    cfmap (MkCatRange pp qq) =
        MkNestedMorphism $ \(MkLangProperty mb) ->
            MkLangProperty $ mapModelBased (\m -> cfmap4 (MkCatDual pp) $ cfmap3 qq m) mb

instance MaybeRepresentational LangProperty where
    maybeRepresentational = Nothing

instance MaybeRepresentational (LangProperty a) where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangProperty

instance HasCCRVariance 'RangeCCRVariance (LangProperty a)

identityLangProperty :: forall x y. LangProperty '( x, y) '( y, x)
identityLangProperty = MkLangProperty $ pureModelBased identityStorageLensProperty

composeLangProperty ::
       forall ap aq bx by cp cq.
       LangProperty '( bx, by) '( cp, cq)
    -> LangProperty '( ap, aq) '( by, bx)
    -> LangProperty '( ap, aq) '( cp, cq)
composeLangProperty = combineLangPropertys composeStorageLensProperty

pairLangProperty ::
       forall ap aq bp bq cp cq.
       LangProperty '( ap, MeetType Entity aq) '( bp, bq)
    -> LangProperty '( ap, MeetType Entity aq) '( cp, cq)
    -> LangProperty '( ap, aq) '( (bp, cp), (bq, cq))
pairLangProperty = combineLangPropertys $ \m1 m2 -> cfmap3 (meet2 @(->)) $ pairStorageLensProperty m1 m2

eitherLangProperty ::
       forall ap aq bp bq cp cq.
       LangProperty '( ap, aq) '( cp, cq)
    -> LangProperty '( bp, bq) '( cp, cq)
    -> LangProperty '( Either ap bp, Either aq bq) '( cp, cq)
eitherLangProperty = combineLangPropertys eitherStorageLensProperty

inverseApplyLangPropertyModel ::
       forall a bx by.
       LangProperty '( a, MeetType Entity a) '( bx, by)
    -> LangWholeModel '( by, bx)
    -> LangFiniteSetModel '( MeetType Entity a, a)
inverseApplyLangPropertyModel (MkLangProperty m) model =
    MkLangFiniteSetModel (MkRange id meet2) $
    applyInverseModelProperty (mapModelBased (cfmap4 (MkCatDual $ meet2 @(->))) m) $ langWholeModelToBiWholeModel model

inverseApplyLangPropertyImmutModel ::
       forall a b.
       LangProperty '( a, MeetType Entity a) '( b, TopType)
    -> ImmutableWholeModel b
    -> LangFiniteSetModel '( MeetType Entity a, a)
inverseApplyLangPropertyImmutModel m r = inverseApplyLangPropertyModel m $ immutableToWholeModel r

inverseApplyLangPropertySet ::
       forall a bx by.
       LangProperty '( a, MeetType Entity a) '( bx, by)
    -> LangFiniteSetModel '( by, bx)
    -> LangFiniteSetModel '( MeetType Entity a, a)
inverseApplyLangPropertySet (MkLangProperty m) (MkLangFiniteSetModel (tra :: Range _ t _) seta) = let
    byt :: by -> t
    byt = shimToFunction $ rangeContra tra
    tbx :: t -> bx
    tbx = shimToFunction $ rangeCo tra
    m' :: ModelProperty (MeetType Entity a) (MeetType Entity a) t t
    m' = mapModelBased (cfmap4 (MkCatDual $ meet2 @(->)) . cfmap2 (MkCatDual tbx) . cfmap1 byt) m
    setb :: WModel (FiniteSetUpdate (MeetType Entity a))
    setb = applyInverseModelPropertySet m' seta
    in MkLangFiniteSetModel (MkRange id meet2) setb
