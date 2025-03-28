module Pinafore.Language.Value.Attribute where

import Import
import Pinafore.Language.Value.FiniteSetModel
import Pinafore.Language.Value.Lens
import Pinafore.Language.Value.Prism
import Pinafore.Language.Value.WholeModel

newtype LangAttribute (a :: (Type, Type)) (b :: (Type, Type))
    = MkLangAttribute (ModelAttribute (Contra a) (Co a) (Contra b) (Co b))

combineLangAttributes ::
    ( forall update.
      StorageLensAttribute (Contra a1) (Co a1) (Contra b1) (Co b1) update -> StorageLensAttribute (Contra a2) (Co a2) (Contra b2) (Co b2) update -> StorageLensAttribute (Contra a12) (Co a12) (Contra b12) (Co b12) update
    ) ->
    LangAttribute a1 b1 ->
    LangAttribute a2 b2 ->
    LangAttribute a12 b12
combineLangAttributes combine (MkLangAttribute mb1) (MkLangAttribute mb2) =
    MkLangAttribute $ combineModelBased combine mb1 mb2

instance CatFunctor (CatRange (->)) (->) (LangAttribute a) where
    cfmap (MkCatRange pp qq) (MkLangAttribute mb) =
        MkLangAttribute $ mapModelBased (\m -> cfmap2 (MkCatDual pp) $ cfmap1 qq m) mb

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) LangAttribute where
    cfmap (MkCatRange pp qq) =
        MkNestedMorphism $ \(MkLangAttribute mb) ->
            MkLangAttribute $ mapModelBased (\m -> cfmap4 (MkCatDual pp) $ cfmap3 qq m) mb

instance MaybeRepresentational LangAttribute where
    maybeRepresentational = Nothing

instance MaybeRepresentational (LangAttribute a) where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangAttribute

instance HasCCRVariance 'RangeCCRVariance (LangAttribute a)

identityLangAttribute :: forall x y. LangAttribute '(x, y) '(y, x)
identityLangAttribute = MkLangAttribute $ pureModelBased identityStorageLensAttribute

composeLangAttribute ::
    forall ap aq bx by cp cq.
    LangAttribute '(bx, by) '(cp, cq) ->
    LangAttribute '(ap, aq) '(by, bx) ->
    LangAttribute '(ap, aq) '(cp, cq)
composeLangAttribute = combineLangAttributes composeStorageLensAttribute

pairLangAttribute ::
    forall ap aq bp bq cp cq.
    LangAttribute '(ap, aq) '(bp, bq) ->
    LangAttribute '(ap, aq) '(cp, cq) ->
    LangAttribute '(ap, aq) '((bp, cp), (bq, cq))
pairLangAttribute = combineLangAttributes pairStorageLensAttribute

eitherLangAttribute ::
    forall ap aq bp bq cp cq.
    LangAttribute '(ap, aq) '(cp, cq) ->
    LangAttribute '(bp, bq) '(cp, cq) ->
    LangAttribute '(Either ap bp, Either aq bq) '(cp, cq)
eitherLangAttribute = combineLangAttributes eitherStorageLensAttribute

applyLangAttributeModel ::
    forall ap aq bp bq. LangAttribute '(ap, aq) '(bp, bq) -> LangWholeModel '(aq, ap) -> LangWholeModel '(bp, bq)
applyLangAttributeModel (MkLangAttribute m) model =
    MutableLangWholeModel $ applyModelAttribute m $ langWholeModelToBiWholeModel model

applyLangAttributeImmutModel ::
    forall a bp bq. LangAttribute '(a, TopType) '(bp, bq) -> ImmutableWholeModel a -> LangWholeModel '(bp, bq)
applyLangAttributeImmutModel m r = applyLangAttributeModel m $ immutableToWholeModel r

applyLangAttributeSet ::
    forall a b.
    LangAttribute '(a, TopType) '(BottomType, MeetType Entity b) ->
    LangFiniteSetModel '(BottomType, a) ->
    LangFiniteSetModel '(MeetType Entity b, b)
applyLangAttributeSet (MkLangAttribute m) (MkLangFiniteSetModel eqv (tr :: Range _ t _) ss) =
    modelBasedModel m $ \model (pm :: _ update) -> let
        tbkm :: StorageFunctionAttribute update t (Know (MeetType Entity b))
        tbkm = ccontramap1 (Known . shimToFunction (rangeCo tr)) $ lensFunctionAttribute pm
        tbskm :: StorageFunctionAttribute update (ListSet t) (ListMap t (Know (MeetType Entity b)))
        tbskm = liftListSetStorageFunctionAttribute tbkm
        tbsm :: StorageFunctionAttribute update (ListSet t) (ListSet (MeetType Entity b))
        tbsm = fmap (setFromList . mapMaybe knowToMaybe . toList) tbskm
        tsetref :: WROWModel (ListSet t)
        tsetref = giveConstraint eqv $ eaToReadOnlyWhole ss
        bsetref :: WROWModel (ListSet (MeetType Entity b))
        bsetref = applyStorageFunction model tbsm tsetref
        in MkLangFiniteSetModel eqEquivalence (MkRange id meet2)
            $ eaMap (convertChangeLens . fromReadOnlyRejectingChangeLens) bsetref

langLensAttribute :: forall ap aq bp bq. LangLens '(ap, aq) '(bp, bq) -> LangAttribute '(ap, aq) '(bp, bq)
langLensAttribute (MkLangLens g pb) =
    MkLangAttribute $ pureModelBased $ funcStorageLensAttribute (Known . g) $ \ka kb -> Just $ liftA2 pb ka kb

langPrismAttribute :: forall ap aq bp bq. LangPrism '(bp, bq) '(ap, aq) -> LangAttribute '(ap, aq) '(bp, bq)
langPrismAttribute (MkLangPrism d e) =
    MkLangAttribute
        $ pureModelBased
        $ funcStorageLensAttribute (Known . e)
        $ \_ ka ->
            case ka of
                Known a ->
                    case d a of
                        Right b -> Just $ Known b
                        Left _ -> Nothing
                Unknown -> Just Unknown

langPrismReverseAttribute :: forall ap aq bp bq. LangPrism '(ap, aq) '(bp, bq) -> LangAttribute '(ap, aq) '(bp, bq)
langPrismReverseAttribute (MkLangPrism d e) =
    MkLangAttribute $ pureModelBased $ funcStorageLensAttribute (maybeToKnow . mToMaybe . d) (\_ -> Just . fmap e)
