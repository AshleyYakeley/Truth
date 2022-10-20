module Pinafore.Language.Value.WholeModel where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Value.Instances ()
import Pinafore.Language.Value.Model
import Shapes

data LangWholeModel (pq :: (Type, Type)) where
    MutableLangWholeModel :: WModel (BiWholeUpdate (Know p) (Know q)) -> LangWholeModel '( p, q)
    ImmutableLangWholeModel :: ImmutableWholeModel q -> LangWholeModel '( p, q)

instance CatFunctor (CatRange (->)) (->) LangWholeModel where
    cfmap (MkCatRange pp qq) (MutableLangWholeModel v) =
        MutableLangWholeModel $ eaMap (mapBiWholeChangeLens (fmap pp) (fmap qq)) v
    cfmap (MkCatRange _ f) (ImmutableLangWholeModel v) = ImmutableLangWholeModel $ fmap f v

instance MaybeRepresentational LangWholeModel where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangWholeModel

newMemWholeModel :: forall a. Action (LangWholeModel '( a, a))
newMemWholeModel = do
    r <- liftIO $ makeMemoryReference Unknown $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel r
    uh <- actionUndoHandler
    return $ wModelToWholeModel $ MkWModel $ undoHandlerModel uh model

langWholeModelToModel :: forall p q. LangWholeModel '( p, q) -> LangModel
langWholeModelToModel (MutableLangWholeModel model) = MkLangModel model
langWholeModelToModel (ImmutableLangWholeModel model) = MkLangModel $ immutableModelToReadOnlyModel model

langWholeModelToReadOnlyValue :: LangWholeModel '( BottomType, a) -> WROWModel (Know a)
langWholeModelToReadOnlyValue model =
    case langWholeModelToImmutable model of
        MkImmutableWholeModel fv -> fv

wROWModelToWholeModel :: WROWModel (Know a) -> LangWholeModel '( TopType, a)
wROWModelToWholeModel ef = immutableToWholeModel $ MkImmutableWholeModel ef

langWholeModelToImmutable :: forall p q. LangWholeModel '( p, q) -> ImmutableWholeModel q
langWholeModelToImmutable (MutableLangWholeModel sr) = MkImmutableWholeModel $ eaMap biReadOnlyChangeLens sr
langWholeModelToImmutable (ImmutableLangWholeModel ir) = ir

immutableToWholeModel :: ImmutableWholeModel a -> LangWholeModel '( TopType, a)
immutableToWholeModel ir = ImmutableLangWholeModel ir

langWholeModelToValue :: LangWholeModel '( p, p) -> WModel (WholeUpdate (Know p))
langWholeModelToValue (MutableLangWholeModel lv) = eaMap biSingleChangeLens lv
langWholeModelToValue (ImmutableLangWholeModel ir) = immutableModelToRejectingModel ir

wModelToWholeModel :: WModel (WholeUpdate (Know a)) -> LangWholeModel '( a, a)
wModelToWholeModel bsv = MutableLangWholeModel $ eaMap singleBiChangeLens bsv

langWholeModelGet :: forall p q. LangWholeModel '( p, q) -> Action q
langWholeModelGet model = do
    ka <- getImmutableModel $ langWholeModelToImmutable model
    actionKnow ka

langWholeModelSet :: forall p q. LangWholeModel '( p, q) -> Know p -> Action ()
langWholeModelSet (MutableLangWholeModel sr) mp = actionModelPush sr $ pure $ MkBiWholeEdit mp
langWholeModelSet (ImmutableLangWholeModel _) _ = empty

langWholeModelMapModel ::
       Functor f => (forall update. WModel update -> f (WModel update)) -> LangWholeModel pq -> f (LangWholeModel pq)
langWholeModelMapModel ff (MutableLangWholeModel model) = fmap MutableLangWholeModel $ ff model
langWholeModelMapModel ff (ImmutableLangWholeModel (MkImmutableWholeModel model)) =
    fmap (ImmutableLangWholeModel . MkImmutableWholeModel) $ ff model

langWholeModelSubscribe ::
       forall a. (?qcontext :: QContext)
    => ImmutableWholeModel a
    -> (Maybe a -> Action ())
    -> Action ()
langWholeModelSubscribe (MkImmutableWholeModel (MkWModel model)) update =
    actionLiftView $ viewBindReadOnlyWholeModel model $ \_ ka -> runAction $ update $ knowToMaybe ka

maybeLensLangWholeModel ::
       forall ap aq bp bq.
       (Maybe aq -> Maybe bq)
    -> (Maybe bp -> Maybe aq -> Maybe (Maybe ap))
    -> LangWholeModel '( ap, aq)
    -> LangWholeModel '( bp, bq)
maybeLensLangWholeModel g pb (MutableLangWholeModel lv) = let
    lensG :: Know aq -> Know bq
    lensG = maybeToKnow . g . knowToMaybe
    lensPB :: Know bp -> Know aq -> Maybe (Know ap)
    lensPB kb ka = fmap maybeToKnow $ pb (knowToMaybe kb) $ knowToMaybe ka
    in MutableLangWholeModel $ eaMap (lensBiWholeChangeLens lensG lensPB) lv
maybeLensLangWholeModel g _ (ImmutableLangWholeModel (MkImmutableWholeModel ir)) = let
    lensG :: Know aq -> Know bq
    lensG = maybeToKnow . g . knowToMaybe
    in ImmutableLangWholeModel $ MkImmutableWholeModel $ eaMapReadOnlyWhole lensG ir

fLensLangWholeModel ::
       forall ap aq b. (aq -> b) -> (b -> Maybe aq -> Maybe ap) -> LangWholeModel '( ap, aq) -> LangWholeModel '( b, b)
fLensLangWholeModel ab baa =
    maybeLensLangWholeModel (fmap ab) $ \mb ma -> do
        b <- mb
        return $ baa b ma

langToMaybeWholeModel :: forall p q. LangWholeModel '( p, q) -> LangWholeModel '( Maybe p, Maybe q)
langToMaybeWholeModel = maybeLensLangWholeModel Just $ \mmp _ -> mmp

langFromMaybeWholeModel :: forall p q. LangWholeModel '( Maybe p, Maybe q) -> LangWholeModel '( p, q)
langFromMaybeWholeModel =
    maybeLensLangWholeModel exec $ \mp mq ->
        Just $ do
            _ <- mq
            Just mp

langWholeModelToBiWholeModel :: LangWholeModel '( p, q) -> WModel (BiWholeUpdate (Know p) (Know q))
langWholeModelToBiWholeModel (MutableLangWholeModel r) = r
langWholeModelToBiWholeModel (ImmutableLangWholeModel ir) = immutableModelToRejectingBiModel ir

langWholeModelToEntityRef :: LangWholeModel '( a, MeetType Entity a) -> WModel (WholeUpdate (Know (MeetType Entity a)))
langWholeModelToEntityRef model =
    eaMap (biSingleChangeLens . mapBiWholeChangeLens (fmap meet2) id) $ langWholeModelToBiWholeModel model

langPairWholeModels ::
       forall ap aq bp bq.
       LangWholeModel '( ap, aq)
    -> LangWholeModel '( bp, bq)
    -> LangWholeModel '( (ap, bp), (aq, bq))
langPairWholeModels (MutableLangWholeModel aref) (MutableLangWholeModel bref) = let
    lget :: (Know aq, Know bq) -> Know (aq, bq)
    lget (ka, kb) = do
        a <- ka
        b <- kb
        return (a, b)
    lputback :: Know (ap, bp) -> (Know aq, Know bq) -> Maybe (Know ap, Know bp)
    lputback Unknown _ = Just (Unknown, Unknown)
    lputback (Known (a, b)) _ = Just (Known a, Known b)
    in MutableLangWholeModel $ eaMap (lensBiWholeChangeLens lget lputback . pairBiWholeChangeLens) $ eaPair aref bref
langPairWholeModels (langWholeModelToImmutable -> aref) (langWholeModelToImmutable -> bref) =
    ImmutableLangWholeModel $ liftA2 (,) aref bref

modelSelectNotify :: EditSource -> Model (BiWholeUpdate (Know p) q) -> SelectNotify p
modelSelectNotify esrc model =
    MkSelectNotify $ \vma -> do
        ma <- vma
        viewRunResource model $ \asub -> do
            _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkBiEdit $ MkWholeReaderEdit $ maybeToKnow ma
            return ()

langWholeModelSelectNotify :: EditSource -> LangWholeModel '( p, q) -> SelectNotify p
langWholeModelSelectNotify esrc (MutableLangWholeModel (MkWModel model)) = modelSelectNotify esrc model
langWholeModelSelectNotify _ (ImmutableLangWholeModel _) = mempty
