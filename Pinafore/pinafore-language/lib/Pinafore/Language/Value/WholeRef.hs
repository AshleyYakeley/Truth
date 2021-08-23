module Pinafore.Language.Value.WholeRef where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Value.Instances ()
import Shapes

data LangWholeRef (pq :: (Type, Type)) where
    MutableLangWholeRef :: WModel (BiWholeUpdate (Know p) (Know q)) -> LangWholeRef '( p, q)
    ImmutableLangWholeRef :: PinaforeImmutableWholeRef q -> LangWholeRef '( p, q)

instance CatFunctor (CatRange (->)) (->) LangWholeRef where
    cfmap (MkCatRange pp qq) (MutableLangWholeRef v) =
        MutableLangWholeRef $ eaMap (mapBiWholeChangeLens (fmap pp) (fmap qq)) v
    cfmap (MkCatRange _ f) (ImmutableLangWholeRef v) = ImmutableLangWholeRef $ fmap f v

instance MaybeRepresentational LangWholeRef where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangWholeRef

langWholeRefToReadOnlyValue :: LangWholeRef '( BottomType, a) -> PinaforeROWRef (Know a)
langWholeRefToReadOnlyValue ref =
    case langWholeRefToImmutable ref of
        MkPinaforeImmutableWholeRef fv -> fv

pinaforeROWRefToWholeRef :: PinaforeROWRef (Know a) -> LangWholeRef '( TopType, a)
pinaforeROWRefToWholeRef ef = pinaforeImmutableToWholeRef $ MkPinaforeImmutableWholeRef ef

langWholeRefToImmutable :: forall p q. LangWholeRef '( p, q) -> PinaforeImmutableWholeRef q
langWholeRefToImmutable (MutableLangWholeRef sr) = MkPinaforeImmutableWholeRef $ eaMap biReadOnlyChangeLens sr
langWholeRefToImmutable (ImmutableLangWholeRef ir) = ir

pinaforeImmutableToWholeRef :: PinaforeImmutableWholeRef a -> LangWholeRef '( TopType, a)
pinaforeImmutableToWholeRef ir = ImmutableLangWholeRef ir

langWholeRefToValue :: LangWholeRef '( p, p) -> WModel (WholeUpdate (Know p))
langWholeRefToValue (MutableLangWholeRef lv) = eaMap biSingleChangeLens lv
langWholeRefToValue (ImmutableLangWholeRef ir) = immutableRefToRejectingRef ir

pinaforeRefToWholeRef :: WModel (WholeUpdate (Know a)) -> LangWholeRef '( a, a)
pinaforeRefToWholeRef bsv = MutableLangWholeRef $ eaMap singleBiChangeLens bsv

langWholeRefGet :: forall p q. LangWholeRef '( p, q) -> PinaforeAction q
langWholeRefGet ref = do
    ka <- getImmutableRef $ langWholeRefToImmutable ref
    pinaforeActionKnow ka

langWholeRefSet :: forall p q. LangWholeRef '( p, q) -> Know p -> PinaforeAction ()
langWholeRefSet (MutableLangWholeRef sr) mp = pinaforeRefPush sr $ pure $ MkBiWholeEdit mp
langWholeRefSet (ImmutableLangWholeRef _) _ = empty

langWholeRefMapModel ::
       Functor f => (forall update. WModel update -> f (WModel update)) -> LangWholeRef pq -> f (LangWholeRef pq)
langWholeRefMapModel ff (MutableLangWholeRef model) = fmap MutableLangWholeRef $ ff model
langWholeRefMapModel ff (ImmutableLangWholeRef (MkPinaforeImmutableWholeRef model)) =
    fmap (ImmutableLangWholeRef . MkPinaforeImmutableWholeRef) $ ff model

langWholeRefSubscribe ::
       forall a. (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef a
    -> (Maybe a -> PinaforeAction ())
    -> PinaforeAction ()
langWholeRefSubscribe (MkPinaforeImmutableWholeRef (MkWModel ref)) update =
    createViewPinaforeAction $ cvBindReadOnlyWholeModel ref (\ka -> runPinaforeAction $ update $ knowToMaybe ka)

maybeLensLangWholeRef ::
       forall ap aq bp bq.
       (Maybe aq -> Maybe bq)
    -> (Maybe bp -> Maybe aq -> Maybe (Maybe ap))
    -> LangWholeRef '( ap, aq)
    -> LangWholeRef '( bp, bq)
maybeLensLangWholeRef g pb (MutableLangWholeRef lv) = let
    lensG :: Know aq -> Know bq
    lensG = maybeToKnow . g . knowToMaybe
    lensPB :: Know bp -> Know aq -> Maybe (Know ap)
    lensPB kb ka = fmap maybeToKnow $ pb (knowToMaybe kb) $ knowToMaybe ka
    in MutableLangWholeRef $ eaMap (lensBiWholeChangeLens lensG lensPB) lv
maybeLensLangWholeRef g _ (ImmutableLangWholeRef (MkPinaforeImmutableWholeRef ir)) = let
    lensG :: Know aq -> Know bq
    lensG = maybeToKnow . g . knowToMaybe
    in ImmutableLangWholeRef $ MkPinaforeImmutableWholeRef $ eaMapReadOnlyWhole lensG ir

fLensLangWholeRef ::
       forall ap aq b. (aq -> b) -> (b -> Maybe aq -> Maybe ap) -> LangWholeRef '( ap, aq) -> LangWholeRef '( b, b)
fLensLangWholeRef ab baa =
    maybeLensLangWholeRef (fmap ab) $ \mb ma -> do
        b <- mb
        return $ baa b ma

langToMaybeWholeRef :: forall p q. LangWholeRef '( p, q) -> LangWholeRef '( Maybe p, Maybe q)
langToMaybeWholeRef = maybeLensLangWholeRef Just $ \mmp _ -> mmp

langFromMaybeWholeRef :: forall p q. LangWholeRef '( Maybe p, Maybe q) -> LangWholeRef '( p, q)
langFromMaybeWholeRef =
    maybeLensLangWholeRef exec $ \mp mq ->
        Just $ do
            _ <- mq
            Just mp

langWholeRefToBiWholeRef :: LangWholeRef '( p, q) -> WModel (BiWholeUpdate (Know p) (Know q))
langWholeRefToBiWholeRef (MutableLangWholeRef r) = r
langWholeRefToBiWholeRef (ImmutableLangWholeRef ir) = immutableRefToRejectingBiRef ir

langWholeRefToEntityRef :: LangWholeRef '( a, MeetType Entity a) -> WModel (WholeUpdate (Know (MeetType Entity a)))
langWholeRefToEntityRef ref =
    eaMap (biSingleChangeLens . mapBiWholeChangeLens (fmap meet2) id) $ langWholeRefToBiWholeRef ref

langPairWholeRefs ::
       forall ap aq bp bq. LangWholeRef '( ap, aq) -> LangWholeRef '( bp, bq) -> LangWholeRef '( (ap, bp), (aq, bq))
langPairWholeRefs (MutableLangWholeRef aref) (MutableLangWholeRef bref) = let
    lget :: (Know aq, Know bq) -> Know (aq, bq)
    lget (ka, kb) = do
        a <- ka
        b <- kb
        return (a, b)
    lputback :: Know (ap, bp) -> (Know aq, Know bq) -> Maybe (Know ap, Know bp)
    lputback Unknown _ = Just (Unknown, Unknown)
    lputback (Known (a, b)) _ = Just (Known a, Known b)
    in MutableLangWholeRef $ eaMap (lensBiWholeChangeLens lget lputback . pairBiWholeChangeLens) $ eaPair aref bref
langPairWholeRefs (langWholeRefToImmutable -> aref) (langWholeRefToImmutable -> bref) =
    ImmutableLangWholeRef $ liftA2 (,) aref bref

modelSelectNotify :: EditSource -> Model (BiWholeUpdate (Know p) q) -> SelectNotify p
modelSelectNotify esrc model =
    MkSelectNotify $ \vma -> do
        ma <- vma
        viewRunResource model $ \asub -> do
            _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkBiEdit $ MkWholeReaderEdit $ maybeToKnow ma
            return ()

langWholeRefSelectNotify :: EditSource -> LangWholeRef '( p, q) -> SelectNotify p
langWholeRefSelectNotify esrc (MutableLangWholeRef (MkWModel model)) = modelSelectNotify esrc model
langWholeRefSelectNotify _ (ImmutableLangWholeRef _) = mempty
