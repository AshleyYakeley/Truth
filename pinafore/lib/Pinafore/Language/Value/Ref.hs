module Pinafore.Language.Value.Ref where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Instances ()
import Shapes

data LangRef (pq :: (Type, Type)) where
    MutableLangRef :: PinaforeRef (BiWholeUpdate (Know p) (Know q)) -> LangRef '( p, q)
    ImmutableLangRef :: PinaforeImmutableRef q -> LangRef '( p, q)

instance CatFunctor (CatRange (->)) (->) LangRef where
    cfmap (MkCatRange pp qq) (MutableLangRef v) = MutableLangRef $ eaMap (mapBiWholeChangeLens (fmap pp) (fmap qq)) v
    cfmap (MkCatRange _ f) (ImmutableLangRef v) = ImmutableLangRef $ fmap f v

instance HasVariance 'Rangevariance LangRef where
    varianceRepresentational = Nothing

langRefToReadOnlyValue :: LangRef '( BottomType, a) -> PinaforeROWRef (Know a)
langRefToReadOnlyValue ref =
    case langRefToImmutable ref of
        MkPinaforeImmutableRef fv -> fv

pinaforeROWRefToRef :: PinaforeROWRef (Know a) -> LangRef '( TopType, a)
pinaforeROWRefToRef ef = pinaforeImmutableToRef $ MkPinaforeImmutableRef ef

langRefToImmutable' :: LangRef '( ap, aq) -> PinaforeImmutableRef aq
langRefToImmutable' (MutableLangRef sr) = MkPinaforeImmutableRef $ eaMap biReadOnlyChangeLens sr
langRefToImmutable' (ImmutableLangRef ir) = ir

langRefToImmutable :: LangRef '( BottomType, a) -> PinaforeImmutableRef a
langRefToImmutable = langRefToImmutable'

pinaforeImmutableToRef :: PinaforeImmutableRef a -> LangRef '( TopType, a)
pinaforeImmutableToRef ir = ImmutableLangRef ir

langRefToValue :: LangRef '( p, p) -> PinaforeRef (WholeUpdate (Know p))
langRefToValue (MutableLangRef lv) = eaMap biSingleChangeLens lv
langRefToValue (ImmutableLangRef ir) = immutableRefToRejectingRef ir

pinaforeRefToRef :: PinaforeRef (WholeUpdate (Know a)) -> LangRef '( a, a)
pinaforeRefToRef bsv = MutableLangRef $ eaMap singleBiChangeLens bsv

langRefGet :: forall q. LangRef '( BottomType, q) -> PinaforeAction q
langRefGet ref = do
    ka <- getImmutableRef $ langRefToImmutable ref
    pinaforeActionKnow ka

langRefSet :: forall p. LangRef '( p, TopType) -> Know p -> PinaforeAction ()
langRefSet (MutableLangRef sr) mp = pinaforeRefPushAction sr $ pure $ MkBiWholeEdit mp
langRefSet (ImmutableLangRef _) _ = empty

runLangRef :: LangRef '( BottomType, PinaforeAction ()) -> PinaforeAction ()
runLangRef ref = langRefGet ref >>= id

maybeLensLangRef ::
       forall ap aq bp bq.
       (Maybe aq -> Maybe bq)
    -> (Maybe bp -> Maybe aq -> Maybe (Maybe ap))
    -> LangRef '( ap, aq)
    -> LangRef '( bp, bq)
maybeLensLangRef g pb (MutableLangRef lv) = let
    lensG :: Know aq -> Know bq
    lensG = maybeToKnow . g . knowToMaybe
    lensPB :: Know bp -> Know aq -> Maybe (Know ap)
    lensPB kb ka = fmap maybeToKnow $ pb (knowToMaybe kb) $ knowToMaybe ka
    in MutableLangRef $ eaMap (lensBiWholeChangeLens lensG lensPB) lv
maybeLensLangRef g _ (ImmutableLangRef (MkPinaforeImmutableRef ir)) = let
    lensG :: Know aq -> Know bq
    lensG = maybeToKnow . g . knowToMaybe
    in ImmutableLangRef $ MkPinaforeImmutableRef $ eaMapReadOnlyWhole lensG ir

fLensLangRef :: forall ap aq b. (aq -> b) -> (b -> Maybe aq -> Maybe ap) -> LangRef '( ap, aq) -> LangRef '( b, b)
fLensLangRef ab baa =
    maybeLensLangRef (fmap ab) $ \mb ma -> do
        b <- mb
        return $ baa b ma

maybeRef :: forall p q. LangRef '( p, q) -> LangRef '( Maybe p, Maybe q)
maybeRef = maybeLensLangRef Just $ \mmp _ -> mmp

langRefToBiWholeRef :: LangRef '( p, q) -> PinaforeRef (BiWholeUpdate (Know p) (Know q))
langRefToBiWholeRef (MutableLangRef r) = r
langRefToBiWholeRef (ImmutableLangRef ir) = immutableRefToRejectingBiRef ir

langRefToEntityRef :: LangRef '( a, MeetType Entity a) -> PinaforeRef (WholeUpdate (Know (MeetType Entity a)))
langRefToEntityRef ref = eaMap (biSingleChangeLens . mapBiWholeChangeLens (fmap meet2) id) $ langRefToBiWholeRef ref

langPairRefs :: forall ap aq bp bq. LangRef '( ap, aq) -> LangRef '( bp, bq) -> LangRef '( (ap, bp), (aq, bq))
langPairRefs (MutableLangRef aref) (MutableLangRef bref) = let
    lget :: (Know aq, Know bq) -> Know (aq, bq)
    lget (ka, kb) = do
        a <- ka
        b <- kb
        return (a, b)
    lputback :: Know (ap, bp) -> (Know aq, Know bq) -> Maybe (Know ap, Know bp)
    lputback Unknown _ = Just (Unknown, Unknown)
    lputback (Known (a, b)) _ = Just (Known a, Known b)
    in MutableLangRef $ eaMap (lensBiWholeChangeLens lget lputback . pairBiWholeChangeLens) $ eaPair aref bref
langPairRefs (langRefToImmutable' -> aref) (langRefToImmutable' -> bref) = ImmutableLangRef $ liftA2 (,) aref bref
