module Pinafore.Language.Value.WholeRef where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Instances ()
import Shapes

data LangWholeRef (pq :: (Type, Type)) where
    MutableLangWholeRef :: WModel (BiWholeUpdate (Know p) (Know q)) -> LangWholeRef '( p, q)
    ImmutableLangWholeRef :: PinaforeImmutableWholeRef q -> LangWholeRef '( p, q)

instance CatFunctor (CatRange (->)) (->) LangWholeRef where
    cfmap (MkCatRange pp qq) (MutableLangWholeRef v) =
        MutableLangWholeRef $ eaMap (mapBiWholeChangeLens (fmap pp) (fmap qq)) v
    cfmap (MkCatRange _ f) (ImmutableLangWholeRef v) = ImmutableLangWholeRef $ fmap f v

instance HasVariance 'Rangevariance LangWholeRef where
    varianceRepresentational = Nothing

langWholeRefToReadOnlyValue :: LangWholeRef '( BottomType, a) -> PinaforeROWRef (Know a)
langWholeRefToReadOnlyValue ref =
    case langWholeRefToImmutable ref of
        MkPinaforeImmutableWholeRef fv -> fv

pinaforeROWRefToWholeRef :: PinaforeROWRef (Know a) -> LangWholeRef '( TopType, a)
pinaforeROWRefToWholeRef ef = pinaforeImmutableToWholeRef $ MkPinaforeImmutableWholeRef ef

langWholeRefToImmutable' :: LangWholeRef '( ap, aq) -> PinaforeImmutableWholeRef aq
langWholeRefToImmutable' (MutableLangWholeRef sr) = MkPinaforeImmutableWholeRef $ eaMap biReadOnlyChangeLens sr
langWholeRefToImmutable' (ImmutableLangWholeRef ir) = ir

langWholeRefToImmutable :: LangWholeRef '( BottomType, a) -> PinaforeImmutableWholeRef a
langWholeRefToImmutable = langWholeRefToImmutable'

pinaforeImmutableToWholeRef :: PinaforeImmutableWholeRef a -> LangWholeRef '( TopType, a)
pinaforeImmutableToWholeRef ir = ImmutableLangWholeRef ir

langWholeRefToValue :: LangWholeRef '( p, p) -> WModel (WholeUpdate (Know p))
langWholeRefToValue (MutableLangWholeRef lv) = eaMap biSingleChangeLens lv
langWholeRefToValue (ImmutableLangWholeRef ir) = immutableRefToRejectingRef ir

pinaforeRefToWholeRef :: WModel (WholeUpdate (Know a)) -> LangWholeRef '( a, a)
pinaforeRefToWholeRef bsv = MutableLangWholeRef $ eaMap singleBiChangeLens bsv

langWholeRefGet :: forall q. LangWholeRef '( BottomType, q) -> PinaforeAction q
langWholeRefGet ref = do
    ka <- getImmutableRef $ langWholeRefToImmutable ref
    pinaforeActionKnow ka

langWholeRefSet :: forall p. LangWholeRef '( p, TopType) -> Know p -> PinaforeAction ()
langWholeRefSet (MutableLangWholeRef sr) mp = pinaforeRefPushAction sr $ pure $ MkBiWholeEdit mp
langWholeRefSet (ImmutableLangWholeRef _) _ = empty

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

langMaybeWholeRef :: forall p q. LangWholeRef '( p, q) -> LangWholeRef '( Maybe p, Maybe q)
langMaybeWholeRef = maybeLensLangWholeRef Just $ \mmp _ -> mmp

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
langPairWholeRefs (langWholeRefToImmutable' -> aref) (langWholeRefToImmutable' -> bref) =
    ImmutableLangWholeRef $ liftA2 (,) aref bref
