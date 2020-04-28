module Pinafore.Language.Value.Ref where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Instances ()
import Shapes
import Truth.Core

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

langRefToImmutable :: LangRef '( BottomType, a) -> PinaforeImmutableRef a
langRefToImmutable (MutableLangRef sr) = MkPinaforeImmutableRef $ eaMap biReadOnlyChangeLens sr
langRefToImmutable (ImmutableLangRef ir) = ir

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

fLensLangRef :: forall ap aq b. (aq -> b) -> (b -> Maybe aq -> Maybe ap) -> LangRef '( ap, aq) -> LangRef '( b, b)
fLensLangRef g pb (MutableLangRef lv) = let
    lensPB :: Know b -> Know aq -> Maybe (Know ap)
    lensPB kb ka =
        getComposeM $ do
            b <- liftInner kb
            liftOuter $ pb b $ knowToMaybe ka
    in MutableLangRef $ eaMap (lensBiWholeChangeLens (fmap g) lensPB) lv
fLensLangRef g _ (ImmutableLangRef ir) = ImmutableLangRef $ fmap g ir

langRefToBiWholeRef :: LangRef '( p, q) -> PinaforeRef (BiWholeUpdate (Know p) (Know q))
langRefToBiWholeRef (MutableLangRef r) = r
langRefToBiWholeRef (ImmutableLangRef ir) = immutableRefToRejectingBiRef ir

langRefToEntityRef :: LangRef '( a, MeetType Entity a) -> PinaforeRef (WholeUpdate (Know (MeetType Entity a)))
langRefToEntityRef ref = eaMap (biSingleChangeLens . mapBiWholeChangeLens (fmap meet2) id) $ langRefToBiWholeRef ref
