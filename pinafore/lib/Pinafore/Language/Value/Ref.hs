module Pinafore.Language.Value.Ref where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Instances ()
import Shapes
import Truth.Core

data LangRef (pq :: (Type, Type)) where
    MutableLangRef :: Range JMShim t pq -> PinaforeValue (WholeUpdate (Know t)) -> LangRef pq
    ImmutableLangRef :: PinaforeImmutableReference q -> LangRef '( p, q)

instance CatFunctor (CatRange (->)) (->) LangRef where
    cfmap f (MutableLangRef r v) = MutableLangRef (cfmap f r) v
    cfmap (MkCatRange _ f) (ImmutableLangRef v) = ImmutableLangRef $ fmap f v

instance HasVariance 'Rangevariance LangRef where
    varianceRepresentational = Nothing

langRefToReadOnlyValue :: LangRef '( BottomType, a) -> PinaforeReadOnlyValue (Know a)
langRefToReadOnlyValue ref =
    case langRefToImmutable ref of
        MkPinaforeImmutableReference fv -> fv

pinaforeReadOnlyValueToRef :: PinaforeReadOnlyValue (Know a) -> LangRef '( TopType, a)
pinaforeReadOnlyValueToRef ef = pinaforeImmutableToRef $ MkPinaforeImmutableReference ef

langRefToImmutable :: LangRef '( BottomType, a) -> PinaforeImmutableReference a
langRefToImmutable (MutableLangRef (MkRange _ tq) sr) =
    fmap (fromEnhanced tq) $ MkPinaforeImmutableReference $ eaToReadOnlyWhole sr
langRefToImmutable (ImmutableLangRef ir) = ir

pinaforeImmutableToRef :: PinaforeImmutableReference a -> LangRef '( TopType, a)
pinaforeImmutableToRef ir = ImmutableLangRef ir

langRefToValue :: LangRef '( p, p) -> PinaforeValue (WholeUpdate (Know p))
langRefToValue (MutableLangRef tr lv) =
    eaMap (bijectionWholeChangeLens $ isoMapCat (fromEnhanced @_ @JMShim) $ cfmap $ rangeBijection tr) lv
langRefToValue (ImmutableLangRef ir) = immutableReferenceToRejectingValue ir

pinaforeValueToRef :: (PinaforeValue (WholeUpdate (Know a))) -> LangRef '( a, a)
pinaforeValueToRef bsv = MutableLangRef identityRange bsv

langRefGet :: forall q. LangRef '( BottomType, q) -> PinaforeAction q
langRefGet ref = do
    ka <- getImmutableReference $ langRefToImmutable ref
    pinaforeActionKnow ka

langRefSet :: forall p. LangRef '( p, TopType) -> Know p -> PinaforeAction ()
langRefSet (MutableLangRef (MkRange pt _) sr) mp =
    pinaforeValuePushAction sr $ pure $ MkWholeReaderEdit $ fmap (fromEnhanced pt) mp
langRefSet (ImmutableLangRef _) _ = empty

runLangRef :: LangRef '( BottomType, PinaforeAction ()) -> PinaforeAction ()
runLangRef ref = langRefGet ref >>= id

fLensLangRef :: forall ap aq b. (aq -> b) -> (b -> Maybe aq -> Maybe ap) -> LangRef '( ap, aq) -> LangRef '( b, b)
fLensLangRef g pb (MutableLangRef (tr :: Range JMShim a _) lv) = let
    trco = fromEnhanced $ rangeCo tr
    trcontra = fromEnhanced $ rangeContra tr
    lensG = fmap $ g . trco
    lensPB :: Know b -> Know a -> Maybe (Know a)
    lensPB kb ka =
        getComposeM $ do
            b <- liftInner kb
            a' <- liftOuter $ pb b $ knowToMaybe $ fmap trco ka
            return $ trcontra a'
    in MutableLangRef identityRange $ eaMap (wholeChangeLens (MkLens lensG lensPB)) lv
fLensLangRef g _ (ImmutableLangRef ir) = ImmutableLangRef $ fmap g ir
