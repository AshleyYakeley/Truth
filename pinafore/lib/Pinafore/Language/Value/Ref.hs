module Pinafore.Language.Value.Ref where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Instances ()
import Shapes
import Truth.Core

data PinaforeRef (pq :: (Type, Type)) where
    MutablePinaforeRef :: Range JMShim t pq -> PinaforeValue (WholeUpdate (Know t)) -> PinaforeRef pq
    ImmutablePinaforeRef :: PinaforeImmutableReference q -> PinaforeRef '( p, q)

instance CatFunctor (CatRange (->)) (->) PinaforeRef where
    cfmap f (MutablePinaforeRef r v) = MutablePinaforeRef (cfmap f r) v
    cfmap (MkCatRange _ f) (ImmutablePinaforeRef v) = ImmutablePinaforeRef $ fmap f v

instance HasVariance 'Rangevariance PinaforeRef where
    varianceRepresentational = Nothing

pinaforeRefToReadOnlyValue :: PinaforeRef '( BottomType, a) -> PinaforeReadOnlyValue (Know a)
pinaforeRefToReadOnlyValue ref =
    case pinaforeRefToImmutable ref of
        MkPinaforeImmutableReference fv -> fv

pinaforeReadOnlyValueToRef :: PinaforeReadOnlyValue (Know a) -> PinaforeRef '( TopType, a)
pinaforeReadOnlyValueToRef ef = pinaforeImmutableToRef $ MkPinaforeImmutableReference ef

pinaforeRefToImmutable :: PinaforeRef '( BottomType, a) -> PinaforeImmutableReference a
pinaforeRefToImmutable (MutablePinaforeRef (MkRange _ tq) sr) =
    fmap (fromEnhanced tq) $ MkPinaforeImmutableReference $ eaToReadOnlyWhole sr
pinaforeRefToImmutable (ImmutablePinaforeRef ir) = ir

pinaforeImmutableToRef :: PinaforeImmutableReference a -> PinaforeRef '( TopType, a)
pinaforeImmutableToRef ir = ImmutablePinaforeRef ir

pinaforeRefToValue :: PinaforeRef '( p, p) -> PinaforeValue (WholeUpdate (Know p))
pinaforeRefToValue (MutablePinaforeRef tr lv) =
    eaMap (bijectionWholeEditLens $ isoMapCat (fromEnhanced @_ @JMShim) $ cfmap $ rangeBijection tr) lv
pinaforeRefToValue (ImmutablePinaforeRef ir) = immutableReferenceToRejectingValue ir

pinaforeValueToRef :: (PinaforeValue (WholeUpdate (Know a))) -> PinaforeRef '( a, a)
pinaforeValueToRef bsv = MutablePinaforeRef identityRange bsv

pinaforeRefGet :: forall q. PinaforeRef '( BottomType, q) -> PinaforeAction q
pinaforeRefGet ref = do
    ka <- getImmutableReference $ pinaforeRefToImmutable ref
    pinaforeActionKnow ka

pinaforeRefSet :: forall p. PinaforeRef '( p, TopType) -> Know p -> PinaforeAction ()
pinaforeRefSet (MutablePinaforeRef (MkRange pt _) sr) mp =
    pinaforeValuePushAction sr $ pure $ MkWholeReaderEdit $ fmap (fromEnhanced pt) mp
pinaforeRefSet (ImmutablePinaforeRef _) _ = empty

runPinaforeRef :: PinaforeRef '( BottomType, PinaforeAction ()) -> PinaforeAction ()
runPinaforeRef ref = pinaforeRefGet ref >>= id

pinaforeFLensRef ::
       forall ap aq b. (aq -> b) -> (b -> Maybe aq -> Maybe ap) -> PinaforeRef '( ap, aq) -> PinaforeRef '( b, b)
pinaforeFLensRef g pb (MutablePinaforeRef (tr :: Range JMShim a _) lv) = let
    trco = fromEnhanced $ rangeCo tr
    trcontra = fromEnhanced $ rangeContra tr
    lensG = fmap $ g . trco
    lensPB :: Know b -> Know a -> Maybe (Know a)
    lensPB kb ka =
        getComposeM $ do
            b <- liftInner kb
            a' <- liftOuter $ pb b $ knowToMaybe $ fmap trco ka
            return $ trcontra a'
    in MutablePinaforeRef identityRange $ eaMap (wholeEditLens (MkLens lensG lensPB)) lv
pinaforeFLensRef g _ (ImmutablePinaforeRef ir) = ImmutablePinaforeRef $ fmap g ir
