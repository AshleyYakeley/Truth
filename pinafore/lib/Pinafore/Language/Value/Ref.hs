module Pinafore.Language.Value.Ref where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Instances ()
import Shapes
import Truth.Core

data PinaforeRef (baseupdate :: Type) (pq :: (Type, Type)) where
    LensPinaforeRef
        :: Range JMShim t pq -> PinaforeLensValue baseupdate (WholeUpdate (Know t)) -> PinaforeRef baseupdate pq
    ImmutPinaforeRef :: PinaforeImmutableReference baseupdate q -> PinaforeRef baseupdate '( p, q)

instance CatFunctor (CatRange (->)) (->) (PinaforeRef baseupdate) where
    cfmap f (LensPinaforeRef r v) = LensPinaforeRef (cfmap f r) v
    cfmap (MkCatRange _ f) (ImmutPinaforeRef v) = ImmutPinaforeRef $ fmap f v

instance HasVariance 'Rangevariance (PinaforeRef baseupdate) where
    varianceRepresentational = Nothing

pinaforeRefToFunction :: PinaforeRef baseupdate '( BottomType, a) -> PinaforeFunctionValue baseupdate (Know a)
pinaforeRefToFunction ref =
    case pinaforeRefToImmutable ref of
        (MkPinaforeImmutableReference fv) -> fv

pinaforeFunctionToRef :: PinaforeFunctionValue baseupdate (Know a) -> PinaforeRef baseupdate '( TopType, a)
pinaforeFunctionToRef ef = ImmutPinaforeRef $ MkPinaforeImmutableReference ef

pinaforeRefToImmutable :: PinaforeRef baseupdate '( BottomType, a) -> PinaforeImmutableReference baseupdate a
pinaforeRefToImmutable (LensPinaforeRef (MkRange _ tq) lens) =
    MkPinaforeImmutableReference $ funcUpdateFunction (fmap $ fromEnhanced tq) . editLensFunction lens
pinaforeRefToImmutable (ImmutPinaforeRef ir) = ir

pinaforeImmutableToRef :: PinaforeImmutableReference baseupdate a -> PinaforeRef baseupdate '( TopType, a)
pinaforeImmutableToRef = ImmutPinaforeRef

pinaforeRefToLens :: PinaforeRef baseupdate '( p, p) -> PinaforeLensValue baseupdate (WholeUpdate (Know p))
pinaforeRefToLens (LensPinaforeRef tr lv) =
    (bijectionWholeEditLens $ isoMapCat (fromEnhanced @_ @JMShim) $ cfmap $ rangeBijection tr) . lv
pinaforeRefToLens (ImmutPinaforeRef ir) = immutableReferenceToLens ir

pinaforeLensToRef :: PinaforeLensValue baseupdate (WholeUpdate (Know a)) -> PinaforeRef baseupdate '( a, a)
pinaforeLensToRef lens = LensPinaforeRef identityRange lens

pinaforeRefGet :: forall baseupdate q. PinaforeRef baseupdate '( BottomType, q) -> PinaforeAction baseupdate q
pinaforeRefGet ref = (getImmutableReference $ pinaforeRefToImmutable ref) >>= pinaforeActionKnow

pinaforeRefSet :: forall baseupdate p. PinaforeRef baseupdate '( p, TopType) -> Know p -> PinaforeAction baseupdate ()
pinaforeRefSet (LensPinaforeRef (MkRange pt _) lens) mp =
    pinaforeLensPush lens [MkWholeReaderEdit $ fmap (fromEnhanced pt) mp]
pinaforeRefSet (ImmutPinaforeRef _) _ = empty

runPinaforeRef :: PinaforeRef baseupdate '( BottomType, PinaforeAction baseupdate ()) -> PinaforeAction baseupdate ()
runPinaforeRef ref = pinaforeRefGet ref >>= id

pinaforeFLensRef ::
       forall baseupdate ap aq b.
       (aq -> b)
    -> (b -> aq -> ap)
    -> PinaforeRef baseupdate '( ap, aq)
    -> PinaforeRef baseupdate '( b, b)
pinaforeFLensRef g pb (LensPinaforeRef tr lv) = let
    trco = fromEnhanced $ rangeCo tr
    trcontra = fromEnhanced $ rangeContra tr
    lensG = fmap $ g . trco
    lensPB kb ka =
        Identity $ do
            b <- kb
            a <- ka
            return $ trcontra $ pb b (trco a)
    in LensPinaforeRef identityRange $ wholeEditLens (MkLens lensG lensPB) . lv
pinaforeFLensRef g _ (ImmutPinaforeRef ir) = ImmutPinaforeRef $ fmap g ir
