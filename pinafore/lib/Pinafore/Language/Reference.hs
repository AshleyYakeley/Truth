module Pinafore.Language.Reference where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Instances ()
import Shapes
import Truth.Core

data PinaforeRef (baseedit :: Type) (pq :: (Type, Type)) where
    LensPinaforeRef :: Range JMShim t pq -> PinaforeLensValue baseedit (WholeEdit (Know t)) -> PinaforeRef baseedit pq
    ImmutPinaforeRef :: PinaforeImmutableReference baseedit q -> PinaforeRef baseedit '( p, q)

instance CatFunctor (CatRange (->)) (->) (PinaforeRef baseedit) where
    cfmap f (LensPinaforeRef r v) = LensPinaforeRef (cfmap f r) v
    cfmap (MkCatRange _ f) (ImmutPinaforeRef v) = ImmutPinaforeRef $ fmap f v

instance HasVariance 'Rangevariance (PinaforeRef baseedit) where
    varianceRepresentational = Nothing

pinaforeRefToFunction :: PinaforeRef baseedit '( BottomType, a) -> PinaforeFunctionValue baseedit (Know a)
pinaforeRefToFunction ref =
    case pinaforeRefToImmutable ref of
        (MkPinaforeImmutableReference fv) -> fv

pinaforeFunctionToRef :: PinaforeFunctionValue baseedit (Know a) -> PinaforeRef baseedit '( TopType, a)
pinaforeFunctionToRef ef = ImmutPinaforeRef $ MkPinaforeImmutableReference ef

pinaforeRefToImmutable :: PinaforeRef baseedit '( BottomType, a) -> PinaforeImmutableReference baseedit a
pinaforeRefToImmutable (LensPinaforeRef (MkRange _ tq) lens) =
    MkPinaforeImmutableReference $ funcEditFunction (fmap $ fromEnhanced tq) . editLensFunction lens
pinaforeRefToImmutable (ImmutPinaforeRef ir) = ir

pinaforeImmutableToRef :: PinaforeImmutableReference baseedit a -> PinaforeRef baseedit '( TopType, a)
pinaforeImmutableToRef = ImmutPinaforeRef

pinaforeRefToLens :: PinaforeRef baseedit '( p, p) -> PinaforeLensValue baseedit (WholeEdit (Know p))
pinaforeRefToLens (LensPinaforeRef tr lv) =
    (bijectionWholeEditLens $ isoMapCat (fromEnhanced @_ @JMShim) $ cfmap $ rangeBijection tr) . lv
pinaforeRefToLens (ImmutPinaforeRef ir) = immutableReferenceToLens ir

pinaforeLensToRef :: PinaforeLensValue baseedit (WholeEdit (Know a)) -> PinaforeRef baseedit '( a, a)
pinaforeLensToRef lens = LensPinaforeRef identityRange lens

pinaforeRefGet :: forall baseedit q. PinaforeRef baseedit '( BottomType, q) -> PinaforeAction baseedit q
pinaforeRefGet ref = (getImmutableReference $ pinaforeRefToImmutable ref) >>= pinaforeActionKnow

pinaforeRefSet :: forall baseedit p. PinaforeRef baseedit '( p, TopType) -> Know p -> PinaforeAction baseedit ()
pinaforeRefSet (LensPinaforeRef (MkRange pt _) lens) mp =
    pinaforeLensPush lens [MkWholeEdit $ fmap (fromEnhanced pt) mp]
pinaforeRefSet (ImmutPinaforeRef _) _ = empty

runPinaforeRef :: PinaforeRef baseedit '( BottomType, PinaforeAction baseedit ()) -> PinaforeAction baseedit ()
runPinaforeRef ref = pinaforeRefGet ref >>= id

pinaforeFLensRef ::
       forall baseedit ap aq b.
       (aq -> b)
    -> (b -> aq -> ap)
    -> PinaforeRef baseedit '( ap, aq)
    -> PinaforeRef baseedit '( b, b)
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
