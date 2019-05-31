module Pinafore.Language.Reference where

import Language.Expression.Dolan
import Pinafore.Base
import Shapes
import Truth.Core

data PinaforeReference (baseedit :: Type) (pq :: (Type, Type)) where
    LensPinaforeReference
        :: Range t pq -> PinaforeLensValue baseedit (WholeEdit (Know t)) -> PinaforeReference baseedit pq
    ImmutPinaforeReference :: PinaforeImmutableReference baseedit q -> PinaforeReference baseedit '( p, q)

instance IsoMapRange (PinaforeReference baseedit)

instance MapRange (PinaforeReference baseedit) where
    mapRange f (LensPinaforeReference r s) = LensPinaforeReference (mapRange f r) s
    mapRange (MkWithRange _ f) (ImmutPinaforeReference ir) = ImmutPinaforeReference $ fmap f ir

instance HasDolanVary '[ 'Rangevariance] (PinaforeReference baseedit) where
    dolanVary = ConsDolanVarianceMap mapRange $ NilDolanVarianceMap

pinaforeReferenceToFunction :: PinaforeReference baseedit '( BottomType, a) -> PinaforeFunctionValue baseedit (Know a)
pinaforeReferenceToFunction ref =
    case pinaforeReferenceToImmutable ref of
        (MkPinaforeImmutableReference fv) -> fv

pinaforeFunctionToReference :: PinaforeFunctionValue baseedit (Know a) -> PinaforeReference baseedit '( TopType, a)
pinaforeFunctionToReference ef = ImmutPinaforeReference $ MkPinaforeImmutableReference ef

pinaforeReferenceToImmutable :: PinaforeReference baseedit '( BottomType, a) -> PinaforeImmutableReference baseedit a
pinaforeReferenceToImmutable (LensPinaforeReference (MkRange _ tq) lens) =
    MkPinaforeImmutableReference $ funcEditFunction (fmap tq) . editLensFunction lens
pinaforeReferenceToImmutable (ImmutPinaforeReference ir) = ir

pinaforeImmutableToReference :: PinaforeImmutableReference baseedit a -> PinaforeReference baseedit '( TopType, a)
pinaforeImmutableToReference = ImmutPinaforeReference

pinaforeReferenceToLens :: PinaforeReference baseedit '( p, p) -> PinaforeLensValue baseedit (WholeEdit (Know p))
pinaforeReferenceToLens (LensPinaforeReference tr lv) = (bijectionWholeEditLens $ cfmap $ rangeBijection tr) . lv
pinaforeReferenceToLens (ImmutPinaforeReference ir) = immutableReferenceToLens ir

pinaforeLensToReference :: PinaforeLensValue baseedit (WholeEdit (Know a)) -> PinaforeReference baseedit '( a, a)
pinaforeLensToReference lens = LensPinaforeReference identityRange lens

pinaforeReferenceGet :: forall baseedit q. PinaforeReference baseedit '( BottomType, q) -> PinaforeAction baseedit q
pinaforeReferenceGet ref = (getImmutableReference $ pinaforeReferenceToImmutable ref) >>= pinaforeActionKnow

pinaforeReferenceSet ::
       forall baseedit p. PinaforeReference baseedit '( p, TopType) -> Know p -> PinaforeAction baseedit ()
pinaforeReferenceSet (LensPinaforeReference (MkRange pt _) lens) mp = pinaforeLensPush lens [MkWholeEdit $ fmap pt mp]
pinaforeReferenceSet (ImmutPinaforeReference _) _ = empty

runPinaforeReference ::
       PinaforeReference baseedit '( BottomType, PinaforeAction baseedit ()) -> PinaforeAction baseedit ()
runPinaforeReference ref = pinaforeReferenceGet ref >>= id

pinaforeFLensReference ::
       forall baseedit ap aq b.
       (aq -> b)
    -> (b -> aq -> ap)
    -> PinaforeReference baseedit '( ap, aq)
    -> PinaforeReference baseedit '( b, b)
pinaforeFLensReference g pb (LensPinaforeReference tr lv) = let
    lensG = fmap $ g . rangeCo tr
    lensPB kb ka =
        Identity $ do
            b <- kb
            a <- ka
            return $ rangeContra tr $ pb b (rangeCo tr a)
    in LensPinaforeReference identityRange $ wholeEditLens (MkLens lensG lensPB) . lv
pinaforeFLensReference g _ (ImmutPinaforeReference ir) = ImmutPinaforeReference $ fmap g ir
