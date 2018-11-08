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
    dolanVary = ConsDolanKindVary mapRange $ NilDolanKindVary

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

getPinaforeReference :: PinaforeReference baseedit '( BottomType, q) -> PinaforeActionM baseedit (Know q)
getPinaforeReference ref = getImmutableReference $ pinaforeReferenceToImmutable ref

setPinaforeReference :: PinaforeReference baseedit '( p, TopType) -> Know p -> PinaforeAction baseedit
setPinaforeReference (LensPinaforeReference (MkRange pt _) lens) mp = pinaforeLensValueSet lens $ fmap pt mp
setPinaforeReference (ImmutPinaforeReference _) _ = return ()

pinaforeReferenceWith ::
       forall baseedit a.
       PinaforeReference baseedit '( BottomType, a)
    -> (a -> PinaforeAction baseedit)
    -> PinaforeAction baseedit
pinaforeReferenceWith ref cont = do
    ka <- getPinaforeReference ref
    case ka of
        Known a -> cont a
        Unknown -> return ()