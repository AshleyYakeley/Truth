module Pinafore.Language.Reference where

import Language.Expression.Dolan
import Pinafore.Base
import Shapes
import Truth.Core

data PinaforeReference (baseedit :: Type) (pq :: (Type, Type)) where
    MkPinaforeReference
        :: Range t pq -> PinaforeLensValue baseedit (WholeEdit (Know t)) -> PinaforeReference baseedit pq

unPinaforeReference :: PinaforeReference baseedit '( p, p) -> PinaforeLensValue baseedit (WholeEdit (Know p))
unPinaforeReference (MkPinaforeReference tr lv) = (bijectionWholeEditLens $ cfmap $ rangeBijection tr) . lv

getPinaforeReference :: PinaforeReference baseedit '( BottomType, q) -> PinaforeActionM baseedit (Know q)
getPinaforeReference (MkPinaforeReference (MkRange _ tq) lens) = do
    t <- pinaforeFunctionValueGet $ lensFunctionValue lens
    return $ fmap tq t

setPinaforeReference :: PinaforeReference baseedit '( p, TopType) -> Know p -> PinaforeAction baseedit
setPinaforeReference (MkPinaforeReference (MkRange pt _) lens) mp = pinaforeLensValueSet lens $ fmap pt mp

instance IsoMapRange (PinaforeReference baseedit)

instance MapRange (PinaforeReference baseedit) where
    mapRange f (MkPinaforeReference r s) = MkPinaforeReference (mapRange f r) s

instance HasDolanVary '[ 'Rangevariance] (PinaforeReference baseedit) where
    dolanVary = ConsDolanKindVary mapRange $ NilDolanKindVary

pinaforeReferenceToFunction :: PinaforeReference baseedit '( BottomType, a) -> PinaforeFunctionValue baseedit (Know a)
pinaforeReferenceToFunction (MkPinaforeReference (MkRange _ tq) lens) =
    funcEditFunction (fmap tq) . editLensFunction lens

pinaforeLensToReference :: PinaforeLensValue baseedit (WholeEdit (Know a)) -> PinaforeReference baseedit '( a, a)
pinaforeLensToReference lens = MkPinaforeReference identityRange lens

pinaforeFunctionToReference :: PinaforeFunctionValue baseedit (Know a) -> PinaforeReference baseedit '( BottomType, a)
pinaforeFunctionToReference ef = MkPinaforeReference (MkRange never id) $ readOnlyEditLens ef

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

pinaforeReferenceToImmutable :: PinaforeReference baseedit '( BottomType, a) -> PinaforeImmutableReference baseedit a
pinaforeReferenceToImmutable (MkPinaforeReference (MkRange _ tq) lens) =
    MkPinaforeImmutableReference $ funcEditFunction (fmap tq) . editLensFunction lens

pinaforeImmutableToReference :: PinaforeImmutableReference baseedit a -> PinaforeReference baseedit '( BottomType, a)
pinaforeImmutableToReference (MkPinaforeImmutableReference ef) =
    MkPinaforeReference (MkRange never id) $ readOnlyEditLens ef
