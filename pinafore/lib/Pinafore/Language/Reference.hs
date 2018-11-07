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

newtype PinaforeImmutableReference baseedit a =
    MkPinaforeImmutableReference (PinaforeFunctionValue baseedit (Know a))

instance Functor (PinaforeImmutableReference baseedit) where
    fmap ab (MkPinaforeImmutableReference a) = MkPinaforeImmutableReference $ funcEditFunction (fmap ab) . a

instance Applicative (PinaforeImmutableReference baseedit) where
    pure a = MkPinaforeImmutableReference $ constEditFunction $ Known a
    (MkPinaforeImmutableReference fab) <*> (MkPinaforeImmutableReference fa) =
        MkPinaforeImmutableReference $ funcEditFunction (\(mab, ma) -> mab <*> ma) . pairWholeEditFunction fab fa

instance Alternative (PinaforeImmutableReference baseedit) where
    empty = MkPinaforeImmutableReference $ constEditFunction Unknown
    (MkPinaforeImmutableReference fa) <|> (MkPinaforeImmutableReference fb) =
        MkPinaforeImmutableReference $ funcEditFunction (\(ma, mb) -> ma <|> mb) . pairWholeEditFunction fa fb

pinaforeReferenceToImmutable :: PinaforeReference baseedit '( BottomType, a) -> PinaforeImmutableReference baseedit a
pinaforeReferenceToImmutable (MkPinaforeReference (MkRange _ tq) lens) =
    MkPinaforeImmutableReference $ funcEditFunction (fmap tq) . editLensFunction lens

pinaforeImmutableToReference :: PinaforeImmutableReference baseedit a -> PinaforeReference baseedit '( BottomType, a)
pinaforeImmutableToReference (MkPinaforeImmutableReference ef) =
    MkPinaforeReference (MkRange never id) $ readOnlyEditLens ef

pinaforeImmutableReferenceValue :: a -> PinaforeImmutableReference baseedit a -> PinaforeFunctionValue baseedit a
pinaforeImmutableReferenceValue def (MkPinaforeImmutableReference f) = funcEditFunction (fromKnow def) . f

applyImmutableReference ::
       PinaforeFunctionMorphism baseedit (Know a) (Know b)
    -> PinaforeImmutableReference baseedit a
    -> PinaforeImmutableReference baseedit b
applyImmutableReference m (MkPinaforeImmutableReference v) = MkPinaforeImmutableReference $ applyPinaforeFunction m v
