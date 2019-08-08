module Pinafore.Base.ImmutableReference where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

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

immutableReferenceToFunction :: PinaforeImmutableReference baseedit a -> PinaforeFunctionValue baseedit (Know a)
immutableReferenceToFunction (MkPinaforeImmutableReference fv) = fv

immutableReferenceToLens :: PinaforeImmutableReference baseedit a -> PinaforeLensValue baseedit (WholeEdit (Know a))
immutableReferenceToLens ref = readOnlyEditLens $ immutableReferenceToFunction ref

getImmutableReference :: PinaforeImmutableReference baseedit a -> PinaforeAction baseedit (Know a)
getImmutableReference ref = pinaforeFunctionValueGet $ immutableReferenceToFunction ref

functionImmutableReference :: PinaforeFunctionValue baseedit a -> PinaforeImmutableReference baseedit a
functionImmutableReference fv = MkPinaforeImmutableReference $ funcEditFunction Known . fv

pinaforeImmutableReferenceValue :: a -> PinaforeImmutableReference baseedit a -> PinaforeFunctionValue baseedit a
pinaforeImmutableReferenceValue def ref = funcEditFunction (fromKnow def) . immutableReferenceToFunction ref

applyImmutableReference ::
       PinaforeFunctionMorphism baseedit (Know a) (Know b)
    -> PinaforeImmutableReference baseedit a
    -> PinaforeImmutableReference baseedit b
applyImmutableReference m (MkPinaforeImmutableReference v) = MkPinaforeImmutableReference $ applyPinaforeFunction m v
