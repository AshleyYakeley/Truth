module Pinafore.Base.ImmutableReference where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

newtype PinaforeImmutableReference baseedit a =
    MkPinaforeImmutableReference (PinaforeFunctionValue baseedit (Know a))

instance Functor (PinaforeImmutableReference baseedit) where
    fmap ab (MkPinaforeImmutableReference a) = MkPinaforeImmutableReference $ funcUpdateFunction (fmap ab) . a

instance Applicative (PinaforeImmutableReference baseedit) where
    pure a = MkPinaforeImmutableReference $ constUpdateFunction $ Known a
    (MkPinaforeImmutableReference fab) <*> (MkPinaforeImmutableReference fa) =
        MkPinaforeImmutableReference $ funcUpdateFunction (\(mab, ma) -> mab <*> ma) . pairWholeUpdateFunction fab fa

instance Alternative (PinaforeImmutableReference baseedit) where
    empty = MkPinaforeImmutableReference $ constUpdateFunction Unknown
    (MkPinaforeImmutableReference fa) <|> (MkPinaforeImmutableReference fb) =
        MkPinaforeImmutableReference $ funcUpdateFunction (\(ma, mb) -> ma <|> mb) . pairWholeUpdateFunction fa fb

immutableReferenceToFunction :: PinaforeImmutableReference baseedit a -> PinaforeFunctionValue baseedit (Know a)
immutableReferenceToFunction (MkPinaforeImmutableReference fv) = fv

immutableReferenceToLens :: PinaforeImmutableReference baseedit a -> PinaforeLensValue baseedit (WholeEdit (Know a))
immutableReferenceToLens ref = readOnlyEditLens $ immutableReferenceToFunction ref

getImmutableReference :: PinaforeImmutableReference baseedit a -> PinaforeAction baseedit (Know a)
getImmutableReference ref = pinaforeFunctionValueGet $ immutableReferenceToFunction ref

functionImmutableReference :: PinaforeFunctionValue baseedit a -> PinaforeImmutableReference baseedit a
functionImmutableReference fv = MkPinaforeImmutableReference $ funcUpdateFunction Known . fv

pinaforeImmutableReferenceValue :: a -> PinaforeImmutableReference baseedit a -> PinaforeFunctionValue baseedit a
pinaforeImmutableReferenceValue def ref = funcUpdateFunction (fromKnow def) . immutableReferenceToFunction ref

applyImmutableReference ::
       PinaforeFunctionMorphism baseedit (Know a) (Know b)
    -> PinaforeImmutableReference baseedit a
    -> PinaforeImmutableReference baseedit b
applyImmutableReference m (MkPinaforeImmutableReference v) = MkPinaforeImmutableReference $ applyPinaforeFunction m v
