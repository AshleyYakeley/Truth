module Pinafore.Base.ImmutableReference where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Pinafore.Base.Value
import Shapes
import Truth.Core

newtype PinaforeImmutableReference baseupdate a =
    MkPinaforeImmutableReference (PinaforeFunctionValue baseupdate (Know a))

instance Functor (PinaforeImmutableReference baseupdate) where
    fmap ab (MkPinaforeImmutableReference a) = MkPinaforeImmutableReference $ funcUpdateFunction (fmap ab) . a

instance Applicative (PinaforeImmutableReference baseupdate) where
    pure a = MkPinaforeImmutableReference $ constUpdateFunction $ Known a
    (MkPinaforeImmutableReference fab) <*> (MkPinaforeImmutableReference fa) =
        MkPinaforeImmutableReference $ funcUpdateFunction (\(mab, ma) -> mab <*> ma) . pairWholeUpdateFunction fab fa

instance Alternative (PinaforeImmutableReference baseupdate) where
    empty = MkPinaforeImmutableReference $ constUpdateFunction Unknown
    (MkPinaforeImmutableReference fa) <|> (MkPinaforeImmutableReference fb) =
        MkPinaforeImmutableReference $ funcUpdateFunction (\(ma, mb) -> ma <|> mb) . pairWholeUpdateFunction fa fb

immutableReferenceToFunction :: PinaforeImmutableReference baseupdate a -> PinaforeFunctionValue baseupdate (Know a)
immutableReferenceToFunction (MkPinaforeImmutableReference fv) = fv

immutableReferenceToLens ::
       PinaforeImmutableReference baseupdate a -> PinaforeLensValue baseupdate (WholeUpdate (Know a))
immutableReferenceToLens ref = updateFunctionToRejectingEditLens $ immutableReferenceToFunction ref

getImmutableReference :: PinaforeImmutableReference baseupdate a -> PinaforeAction baseupdate (Know a)
getImmutableReference ref = pinaforeFunctionValueGet $ immutableReferenceToFunction ref

functionImmutableReference :: PinaforeFunctionValue baseupdate a -> PinaforeImmutableReference baseupdate a
functionImmutableReference fv = MkPinaforeImmutableReference $ funcUpdateFunction Known . fv

pinaforeImmutableReferenceValue :: a -> PinaforeImmutableReference baseupdate a -> PinaforeFunctionValue baseupdate a
pinaforeImmutableReferenceValue def ref = funcUpdateFunction (fromKnow def) . immutableReferenceToFunction ref

applyImmutableReference ::
       PinaforeFunctionMorphism baseupdate (Know a) (Know b)
    -> PinaforeImmutableReference baseupdate a
    -> PinaforeImmutableReference baseupdate b
applyImmutableReference m (MkPinaforeImmutableReference v) = MkPinaforeImmutableReference $ applyPinaforeFunction m v
