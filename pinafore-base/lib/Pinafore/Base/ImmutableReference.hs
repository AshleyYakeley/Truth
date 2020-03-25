module Pinafore.Base.ImmutableReference where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Pinafore.Base.Value
import Shapes
import Truth.Core

newtype PinaforeImmutableReference a =
    MkPinaforeImmutableReference (PinaforeReadOnlyValue (Know a))

instance Functor PinaforeImmutableReference where
    fmap ab (MkPinaforeImmutableReference sa) = MkPinaforeImmutableReference $ eaMapReadOnlyWhole (fmap ab) sa

instance Applicative PinaforeImmutableReference where
    pure a = MkPinaforeImmutableReference $ eaPure $ Known a
    (MkPinaforeImmutableReference sab) <*> (MkPinaforeImmutableReference sa) =
        MkPinaforeImmutableReference $ eaMapReadOnlyWhole (\(mab, ma) -> mab <*> ma) $ eaPairReadOnlyWhole sab sa

instance Alternative PinaforeImmutableReference where
    empty = MkPinaforeImmutableReference $ eaPure Unknown
    (MkPinaforeImmutableReference sa) <|> (MkPinaforeImmutableReference sb) =
        MkPinaforeImmutableReference $ eaMapReadOnlyWhole (\(ma, mb) -> ma <|> mb) $ eaPairReadOnlyWhole sa sb

immutableReferenceToReadOnlyValue :: PinaforeImmutableReference a -> PinaforeReadOnlyValue (Know a)
immutableReferenceToReadOnlyValue (MkPinaforeImmutableReference fv) = fv

immutableReferenceToRejectingValue :: PinaforeImmutableReference a -> PinaforeValue (WholeUpdate (Know a))
immutableReferenceToRejectingValue ref = eaMap fromReadOnlyRejectingEditLens $ immutableReferenceToReadOnlyValue ref

getImmutableReference :: PinaforeImmutableReference a -> PinaforeAction (Know a)
getImmutableReference ref = do
    rc <- pinaforeResourceContext
    liftIO $ pinaforeFunctionValueGet rc $ immutableReferenceToReadOnlyValue ref

functionImmutableReference :: PinaforeReadOnlyValue a -> PinaforeImmutableReference a
functionImmutableReference fv = MkPinaforeImmutableReference $ eaMap (liftReadOnlyEditLens $ funcEditLens Known) fv

pinaforeImmutableReferenceValue :: a -> PinaforeImmutableReference a -> PinaforeReadOnlyValue a
pinaforeImmutableReferenceValue def ref = eaMapReadOnlyWhole (fromKnow def) $ immutableReferenceToReadOnlyValue ref

applyImmutableReference ::
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
    -> PinaforeImmutableReference a
    -> PinaforeImmutableReference b
applyImmutableReference basesub m (MkPinaforeImmutableReference v) =
    MkPinaforeImmutableReference $ applyPinaforeFunction basesub m v
