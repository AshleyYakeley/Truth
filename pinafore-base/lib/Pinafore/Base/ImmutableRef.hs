module Pinafore.Base.ImmutableRef where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Pinafore.Base.Ref
import Shapes
import Truth.Core

newtype PinaforeImmutableRef a =
    MkPinaforeImmutableRef (PinaforeROWRef (Know a))

instance Functor PinaforeImmutableRef where
    fmap ab (MkPinaforeImmutableRef sa) = MkPinaforeImmutableRef $ eaMapReadOnlyWhole (fmap ab) sa

instance Applicative PinaforeImmutableRef where
    pure a = MkPinaforeImmutableRef $ eaPure $ Known a
    (MkPinaforeImmutableRef sab) <*> (MkPinaforeImmutableRef sa) =
        MkPinaforeImmutableRef $ eaMapReadOnlyWhole (\(mab, ma) -> mab <*> ma) $ eaPairReadOnlyWhole sab sa

instance Alternative PinaforeImmutableRef where
    empty = MkPinaforeImmutableRef $ eaPure Unknown
    (MkPinaforeImmutableRef sa) <|> (MkPinaforeImmutableRef sb) =
        MkPinaforeImmutableRef $ eaMapReadOnlyWhole (\(ma, mb) -> ma <|> mb) $ eaPairReadOnlyWhole sa sb

immutableRefToReadOnlyValue :: PinaforeImmutableRef a -> PinaforeROWRef (Know a)
immutableRefToReadOnlyValue (MkPinaforeImmutableRef fv) = fv

immutableRefToRejectingValue :: PinaforeImmutableRef a -> PinaforeRef (WholeUpdate (Know a))
immutableRefToRejectingValue ref = eaMap fromReadOnlyRejectingChangeLens $ immutableRefToReadOnlyValue ref

getImmutableRef :: PinaforeImmutableRef a -> PinaforeAction (Know a)
getImmutableRef ref = do
    rc <- pinaforeResourceContext
    liftIO $ pinaforeFunctionValueGet rc $ immutableRefToReadOnlyValue ref

functionImmutableRef :: PinaforeROWRef a -> PinaforeImmutableRef a
functionImmutableRef fv = MkPinaforeImmutableRef $ eaMap (liftReadOnlyChangeLens $ funcChangeLens Known) fv

pinaforeImmutableRefValue :: a -> PinaforeImmutableRef a -> PinaforeROWRef a
pinaforeImmutableRefValue def ref = eaMapReadOnlyWhole (fromKnow def) $ immutableRefToReadOnlyValue ref

applyImmutableRef ::
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
    -> PinaforeImmutableRef a
    -> PinaforeImmutableRef b
applyImmutableRef basesub m (MkPinaforeImmutableRef v) = MkPinaforeImmutableRef $ applyPinaforeFunction basesub m v
