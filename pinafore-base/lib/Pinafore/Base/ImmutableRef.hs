module Pinafore.Base.ImmutableRef where

import Changes.Core
import Pinafore.Base.Action
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.Know
import Pinafore.Base.Ref
import Shapes

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

immutableRefToReadOnlyRef :: PinaforeImmutableRef a -> PinaforeROWRef (Know a)
immutableRefToReadOnlyRef (MkPinaforeImmutableRef fv) = fv

immutableRefToRejectingRef :: PinaforeImmutableRef a -> PinaforeRef (WholeUpdate (Know a))
immutableRefToRejectingRef ref = eaMap fromReadOnlyRejectingChangeLens $ immutableRefToReadOnlyRef ref

immutableRefToRejectingBiRef :: PinaforeImmutableRef a -> PinaforeRef (BiUpdate pupdate (WholeUpdate (Know a)))
immutableRefToRejectingBiRef ref =
    eaMap (fromReadOnlyRejectingChangeLens . readOnlyBiChangeLens) $ immutableRefToReadOnlyRef ref

getImmutableRef :: PinaforeImmutableRef a -> PinaforeAction (Know a)
getImmutableRef ref = do
    rc <- pinaforeResourceContext
    liftIO $ pinaforeFunctionValueGet rc $ immutableRefToReadOnlyRef ref

functionImmutableRef :: PinaforeROWRef a -> PinaforeImmutableRef a
functionImmutableRef fv = MkPinaforeImmutableRef $ eaMap (liftReadOnlyChangeLens $ funcChangeLens Known) fv

pinaforeImmutableRefValue :: a -> PinaforeImmutableRef a -> PinaforeROWRef a
pinaforeImmutableRefValue def ref = eaMapReadOnlyWhole (fromKnow def) $ immutableRefToReadOnlyRef ref

applyImmutableRef ::
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
    -> PinaforeImmutableRef a
    -> PinaforeImmutableRef b
applyImmutableRef basesub m (MkPinaforeImmutableRef v) = MkPinaforeImmutableRef $ applyPinaforeFunction basesub m v
