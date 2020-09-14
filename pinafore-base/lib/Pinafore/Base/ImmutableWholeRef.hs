module Pinafore.Base.ImmutableWholeRef where

import Changes.Core
import Pinafore.Base.Action
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.Know
import Pinafore.Base.Ref
import Shapes

newtype PinaforeImmutableWholeRef a =
    MkPinaforeImmutableWholeRef (PinaforeROWRef (Know a))

instance Functor PinaforeImmutableWholeRef where
    fmap ab (MkPinaforeImmutableWholeRef sa) = MkPinaforeImmutableWholeRef $ eaMapReadOnlyWhole (fmap ab) sa

instance Applicative PinaforeImmutableWholeRef where
    pure a = MkPinaforeImmutableWholeRef $ eaPure $ Known a
    (MkPinaforeImmutableWholeRef sab) <*> (MkPinaforeImmutableWholeRef sa) =
        MkPinaforeImmutableWholeRef $ eaMapReadOnlyWhole (\(mab, ma) -> mab <*> ma) $ eaPairReadOnlyWhole sab sa

instance Alternative PinaforeImmutableWholeRef where
    empty = MkPinaforeImmutableWholeRef $ eaPure Unknown
    (MkPinaforeImmutableWholeRef sa) <|> (MkPinaforeImmutableWholeRef sb) =
        MkPinaforeImmutableWholeRef $ eaMapReadOnlyWhole (\(ma, mb) -> ma <|> mb) $ eaPairReadOnlyWhole sa sb

immutableRefToReadOnlyRef :: PinaforeImmutableWholeRef a -> PinaforeROWRef (Know a)
immutableRefToReadOnlyRef (MkPinaforeImmutableWholeRef fv) = fv

immutableRefToRejectingRef :: PinaforeImmutableWholeRef a -> PinaforeRef (WholeUpdate (Know a))
immutableRefToRejectingRef ref = eaMap fromReadOnlyRejectingChangeLens $ immutableRefToReadOnlyRef ref

immutableRefToRejectingBiRef :: PinaforeImmutableWholeRef a -> PinaforeRef (BiUpdate pupdate (WholeUpdate (Know a)))
immutableRefToRejectingBiRef ref =
    eaMap (fromReadOnlyRejectingChangeLens . readOnlyBiChangeLens) $ immutableRefToReadOnlyRef ref

getImmutableRef :: PinaforeImmutableWholeRef a -> PinaforeAction (Know a)
getImmutableRef ref = do
    rc <- pinaforeResourceContext
    liftIO $ pinaforeFunctionValueGet rc $ immutableRefToReadOnlyRef ref

functionImmutableRef :: PinaforeROWRef a -> PinaforeImmutableWholeRef a
functionImmutableRef fv = MkPinaforeImmutableWholeRef $ eaMap (liftReadOnlyChangeLens $ funcChangeLens Known) fv

pinaforeImmutableRefValue :: a -> PinaforeImmutableWholeRef a -> PinaforeROWRef a
pinaforeImmutableRefValue def ref = eaMapReadOnlyWhole (fromKnow def) $ immutableRefToReadOnlyRef ref

applyImmutableRef ::
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef b
applyImmutableRef basesub m (MkPinaforeImmutableWholeRef v) =
    MkPinaforeImmutableWholeRef $ applyPinaforeFunction basesub m v
