module Pinafore.Base.ImmutableWholeModel where

import Changes.Core
import Pinafore.Base.Action
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.Know
import Pinafore.Base.Ref
import Shapes

newtype PinaforeImmutableWholeModel a =
    MkPinaforeImmutableWholeModel (PinaforeROWModel (Know a))

instance Functor PinaforeImmutableWholeModel where
    fmap ab (MkPinaforeImmutableWholeModel sa) = MkPinaforeImmutableWholeModel $ eaMapReadOnlyWhole (fmap ab) sa

instance Applicative PinaforeImmutableWholeModel where
    pure a = MkPinaforeImmutableWholeModel $ eaPure $ Known a
    (MkPinaforeImmutableWholeModel sab) <*> (MkPinaforeImmutableWholeModel sa) =
        MkPinaforeImmutableWholeModel $ eaMapReadOnlyWhole (\(mab, ma) -> mab <*> ma) $ eaPairReadOnlyWhole sab sa

instance Alternative PinaforeImmutableWholeModel where
    empty = MkPinaforeImmutableWholeModel $ eaPure Unknown
    (MkPinaforeImmutableWholeModel sa) <|> (MkPinaforeImmutableWholeModel sb) =
        MkPinaforeImmutableWholeModel $ eaMapReadOnlyWhole (\(ma, mb) -> ma <|> mb) $ eaPairReadOnlyWhole sa sb

immutableModelToReadOnlyModel :: PinaforeImmutableWholeModel a -> PinaforeROWModel (Know a)
immutableModelToReadOnlyModel (MkPinaforeImmutableWholeModel fv) = fv

immutableModelToRejectingModel :: PinaforeImmutableWholeModel a -> WModel (WholeUpdate (Know a))
immutableModelToRejectingModel model = eaMap fromReadOnlyRejectingChangeLens $ immutableModelToReadOnlyModel model

immutableModelToRejectingBiModel :: PinaforeImmutableWholeModel a -> WModel (BiUpdate pupdate (WholeUpdate (Know a)))
immutableModelToRejectingBiModel model =
    eaMap (fromReadOnlyRejectingChangeLens . readOnlyBiChangeLens) $ immutableModelToReadOnlyModel model

getImmutableModel :: PinaforeImmutableWholeModel a -> PinaforeAction (Know a)
getImmutableModel model = pinaforeModelGet (immutableModelToReadOnlyModel model) $ readM ReadWhole

functionImmutableModel :: PinaforeROWModel a -> PinaforeImmutableWholeModel a
functionImmutableModel fv = MkPinaforeImmutableWholeModel $ eaMap (liftReadOnlyChangeLens $ funcChangeLens Known) fv

pinaforeImmutableModelValue :: a -> PinaforeImmutableWholeModel a -> PinaforeROWModel a
pinaforeImmutableModelValue def model = eaMapReadOnlyWhole (fromKnow def) $ immutableModelToReadOnlyModel model

applyImmutableModel ::
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
    -> PinaforeImmutableWholeModel a
    -> PinaforeImmutableWholeModel b
applyImmutableModel basesub m (MkPinaforeImmutableWholeModel v) =
    MkPinaforeImmutableWholeModel $ applyPinaforeFunction basesub m v
