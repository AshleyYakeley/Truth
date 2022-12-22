module Pinafore.Base.Model.ImmutableWholeModel where

import Changes.Core
import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Model.FunctionMorphism
import Pinafore.Base.Model.Model
import Shapes

newtype ImmutableWholeModel a =
    MkImmutableWholeModel (WROWModel (Know a))

instance Functor ImmutableWholeModel where
    fmap ab (MkImmutableWholeModel sa) = MkImmutableWholeModel $ eaMapReadOnlyWhole (fmap ab) sa

instance Applicative ImmutableWholeModel where
    pure a = MkImmutableWholeModel $ eaPure $ Known a
    (MkImmutableWholeModel sab) <*> (MkImmutableWholeModel sa) =
        MkImmutableWholeModel $ eaMapReadOnlyWhole (\(mab, ma) -> mab <*> ma) $ eaPairReadOnlyWhole sab sa

instance Alternative ImmutableWholeModel where
    empty = MkImmutableWholeModel $ eaPure Unknown
    (MkImmutableWholeModel sa) <|> (MkImmutableWholeModel sb) =
        MkImmutableWholeModel $ eaMapReadOnlyWhole (\(ma, mb) -> ma <|> mb) $ eaPairReadOnlyWhole sa sb

immutableModelToReadOnlyModel :: ImmutableWholeModel a -> WROWModel (Know a)
immutableModelToReadOnlyModel (MkImmutableWholeModel fv) = fv

immutableModelToRejectingModel :: ImmutableWholeModel a -> WModel (WholeUpdate (Know a))
immutableModelToRejectingModel model = eaMap fromReadOnlyRejectingChangeLens $ immutableModelToReadOnlyModel model

immutableModelToRejectingBiModel :: ImmutableWholeModel a -> WModel (BiUpdate pupdate (WholeUpdate (Know a)))
immutableModelToRejectingBiModel model =
    eaMap (fromReadOnlyRejectingChangeLens . readOnlyBiChangeLens) $ immutableModelToReadOnlyModel model

getImmutableModel :: ImmutableWholeModel a -> Action (Know a)
getImmutableModel model = actionModelGet (immutableModelToReadOnlyModel model) $ readM ReadWhole

functionImmutableModel :: WROWModel a -> ImmutableWholeModel a
functionImmutableModel fv = MkImmutableWholeModel $ eaMap (liftReadOnlyChangeLens $ funcChangeLens Known) fv

immutableWholeModelValue :: a -> ImmutableWholeModel a -> WROWModel a
immutableWholeModelValue def model = eaMapReadOnlyWhole (fromKnow def) $ immutableModelToReadOnlyModel model

applyImmutableModel ::
       Model baseupdate
    -> StorageFunctionMorphism baseupdate (Know a) (Know b)
    -> ImmutableWholeModel a
    -> ImmutableWholeModel b
applyImmutableModel basesub m (MkImmutableWholeModel v) = MkImmutableWholeModel $ applyStorageFunction basesub m v
