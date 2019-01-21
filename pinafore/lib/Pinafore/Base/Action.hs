module Pinafore.Base.Action where

import Pinafore.Base.Entity
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

type PinaforeActionM baseedit = ReaderT (UIWindow baseedit -> IO (), Object baseedit) IO

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

pinaforeLiftResult :: Result Text a -> PinaforeActionM baseedit a
pinaforeLiftResult = resultTextToM

pinaforeFunctionValueGet :: PinaforeFunctionValue baseedit t -> PinaforeActionM baseedit t
pinaforeFunctionValueGet fval = do
    (_, MkObject {..}) <- ask
    liftIO $ runTransform objRun $ editFunctionRead fval objRead ReadWhole

pinaforeLensPush :: PinaforeLensValue baseedit edit -> [edit] -> PinaforeAction baseedit
pinaforeLensPush lens edits = do
    (_, object) <- ask
    case lensObject True lens object of
        MkObject {..} -> liftIO $ runTransform objRun $ pushEdit $ objEdit edits

pinaforeLensValueSet :: PinaforeLensValue baseedit (WholeEdit t) -> t -> PinaforeAction baseedit
pinaforeLensValueSet lens v = pinaforeLensPush lens [MkWholeEdit v]

type PinaforeAction baseedit = PinaforeActionM baseedit ()

pinaforeNewEntity :: PinaforeActionM baseedit Entity
pinaforeNewEntity = liftIO $ newKeyContainerItem @(FiniteSet Entity)

pinaforeListFor :: [a] -> (a -> PinaforeAction baseedit) -> PinaforeAction baseedit
pinaforeListFor = for_

pinaforeNewWindow :: UIWindow baseedit -> PinaforeAction baseedit
pinaforeNewWindow uiw = do
    (neww, _) <- ask
    liftIO $ neww uiw

newtype PinaforeContext baseedit =
    MkPinaforeContext (UnliftIO (PinaforeActionM baseedit))

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeActionM baseedit a -> IO a
runPinaforeAction action =
    case ?pinafore of
        MkPinaforeContext unlift -> runTransform unlift action
