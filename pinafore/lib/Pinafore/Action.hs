module Pinafore.Action where

import Pinafore.Language.Entity
import Pinafore.Morphism
import Pinafore.Table (Point)
import Shapes
import Truth.Core

type PinaforeActionM baseedit = ComposeM (Result Text) (View (ConstEdit Entity) baseedit)

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

pinaforeLiftView :: View (ConstEdit Entity) baseedit a -> PinaforeActionM baseedit a
pinaforeLiftView = liftOuter

pinaforeLiftResult :: Result Text a -> PinaforeActionM baseedit a
pinaforeLiftResult = liftInner

pinaforeFunctionValueGet :: PinaforeFunctionValue baseedit t -> PinaforeActionM baseedit t
pinaforeFunctionValueGet fval = pinaforeLiftView $ viewObjectRead $ \_ mr -> editFunctionRead fval mr ReadWhole

pinaforeLensValueSet :: PinaforeLensValue baseedit (WholeEdit t) -> t -> PinaforeAction baseedit
pinaforeLensValueSet lens v = pinaforeLiftView $ viewMapEdit lens $ viewObjectPushEdit $ \_ push -> push [MkWholeEdit v]

pinaforeActionRequest :: IOWitness t -> PinaforeActionM baseedit t
pinaforeActionRequest wit =
    MkComposeM $ do
        mt <- viewRequest wit
        return $
            case mt of
                Just t -> SuccessResult t
                Nothing -> FailureResult $ "failed request"

type PinaforeAction baseedit = PinaforeActionM baseedit ()

pinaforeGeneratePoint :: PinaforeActionM baseedit Point
pinaforeGeneratePoint = liftIO $ newKeyContainerItem @(FiniteSet Point)

pinaforeListFor :: [a] -> (a -> PinaforeAction baseedit) -> PinaforeAction baseedit
pinaforeListFor = for_
