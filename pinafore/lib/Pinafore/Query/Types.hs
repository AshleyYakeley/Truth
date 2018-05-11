module Pinafore.Query.Types where

import Pinafore.Entity
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.PredicateMorphism
import Pinafore.Table (Point)
import Shapes
import Truth.Core

type QRefLiteral baseedit t = PinaforeLensValue baseedit (WholeEdit (Maybe t))

type QLiteral baseedit t = PinaforeFunctionValue baseedit (Maybe t)

type QRefPoint baseedit = PinaforeLensValue baseedit (WholeEdit Point)

type QPoint baseedit = PinaforeFunctionValue baseedit Point

type QRefSetPoint baseedit = PinaforeLensValue baseedit (FiniteSetEdit Point)

type QSetLiteral baseedit t = PinaforeFunctionValue baseedit (FiniteSet t)

type QSetPoint baseedit = QSetLiteral baseedit Point

type QMorphismRefLiteral baseedit t = PinaforeLensMorphism baseedit Point (Maybe t)

type QMorphismRefPoint baseedit = PinaforeLensMorphism baseedit Point Point

type QMorphismLiteral baseedit t = PinaforeFunctionMorphism baseedit Point (Maybe t)

type QMorphismPoint baseedit = PinaforeFunctionMorphism baseedit Point Point

qApplyMorphismRefPoint :: QMorphismRefPoint baseedit -> QRefPoint baseedit -> QRefPoint baseedit
qApplyMorphismRefPoint = applyPinaforeLens

qApplyMorphismLiteralSet :: QMorphismLiteral baseedit t -> QSetPoint baseedit -> QSetLiteral baseedit t
qApplyMorphismLiteralSet f a = applyPinaforeFunction (arr catMaybes . cfmap f) a

qApplyMorphismRefPointSet :: QMorphismRefPoint baseedit -> QRefSetPoint baseedit -> QRefSetPoint baseedit
qApplyMorphismRefPointSet f a =
    readOnlyEditLens $
    convertEditFunction . applyPinaforeFunction (cfmap $ lensFunctionMorphism f) (lensFunctionValue a)

qInverseApplyMorphismRefToPoint :: QMorphismRefPoint baseedit -> QRefPoint baseedit -> QRefSetPoint baseedit
qInverseApplyMorphismRefToPoint = applyInversePinaforeLens newPoint

qInverseApplyMorphismRefToLiteral ::
       (AsLiteral t, HasPinaforeEntityEdit baseedit)
    => QMorphismRefPoint baseedit
    -> QRefLiteral baseedit t
    -> QRefSetPoint baseedit
qInverseApplyMorphismRefToLiteral f a = applyInversePinaforeLens (return Nothing) (literalPinaforeLensMorphism . f) a

qInverseApplyMorphismRefToConstant ::
       (AsLiteral t, HasPinaforeEntityEdit baseedit) => QMorphismRefPoint baseedit -> t -> QRefSetPoint baseedit
qInverseApplyMorphismRefToConstant f a = qInverseApplyMorphismRefToLiteral f $ constEditLens $ Just a

qInverseApplyMorphismRefToSet :: QMorphismRefPoint baseedit -> QRefSetPoint baseedit -> QRefSetPoint baseedit
qInverseApplyMorphismRefToSet f a =
    readOnlyEditLens $
    convertEditFunction .
    applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a)

type QActionM baseedit = ComposeM (Result Text) (View (ConstEdit Point) baseedit)

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

qGetFunctionValue :: PinaforeFunctionValue baseedit t -> QActionM baseedit t
qGetFunctionValue fval = liftOuter $ viewObjectRead $ \_ mr -> editFunctionRead fval mr ReadWhole

actionRequest :: IOWitness t -> QActionM baseedit t
actionRequest wit =
    MkComposeM $ do
        mt <- viewRequest wit
        return $
            case mt of
                Just t -> SuccessResult t
                Nothing -> FailureResult $ "failed request"

type QAction baseedit = QActionM baseedit ()
