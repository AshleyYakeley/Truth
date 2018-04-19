module Pinafore.Query.Types where

import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Table
import Shapes
import Truth.Core

type QLiteral baseedit t = PinaforeLensValue baseedit (WholeEdit (Maybe t))

type QImLiteral baseedit t = PinaforeFunctionValue baseedit (Maybe t)

type QPoint baseedit = QLiteral baseedit Point

type QImPoint baseedit = QImLiteral baseedit Point

type QLiteralSet baseedit t = PinaforeLensValue baseedit (FiniteSetEdit t)

type QSet baseedit = PinaforeLensValue baseedit (FiniteSetEdit Point)

type QImLiteralSet baseedit t = PinaforeFunctionValue baseedit (FiniteSet t)

type QImSet baseedit = QImLiteralSet baseedit Point

type QLiteralMorphism baseedit = PinaforeLensMorphism baseedit Point

type QPointMorphism baseedit = QLiteralMorphism baseedit Point

type QImLiteralMorphism baseedit t = PinaforeFunctionMorphism baseedit Point (Maybe t)

type QImPointMorphism baseedit = QImLiteralMorphism baseedit Point

qApplyMorphismValue :: QLiteralMorphism baseedit t -> QPoint baseedit -> QLiteral baseedit t
qApplyMorphismValue = applyPinaforeLens

qApplyImMorphismPoint :: QImLiteralMorphism baseedit t -> QImPoint baseedit -> QImLiteral baseedit t
qApplyImMorphismPoint f a = applyPinaforeFunction (arr (\mmt -> mmt >>= id) . cfmap f) a

qApplyImMorphismSet :: QImLiteralMorphism baseedit t -> QImSet baseedit -> QImLiteralSet baseedit t
qApplyImMorphismSet f a = applyPinaforeFunction (arr catMaybes . cfmap f) a

qImSetToLiteral :: (HasPinaforeTableEdit baseedit, AsLiteral val) => QImSet baseedit -> QImLiteralSet baseedit val
qImSetToLiteral = qApplyImMorphismSet $ lensFunctionMorphism literalPinaforeLensMorphism

qApplyMorphismSet :: QPointMorphism baseedit -> QSet baseedit -> QSet baseedit
qApplyMorphismSet f a =
    readOnlyEditLens $ convertEditFunction . qApplyImMorphismSet (lensFunctionMorphism f) (lensFunctionValue a)

qInverseApplyMorphismPoint :: QPointMorphism baseedit -> QPoint baseedit -> QSet baseedit
qInverseApplyMorphismPoint = applyInversePinaforeLens

qInverseApplyMorphismLiteral ::
       HasPinaforeTableEdit baseedit => QPointMorphism baseedit -> QLiteral baseedit Literal -> QSet baseedit
qInverseApplyMorphismLiteral f a = applyInversePinaforeLens (literalPinaforeLensMorphism . f) a

qInverseApplyMorphismConstant :: HasPinaforeTableEdit baseedit => QPointMorphism baseedit -> Literal -> QSet baseedit
qInverseApplyMorphismConstant f a = qInverseApplyMorphismLiteral f $ constEditLens $ Just a

qInverseApplyMorphismSet :: QPointMorphism baseedit -> QSet baseedit -> QSet baseedit
qInverseApplyMorphismSet f a =
    readOnlyEditLens $
    convertEditFunction .
    applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a)

type QActionM baseedit = ComposeM (Result Text) (View baseedit)

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
