module Pinafore.Types where

--import Pinafore.Entity
import Pinafore.Know
import Pinafore.Morphism
import Pinafore.Table (Point)
import Shapes
import Truth.Core

type QFuncValue baseedit t = PinaforeFunctionValue baseedit (Know t)

type QLensValue baseedit t = PinaforeLensValue baseedit (WholeEdit (Know t))

type QFuncSet baseedit t = PinaforeFunctionValue baseedit (FiniteSet t)

type QLensSet baseedit t = PinaforeLensValue baseedit (FiniteSetEdit t)

{-
type QFuncPoint baseedit = PinaforeFunctionValue baseedit Point

type QLensPoint baseedit = PinaforeLensValue baseedit (WholeEdit Point)

type QFuncSetPoint baseedit = QFuncSet baseedit Point

type QLensSetPoint baseedit = QLensSet baseedit Point
-}
type QMorphismRefLiteral baseedit t = PinaforeLensMorphism baseedit Point (Know t)

type QMorphismRefPoint baseedit = PinaforeLensMorphism baseedit Point Point

type QMorphismLiteral baseedit t = PinaforeFunctionMorphism baseedit Point (Know t)

type QMorphismPoint baseedit = PinaforeFunctionMorphism baseedit Point Point
{-
qApplyMorphismRefPoint :: QMorphismRefPoint baseedit -> QLensPoint baseedit -> QLensPoint baseedit
qApplyMorphismRefPoint = applyPinaforeLens
-}
{-
qApplyMorphismLiteralSet :: QMorphismLiteral baseedit t -> QFuncSetPoint baseedit -> QFuncSet baseedit t
qApplyMorphismLiteralSet f a = applyPinaforeFunction (arr catKnowns . cfmap f) a

qApplyMorphismRefPointSet :: QMorphismRefPoint baseedit -> QLensSetPoint baseedit -> QLensSetPoint baseedit
qApplyMorphismRefPointSet f a =
    readOnlyEditLens $
    convertEditFunction . applyPinaforeFunction (cfmap $ lensFunctionMorphism f) (lensFunctionValue a)
-}
{-
qInverseApplyMorphismRefToPoint :: QMorphismRefPoint baseedit -> QLensPoint baseedit -> QLensSetPoint baseedit
qInverseApplyMorphismRefToPoint = applyInversePinaforeLens newPoint
-}
{-
qInverseApplyMorphismRefToLiteral ::
       (AsLiteral t, HasPinaforeEntityEdit baseedit)
    => QMorphismRefPoint baseedit
    -> QLensValue baseedit t
    -> QLensSetPoint baseedit
qInverseApplyMorphismRefToLiteral f a = applyInversePinaforeLens (return Unknown) (literalPinaforeLensMorphism . f) a

qInverseApplyMorphismRefToConstant ::
       (AsLiteral t, HasPinaforeEntityEdit baseedit) => QMorphismRefPoint baseedit -> t -> QLensSetPoint baseedit
qInverseApplyMorphismRefToConstant f a = qInverseApplyMorphismRefToLiteral f $ constEditLens $ Known a
-}
{-
qInverseApplyMorphismRefToSet :: QMorphismRefPoint baseedit -> QLensSetPoint baseedit -> QLensSetPoint baseedit
qInverseApplyMorphismRefToSet = applyInversePinaforeLensSet newPoint
-}
