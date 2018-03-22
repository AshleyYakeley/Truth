module Pinafore.Query.Value where

import Pinafore.Morphism
import Pinafore.Table
import Shapes
import Truth.Core

type QLiteral baseedit t = PinaforeLensValue baseedit (WholeEdit (Maybe t))

type QImLiteral baseedit t = PinaforeFunctionValue baseedit (Maybe t)

type QPoint baseedit = QLiteral baseedit Point

type QImPoint baseedit = QImLiteral baseedit Point

type QSet baseedit = PinaforeLensValue baseedit (FiniteSetEdit Point)

type QImSet baseedit = PinaforeFunctionValue baseedit (FiniteSet Point)

type QLiteralMorphism baseedit = PinaforeLensMorphism baseedit Point

type QPointMorphism baseedit = QLiteralMorphism baseedit Point

type QImLiteralMorphism baseedit t = PinaforeFunctionMorphism baseedit Point (Maybe t)

type QImPointMorphism baseedit = QImLiteralMorphism baseedit Point

type QAction baseedit = View baseedit ()

data QType baseedit t where
    QTException :: QType baseedit Text
    QTConstant :: QType baseedit Text
    QTLiteral :: QType baseedit (QLiteral baseedit Text)
    QTPoint :: QType baseedit (QPoint baseedit)
    QTSet :: QType baseedit (QSet baseedit)
    QTMorphism :: QType baseedit (QPointMorphism baseedit)
    QTInverseMorphism :: QType baseedit (QPointMorphism baseedit)
    QTList :: QType baseedit [QValue baseedit]
    QTFunction :: QType baseedit (QValue baseedit -> QValue baseedit)
    QTAction :: QType baseedit (QAction baseedit)
    QTUserInterface :: QType baseedit (UISpec baseedit)

instance Show (QType baseedit t) where
    show QTException = "exception"
    show QTConstant = "constant"
    show QTLiteral = "literal"
    show QTPoint = "point"
    show QTSet = "set"
    show QTMorphism = "morphism"
    show QTInverseMorphism = "inverse morphism"
    show QTList = "list"
    show QTFunction = "function"
    show QTAction = "action"
    show QTUserInterface = "user interface"

instance TestEquality (QType baseedit) where
    testEquality QTException QTException = Just Refl
    testEquality QTConstant QTConstant = Just Refl
    testEquality QTLiteral QTLiteral = Just Refl
    testEquality QTPoint QTPoint = Just Refl
    testEquality QTSet QTSet = Just Refl
    testEquality QTMorphism QTMorphism = Just Refl
    testEquality QTInverseMorphism QTInverseMorphism = Just Refl
    testEquality QTList QTList = Just Refl
    testEquality QTFunction QTFunction = Just Refl
    testEquality QTAction QTAction = Just Refl
    testEquality QTUserInterface QTUserInterface = Just Refl
    testEquality _ _ = Nothing

type QValue baseedit = Any (QType baseedit)

instance Show (QValue baseedit) where
    show (MkAny QTException val) = unpack $ "exception: " <> val
    show (MkAny QTConstant val) = unpack val
    show (MkAny QTUserInterface val) = show val
    show (MkAny QTList val) = "[" ++ intercalate "," (fmap show val) ++ "]"
    show (MkAny t _) = "<" ++ show t ++ ">"

badFromQValue :: QValue baseedit -> Result Text t
badFromQValue (MkAny QTException s) = FailureResult s
badFromQValue (MkAny t _) = fail $ "unexpected " ++ show t

qconstant :: Text -> QValue baseedit
qconstant = MkAny QTConstant

qpredicate :: HasPinaforeTableEdit baseedit => Predicate -> QValue baseedit
qpredicate p = MkAny QTMorphism $ predicatePinaforeLensMorphism p

qpoint :: Point -> QValue baseedit
qpoint p = MkAny QTPoint $ constEditLens $ Just p

qfunction :: (QValue baseedit -> QValue baseedit) -> QValue baseedit
qfunction = MkAny QTFunction

qexception :: Text -> QValue baseedit
qexception = MkAny QTException

qpartialapply :: HasPinaforeTableEdit baseedit => QValue baseedit -> Result Text (QValue baseedit -> QValue baseedit)
qpartialapply (MkAny QTException ex) = FailureResult ex
qpartialapply (MkAny QTFunction f) = return f
qpartialapply (MkAny QTMorphism f) =
    return $ \case
        MkAny QTPoint a -> MkAny QTPoint $ applyPinaforeLens f a
        MkAny QTSet a ->
            MkAny QTSet $
            readOnlyEditLens $
            convertEditFunction .
            applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a)
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QTMorphism ++ " to " ++ show ta
qpartialapply (MkAny QTInverseMorphism f) =
    return $ \case
        MkAny QTConstant a ->
            MkAny QTSet $ applyInversePinaforeLens (literalPinaforeLensMorphism . f) $ constEditLens $ Just a
        MkAny QTLiteral a -> MkAny QTSet $ applyInversePinaforeLens (literalPinaforeLensMorphism . f) a
        MkAny QTPoint a -> MkAny QTSet $ applyInversePinaforeLens f a
        MkAny QTSet a ->
            MkAny QTSet $
            readOnlyEditLens $
            convertEditFunction .
            applyPinaforeFunction
                (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f))
                (lensFunctionValue a)
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QTInverseMorphism ++ " to " ++ show ta
qpartialapply (MkAny tf _) = FailureResult $ pack $ "cannot apply " ++ show tf

qapply :: HasPinaforeTableEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qapply vf va =
    case qpartialapply vf of
        SuccessResult f -> f va
        FailureResult ex -> MkAny QTException ex

qinvert :: QValue baseedit -> QValue baseedit
qinvert (MkAny QTException ex) = MkAny QTException ex
qinvert (MkAny QTMorphism m) = MkAny QTInverseMorphism m
qinvert (MkAny QTInverseMorphism m) = MkAny QTMorphism m
qinvert (MkAny t _) = qexception $ pack $ "cannot invert " ++ show t
