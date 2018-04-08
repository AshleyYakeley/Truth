module Pinafore.Query.Value where

import Pinafore.Query.Order
import Pinafore.Query.Types
import Pinafore.Table
import Shapes
import Truth.Core

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
    QTOrder :: QType baseedit (QOrder baseedit)
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
    show QTOrder = "order"
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
    testEquality QTOrder QTOrder = Just Refl
    testEquality QTUserInterface QTUserInterface = Just Refl
    testEquality _ _ = Nothing

type QValue baseedit = Any (QType baseedit)

instance Show (QValue baseedit) where
    show (MkAny QTException val) = unpack $ "exception: " <> val
    show (MkAny QTConstant val) = unpack val
    show (MkAny QTUserInterface val) = show val
    show (MkAny QTList val) = "[" ++ intercalate "," (fmap show val) ++ "]"
    show (MkAny t _) = "<" ++ show t ++ ">"

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
        MkAny QTPoint a -> MkAny QTPoint $ qApplyMorphismValue f a
        MkAny QTSet a -> MkAny QTSet $ qApplyMorphismSet f a
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QTMorphism ++ " to " ++ show ta
qpartialapply (MkAny QTInverseMorphism f) =
    return $ \case
        MkAny QTConstant a -> MkAny QTSet $ qInverseApplyMorphismConstant f a
        MkAny QTLiteral a -> MkAny QTSet $ qInverseApplyMorphismLiteral f a
        MkAny QTPoint a -> MkAny QTSet $ qInverseApplyMorphismPoint f a
        MkAny QTSet a -> MkAny QTSet $ qInverseApplyMorphismSet f a
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
