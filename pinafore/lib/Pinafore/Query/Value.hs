module Pinafore.Query.Value where

import Pinafore.Literal
import Pinafore.PredicateMorphism
import Pinafore.Query.Order
import Pinafore.Query.Types
import Pinafore.Table
import Shapes
import Truth.Core

data QType baseedit t where
    QTException :: QType baseedit Text
    QTConstLiteral :: QType baseedit Literal
    QTRefLiteral :: QType baseedit (QRefLiteral baseedit Literal)
    QTRefPoint :: QType baseedit (QRefPoint baseedit)
    QTRefSet :: QType baseedit (QRefSetPoint baseedit)
    QTMorphism :: QType baseedit (QMorphismRefPoint baseedit)
    QTInverseMorphism :: QType baseedit (QMorphismRefPoint baseedit)
    QTList :: QType baseedit [QValue baseedit]
    QTFunction :: QType baseedit (QValue baseedit -> QValue baseedit)
    QTAction :: QType baseedit (QAction baseedit)
    QTOrder :: QType baseedit (QOrder baseedit)
    QTUserInterface :: QType baseedit (UISpec (ConstEdit Point) baseedit)

instance Show (QType baseedit t) where
    show QTException = "exception"
    show QTConstLiteral = "literal!"
    show QTRefLiteral = "literal*"
    show QTRefPoint = "entity*"
    show QTRefSet = "set*"
    show QTMorphism = "entity* ~> entity*"
    show QTInverseMorphism = "entity* <~ entity*"
    show QTList = "[value]"
    show QTFunction = "value -> value"
    show QTAction = "action"
    show QTOrder = "order"
    show QTUserInterface = "user interface"

instance TestEquality (QType baseedit) where
    testEquality QTException QTException = Just Refl
    testEquality QTConstLiteral QTConstLiteral = Just Refl
    testEquality QTRefLiteral QTRefLiteral = Just Refl
    testEquality QTRefPoint QTRefPoint = Just Refl
    testEquality QTRefSet QTRefSet = Just Refl
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
    show (MkAny QTConstLiteral val) = show $ unpack $ unLiteral val
    show (MkAny QTUserInterface val) = show val
    show (MkAny QTList val) = "[" ++ intercalate "," (fmap show val) ++ "]"
    show (MkAny t _) = "<" ++ show t ++ ">"

qconstant :: Literal -> QValue baseedit
qconstant = MkAny QTConstLiteral

qpredicate :: HasPinaforeEntityEdit baseedit => Predicate -> QValue baseedit
qpredicate p = MkAny QTMorphism $ predicatePinaforeLensMorphism p

qpoint :: Point -> QValue baseedit
qpoint p = MkAny QTRefPoint $ constEditLens p

qfunction :: (QValue baseedit -> QValue baseedit) -> QValue baseedit
qfunction = MkAny QTFunction

qexception :: Text -> QValue baseedit
qexception = MkAny QTException

qpartialapply :: HasPinaforeEntityEdit baseedit => QValue baseedit -> Result Text (QValue baseedit -> QValue baseedit)
qpartialapply (MkAny QTException ex) = FailureResult ex
qpartialapply (MkAny QTFunction f) = return f
qpartialapply (MkAny QTMorphism f) =
    return $ \case
        MkAny QTException ex -> qexception ex
        MkAny QTRefPoint a -> MkAny QTRefPoint $ qApplyMorphismRefPoint f a
        MkAny QTRefSet a -> MkAny QTRefSet $ qApplyMorphismRefPointSet f a
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QTMorphism ++ " to " ++ show ta
qpartialapply (MkAny QTInverseMorphism f) =
    return $ \case
        MkAny QTException ex -> qexception ex
        MkAny QTConstLiteral a -> MkAny QTRefSet $ qInverseApplyMorphismRefToConstant f a
        MkAny QTRefLiteral a -> MkAny QTRefSet $ qInverseApplyMorphismRefToLiteral f a
        MkAny QTRefPoint a -> MkAny QTRefSet $ qInverseApplyMorphismRefToPoint f a
        MkAny QTRefSet a -> MkAny QTRefSet $ qInverseApplyMorphismRefToSet f a
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QTInverseMorphism ++ " to " ++ show ta
qpartialapply (MkAny tf _) = FailureResult $ pack $ "cannot apply " ++ show tf

qapply :: HasPinaforeEntityEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qapply vf va =
    case qpartialapply vf of
        SuccessResult f -> f va
        FailureResult ex -> MkAny QTException ex

qinvert :: QValue baseedit -> QValue baseedit
qinvert (MkAny QTException ex) = MkAny QTException ex
qinvert (MkAny QTMorphism m) = MkAny QTInverseMorphism m
qinvert (MkAny QTInverseMorphism m) = MkAny QTMorphism m
qinvert (MkAny t _) = qexception $ pack $ "cannot invert " ++ show t
