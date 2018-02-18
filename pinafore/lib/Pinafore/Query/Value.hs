module Pinafore.Query.Value where

import Pinafore.AsText
import Pinafore.Edit
import Pinafore.Morphism
import Shapes
import Truth.Core

data QType t where
    QException :: QType Text
    QLiteral :: QType Text
    QPoint :: QType (PinaforeLensValue (WholeEdit (Maybe Point)))
    QSet :: QType (PinaforeLensValue (FiniteSetEdit Point))
    QMorphism :: QType (PinaforeLensMorphism Point Point)
    QInverseMorphism :: QType (PinaforeLensMorphism Point Point)
    QList :: QType [QValue]
    QFunction :: QType (QValue -> QValue)
    QUISpec :: QType (UISpec PinaforeEdit)

instance Show (QType t) where
    show QException = "exception"
    show QLiteral = "literal"
    show QPoint = "point"
    show QSet = "set"
    show QMorphism = "morphism"
    show QInverseMorphism = "inverse morphism"
    show QList = "list"
    show QFunction = "function"
    show QUISpec = "user interface"

instance TestEquality QType where
    testEquality QException QException = Just Refl
    testEquality QLiteral QLiteral = Just Refl
    testEquality QPoint QPoint = Just Refl
    testEquality QSet QSet = Just Refl
    testEquality QMorphism QMorphism = Just Refl
    testEquality QInverseMorphism QInverseMorphism = Just Refl
    testEquality QList QList = Just Refl
    testEquality QFunction QFunction = Just Refl
    testEquality QUISpec QUISpec = Just Refl
    testEquality _ _ = Nothing

type QValue = Any QType

instance Show QValue where
    show (MkAny QException val) = unpack $ "exception: " <> val
    show (MkAny QLiteral val) = unpack val
    show (MkAny QUISpec val) = show val
    show (MkAny QList val) = "[" ++ intercalate "," (fmap show val) ++ "]"
    show (MkAny t _) = "<" ++ show t ++ ">"

qliteral :: Text -> QValue
qliteral = MkAny QLiteral

qfunction :: (QValue -> QValue) -> QValue
qfunction = MkAny QFunction

qexception :: Text -> QValue
qexception = MkAny QException

qapply :: QValue -> QValue -> QValue
qapply (MkAny QException ex) _ = MkAny QException ex
qapply (MkAny QFunction f) a = f a
qapply (MkAny QMorphism f) (MkAny QPoint a) = MkAny QPoint $ applyPinaforeLens f a
qapply (MkAny QMorphism f) (MkAny QSet a) =
    MkAny QSet $
    readOnlyEditLens $
    convertEditFunction . applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a)
qapply (MkAny QInverseMorphism f) (MkAny QLiteral a) =
    MkAny QSet $ applyInversePinaforeLens (literalPinaforeLensMorphism . f) $ constEditLens $ Just a
qapply (MkAny QInverseMorphism f) (MkAny QPoint a) = MkAny QSet $ applyInversePinaforeLens f a
qapply (MkAny QInverseMorphism f) (MkAny QSet a) =
    MkAny QSet $
    readOnlyEditLens $
    convertEditFunction .
    applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a)
qapply (MkAny tf _) (MkAny ta _) = qexception $ pack $ "cannot apply " ++ show tf ++ " to " ++ show ta

qinvert :: QValue -> QValue
qinvert (MkAny QException ex) = MkAny QException ex
qinvert (MkAny QMorphism m) = MkAny QInverseMorphism m
qinvert (MkAny QInverseMorphism m) = MkAny QMorphism m
qinvert (MkAny t _) = qexception $ pack $ "cannot invert " ++ show t

qcombine :: QValue -> QValue -> QValue
qcombine (MkAny QMorphism g) (MkAny QMorphism f) = MkAny QMorphism $ g . f
qcombine (MkAny QInverseMorphism g) (MkAny QInverseMorphism f) = MkAny QInverseMorphism $ g . f
qcombine g f = MkAny QFunction $ qapply g . qapply f

qpredicate :: Predicate -> QValue
qpredicate p = MkAny QMorphism $ predicatePinaforeLensMorphism p

qpoint :: Point -> QValue
qpoint p = MkAny QPoint $ constEditLens $ Just p

qmeet ::
       PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
qmeet a b = readOnlyEditLens meetEditFunction . pairJoinEditLenses a b

qjoin ::
       PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
qjoin a b = readOnlyEditLens joinEditFunction . pairJoinEditLenses a b

maybeToFiniteSet :: Maybe a -> FiniteSet a
maybeToFiniteSet (Just a) = opoint a
maybeToFiniteSet Nothing = mempty

qdisplay :: QValue -> PinaforeFunctionValue (FiniteSet Text)
qdisplay val =
    case fromQValue val of
        SuccessResult a -> a
        FailureResult _ -> constEditFunction $ opoint $ pack $ show val

badFromQValue :: QValue -> Result Text t
badFromQValue (MkAny QException s) = FailureResult s
badFromQValue (MkAny t _) = fail $ "unexpected " ++ show t

class FromQValue t where
    fromQValue :: QValue -> Result Text t
    qTypeDescriptionFrom :: Text
    qTypeDescriptionFromSingle :: Text
    qTypeDescriptionFromSingle = qTypeDescriptionFrom @t

instance FromQValue QValue where
    fromQValue = return
    qTypeDescriptionFrom = "value"

instance FromQValue Text where
    fromQValue (MkAny QLiteral v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "text-literal"

instance FromQValue Bool where
    fromQValue (MkAny QLiteral "false") = return False
    fromQValue (MkAny QLiteral "true") = return True
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "boolean-literal"

instance FromQValue (PinaforeLensValue (WholeEdit (Maybe Point))) where
    fromQValue (MkAny QPoint v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "point"

instance FromQValue (PinaforeLensValue (WholeEdit (Maybe Text))) where
    fromQValue (MkAny QLiteral v) = return $ constEditLens $ Just v
    fromQValue (MkAny QPoint v) = return $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "text"

instance FromQValue (PinaforeLensValue (WholeEdit (Maybe t))) => FromQValue (PinaforeFunctionValue (Maybe t)) where
    fromQValue v = do
        a :: PinaforeLensValue (WholeEdit (Maybe t)) <- fromQValue v
        return $ lensFunctionValue a
    qTypeDescriptionFrom = qTypeDescriptionFrom @(PinaforeLensValue (WholeEdit (Maybe t)))

instance FromQValue (PinaforeLensValue (FiniteSetEdit Point)) where
    fromQValue (MkAny QPoint v) = return $ (readOnlyEditLens $ funcEditFunction maybeToFiniteSet) . v
    fromQValue (MkAny QSet v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "set"

instance FromQValue (PinaforeFunctionValue (FiniteSet Point)) where
    fromQValue (MkAny QPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue (MkAny QSet a) =
        return $ applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "point"

instance FromQValue (PinaforeFunctionValue (FiniteSet Text)) where
    fromQValue (MkAny QLiteral a) = return $ constEditFunction $ opoint a
    fromQValue (MkAny QPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction
                   (arr mms . cfmap (lensFunctionMorphism literalPinaforeLensMorphism))
                   (lensFunctionValue a)
    fromQValue (MkAny QSet a) =
        return $
        applyPinaforeFunction
            (arr catMaybes . cfmap (lensFunctionMorphism literalPinaforeLensMorphism))
            (lensFunctionValue a)
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "text"

instance FromQValue (PinaforeLensMorphism Point Point) where
    fromQValue (MkAny QMorphism v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "point-morphism"

instance FromQValue (PinaforeLensMorphism Point Text) where
    fromQValue v = do
        m <- fromQValue v
        return $ literalPinaforeLensMorphism . m
    qTypeDescriptionFrom = "text-morphism"

instance FromQValue (PinaforeLensMorphism a b) => FromQValue (PinaforeFunctionMorphism a (Maybe b)) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m
    qTypeDescriptionFrom = qTypeDescriptionFrom @(PinaforeLensMorphism a b)

instance FromQValue (UISpec PinaforeEdit) where
    fromQValue (MkAny QUISpec v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "uispec"

instance FromQValue (UIWindow PinaforeEdit) where
    fromQValue v = do
        (title, content) <- fromQValue v
        return $ MkUIWindow (funcEditFunction @(WholeEdit (Maybe Text)) (fromMaybe "") . title) content
    qTypeDescriptionFrom =
        qTypeDescriptionFrom @(EditFunction PinaforeEdit (WholeEdit (Maybe Text)), UISpec PinaforeEdit)

instance FromQValue t => FromQValue [t] where
    fromQValue (MkAny QList v) = for v fromQValue
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "[" <> qTypeDescriptionFrom @t <> "]"

instance (FromQValue a, FromQValue b) => FromQValue (a, b) where
    fromQValue (MkAny QList [va, vb]) = do
        a <- fromQValue va
        b <- fromQValue vb
        return (a, b)
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "(" <> qTypeDescriptionFrom @a <> ", " <> qTypeDescriptionFrom @b <> ")"

instance FromQValue t => FromQValue (Result Text t) where
    fromQValue v = fmap return $ fromQValue v
    qTypeDescriptionFrom = "result " <> qTypeDescriptionFrom @t

instance FromQValue t => FromQValue (IO t) where
    fromQValue v = fmap return $ fromQValue v
    qTypeDescriptionFrom = "action " <> qTypeDescriptionFrom @t

instance (ToQValue a, FromQValue b) => FromQValue (a -> Result Text b) where
    fromQValue vf = return $ fromQValue . qapply vf . toQValue
    qTypeDescriptionFrom = qTypeDescriptionToSingle @a <> " -> " <> qTypeDescriptionFrom @b
    qTypeDescriptionFromSingle = "(" <> qTypeDescriptionFrom @(a -> Result Text b) <> ")"

class ToQValue t where
    toQValue :: t -> QValue
    qTypeDescriptionTo :: Text
    qTypeDescriptionToSingle :: Text
    qTypeDescriptionToSingle = qTypeDescriptionTo @t

instance ToQValue QValue where
    toQValue = id
    qTypeDescriptionTo = "value"

instance ToQValue t => ToQValue (Result Text t) where
    toQValue (SuccessResult a) = toQValue a
    toQValue (FailureResult e) = qexception e
    qTypeDescriptionTo = "result " <> qTypeDescriptionTo @t

instance (FromQValue a, ToQValue b) => ToQValue (a -> b) where
    toQValue ab = qfunction $ toQValue . fmap ab . fromQValue
    qTypeDescriptionTo = qTypeDescriptionFromSingle @a <> " -> " <> qTypeDescriptionTo @b
    qTypeDescriptionToSingle = "(" <> qTypeDescriptionTo @(a -> b) <> ")"

instance ToQValue Predicate where
    toQValue p = qpredicate p
    qTypeDescriptionTo = "predicate"

instance ToQValue Point where
    toQValue p = qpoint p
    qTypeDescriptionTo = "point"

instance ToQValue Text where
    toQValue p = qliteral p
    qTypeDescriptionTo = "text-literal"

instance ToQValue Bool where
    toQValue t = toQValue $ toText t
    qTypeDescriptionTo = "boolean-literal"

instance ToQValue Int where
    toQValue t = toQValue $ toText t
    qTypeDescriptionTo = "integer-literal"

instance ToQValue Integer where
    toQValue t = toQValue $ toText t
    qTypeDescriptionTo = "integer-literal"

instance ToQValue t => ToQValue [t] where
    toQValue t = MkAny QList $ fmap toQValue t
    qTypeDescriptionTo = "[" <> qTypeDescriptionTo @t <> "]"

instance (ToQValue a, ToQValue b) => ToQValue (a, b) where
    toQValue (a, b) = toQValue [toQValue a, toQValue b]
    qTypeDescriptionTo = "(" <> qTypeDescriptionTo @a <> ", " <> qTypeDescriptionTo @b <> ")"

instance ToQValue (PinaforeLensValue (WholeEdit (Maybe Point))) where
    toQValue t = MkAny QPoint t
    qTypeDescriptionTo = "point"

instance ToQValue (PinaforeLensValue (FiniteSetEdit Point)) where
    toQValue t = MkAny QSet t
    qTypeDescriptionTo = "set"

instance edit ~ PinaforeEdit => ToQValue (UISpec edit) where
    toQValue t = MkAny QUISpec t
    qTypeDescriptionTo = "uispec"
