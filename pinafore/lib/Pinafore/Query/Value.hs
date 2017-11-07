module Pinafore.Query.Value where

import Pinafore.AsText
import Pinafore.Edit
import Shapes
import Truth.Core

data QType t where
    QException :: QType String
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
    show (MkAny QException val) = "exception: " ++ val
    show (MkAny QLiteral val) = unpack val
    show (MkAny QUISpec val) = show val
    show (MkAny QList val) = "[" ++ intercalate "," (fmap show val) ++ "]"
    show (MkAny t _) = "<" ++ show t ++ ">"

qliteral :: Text -> QValue
qliteral = MkAny QLiteral

qfunction :: (QValue -> QValue) -> QValue
qfunction = MkAny QFunction

qexception :: String -> QValue
qexception = MkAny QException

qapply :: QValue -> QValue -> QValue
qapply (MkAny QException ex) _ = MkAny QException ex
qapply (MkAny QFunction f) a = f a
qapply (MkAny QMorphism f) (MkAny QPoint a) = MkAny QPoint $ applyPinaforeLens f a
qapply (MkAny QMorphism f) (MkAny QSet a) =
    MkAny QSet $
    readOnlyGeneralLens $
    convertGeneralFunction <.>
    applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a)
qapply (MkAny QInverseMorphism f) (MkAny QLiteral a) =
    MkAny QSet $ applyInversePinaforeLens (primitivePinaforeLensMorphism . f) $ constGeneralLens $ Just a
qapply (MkAny QInverseMorphism f) (MkAny QPoint a) = MkAny QSet $ applyInversePinaforeLens f a
qapply (MkAny QInverseMorphism f) (MkAny QSet a) =
    MkAny QSet $
    readOnlyGeneralLens $
    convertGeneralFunction <.>
    applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a)
qapply (MkAny tf _) (MkAny ta _) = qexception $ "cannot apply " ++ show tf ++ " to " ++ show ta

qinvert :: QValue -> QValue
qinvert (MkAny QException ex) = MkAny QException ex
qinvert (MkAny QMorphism m) = MkAny QInverseMorphism m
qinvert (MkAny QInverseMorphism m) = MkAny QMorphism m
qinvert (MkAny t _) = qexception $ "cannot invert " ++ show t

qcombine :: QValue -> QValue -> QValue
qcombine (MkAny QMorphism g) (MkAny QMorphism f) = MkAny QMorphism $ g . f
qcombine (MkAny QInverseMorphism g) (MkAny QInverseMorphism f) = MkAny QInverseMorphism $ g . f
qcombine g f = MkAny QFunction $ qapply g . qapply f

qpredicate :: Predicate -> QValue
qpredicate p = MkAny QMorphism $ predicatePinaforeLensMorphism p

qpoint :: Point -> QValue
qpoint p = MkAny QPoint $ constGeneralLens $ Just p

qmeet ::
       PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
qmeet a b = readOnlyGeneralLens meetGeneralFunction <.> pairJoinGeneralLenses a b

qjoin ::
       PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
    -> PinaforeLensValue (FiniteSetEdit Point)
qjoin a b = readOnlyGeneralLens joinGeneralFunction <.> pairJoinGeneralLenses a b

maybeToFiniteSet :: Maybe a -> FiniteSet a
maybeToFiniteSet (Just a) = opoint a
maybeToFiniteSet Nothing = mempty

qdisplay :: QValue -> PinaforeFunctionValue (FiniteSet Text)
qdisplay val =
    case fromQValue val of
        SuccessResult a -> a
        FailureResult _ -> constGeneralFunction $ opoint $ pack $ show val

badFromQValue :: QValue -> Result String t
badFromQValue (MkAny QException s) = fail s
badFromQValue (MkAny t _) = fail $ "unexpected " ++ show t

class FromQValue t where
    fromQValue :: QValue -> Result String t

instance FromQValue QValue where
    fromQValue = return

instance FromQValue Text where
    fromQValue (MkAny QLiteral v) = return v
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeLensValue (WholeEdit (Maybe Point))) where
    fromQValue (MkAny QPoint v) = return v
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeLensValue (WholeEdit (Maybe Text))) where
    fromQValue (MkAny QLiteral v) = return $ constGeneralLens $ Just v
    fromQValue (MkAny QPoint v) = return $ applyPinaforeLens primitivePinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeLensValue (WholeEdit (Maybe t))) => FromQValue (PinaforeFunctionValue (Maybe t)) where
    fromQValue v = do
        a :: PinaforeLensValue (WholeEdit (Maybe t)) <- fromQValue v
        return $ lensFunctionValue a

instance FromQValue (PinaforeLensValue (FiniteSetEdit Point)) where
    fromQValue (MkAny QPoint v) = return $ funcROGeneralLens maybeToFiniteSet <.> v
    fromQValue (MkAny QSet v) = return v
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeFunctionValue (FiniteSet Point)) where
    fromQValue (MkAny QPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue (MkAny QSet a) =
        return $ applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeFunctionValue (FiniteSet Text)) where
    fromQValue (MkAny QLiteral a) = return $ constGeneralFunction $ opoint a
    fromQValue (MkAny QPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction
                   (arr mms . cfmap (lensFunctionMorphism primitivePinaforeLensMorphism))
                   (lensFunctionValue a)
    fromQValue (MkAny QSet a) =
        return $
        applyPinaforeFunction
            (arr catMaybes . cfmap (lensFunctionMorphism primitivePinaforeLensMorphism))
            (lensFunctionValue a)
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeLensMorphism Point Point) where
    fromQValue (MkAny QMorphism v) = return v
    fromQValue v = badFromQValue v

instance FromQValue (PinaforeLensMorphism Point Text) where
    fromQValue v = do
        m <- fromQValue v
        return $ primitivePinaforeLensMorphism . m

instance FromQValue (PinaforeLensMorphism a b) => FromQValue (PinaforeFunctionMorphism a (Maybe b)) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m

instance FromQValue (UISpec PinaforeEdit) where
    fromQValue (MkAny QUISpec v) = return v
    fromQValue v = badFromQValue v

instance FromQValue t => FromQValue [t] where
    fromQValue (MkAny QList v) = for v fromQValue
    fromQValue v = badFromQValue v

instance (FromQValue a, FromQValue b) => FromQValue (a, b) where
    fromQValue (MkAny QList [va, vb]) = do
        a <- fromQValue va
        b <- fromQValue vb
        return (a, b)
    fromQValue v = badFromQValue v

instance FromQValue t => FromQValue (Result String t) where
    fromQValue v = fmap return $ fromQValue v

instance FromQValue t => FromQValue (IO t) where
    fromQValue v = fmap return $ fromQValue v

instance (ToQValue a, FromQValue b) => FromQValue (a -> Result String b) where
    fromQValue vf = return $ fromQValue . qapply vf . toQValue

class ToQValue t where
    toQValue :: t -> QValue

instance ToQValue QValue where
    toQValue = id

instance ToQValue t => ToQValue (Result String t) where
    toQValue (SuccessResult a) = toQValue a
    toQValue (FailureResult e) = qexception e

instance (FromQValue a, ToQValue b) => ToQValue (a -> b) where
    toQValue ab = qfunction $ toQValue . fmap ab . fromQValue

instance ToQValue Predicate where
    toQValue p = qpredicate p

instance ToQValue Point where
    toQValue p = qpoint p

instance ToQValue Text where
    toQValue p = qliteral p

instance ToQValue Bool where
    toQValue t = toQValue $ toText t

instance ToQValue Int where
    toQValue t = toQValue $ toText t

instance ToQValue Integer where
    toQValue t = toQValue $ toText t

instance ToQValue t => ToQValue [t] where
    toQValue t = MkAny QList $ fmap toQValue t

instance (ToQValue a, ToQValue b) => ToQValue (a, b) where
    toQValue (a, b) = toQValue [toQValue a, toQValue b]

instance ToQValue (PinaforeLensValue (WholeEdit (Maybe Point))) where
    toQValue t = MkAny QPoint t

instance ToQValue (PinaforeLensValue (FiniteSetEdit Point)) where
    toQValue t = MkAny QSet t

instance edit ~ PinaforeEdit => ToQValue (UISpec edit) where
    toQValue t = MkAny QUISpec t
