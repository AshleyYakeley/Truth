module Pinafore.Query.Value where

import Data.List (zipWith)
import Pinafore.AsText
import Pinafore.Morphism
import Pinafore.Table
import Shapes
import Truth.Core

data QType baseedit t where
    QException :: QType baseedit Text
    QConstant :: QType baseedit Text
    QLiteral :: QType baseedit (PinaforeLensValue baseedit (WholeEdit (Maybe Text)))
    QPoint :: QType baseedit (PinaforeLensValue baseedit (WholeEdit (Maybe Point)))
    QSet :: QType baseedit (PinaforeLensValue baseedit (FiniteSetEdit Point))
    QMorphism :: QType baseedit (PinaforeLensMorphism baseedit Point Point)
    QInverseMorphism :: QType baseedit (PinaforeLensMorphism baseedit Point Point)
    QList :: QType baseedit [QValue baseedit]
    QFunction :: QType baseedit (QValue baseedit -> QValue baseedit)
    QUISpec :: QType baseedit (UISpec baseedit)

instance Show (QType baseedit t) where
    show QException = "exception"
    show QConstant = "constant"
    show QLiteral = "literal"
    show QPoint = "point"
    show QSet = "set"
    show QMorphism = "morphism"
    show QInverseMorphism = "inverse morphism"
    show QList = "list"
    show QFunction = "function"
    show QUISpec = "user interface"

instance TestEquality (QType baseedit) where
    testEquality QException QException = Just Refl
    testEquality QConstant QConstant = Just Refl
    testEquality QLiteral QLiteral = Just Refl
    testEquality QPoint QPoint = Just Refl
    testEquality QSet QSet = Just Refl
    testEquality QMorphism QMorphism = Just Refl
    testEquality QInverseMorphism QInverseMorphism = Just Refl
    testEquality QList QList = Just Refl
    testEquality QFunction QFunction = Just Refl
    testEquality QUISpec QUISpec = Just Refl
    testEquality _ _ = Nothing

type QValue baseedit = Any (QType baseedit)

instance Show (QValue baseedit) where
    show (MkAny QException val) = unpack $ "exception: " <> val
    show (MkAny QConstant val) = unpack val
    show (MkAny QUISpec val) = show val
    show (MkAny QList val) = "[" ++ intercalate "," (fmap show val) ++ "]"
    show (MkAny t _) = "<" ++ show t ++ ">"

qconstant :: Text -> QValue baseedit
qconstant = MkAny QConstant

qfunction :: (QValue baseedit -> QValue baseedit) -> QValue baseedit
qfunction = MkAny QFunction

qexception :: Text -> QValue baseedit
qexception = MkAny QException

qpartialapply :: HasPinaforeTableEdit baseedit => QValue baseedit -> Result Text (QValue baseedit -> QValue baseedit)
qpartialapply (MkAny QException ex) = FailureResult ex
qpartialapply (MkAny QFunction f) = return f
qpartialapply (MkAny QMorphism f) =
    return $ \case
        MkAny QPoint a -> MkAny QPoint $ applyPinaforeLens f a
        MkAny QSet a ->
            MkAny QSet $
            readOnlyEditLens $
            convertEditFunction .
            applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a)
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QMorphism ++ " to " ++ show ta
qpartialapply (MkAny QInverseMorphism f) =
    return $ \case
        MkAny QConstant a ->
            MkAny QSet $ applyInversePinaforeLens (literalPinaforeLensMorphism . f) $ constEditLens $ Just a
        MkAny QLiteral a -> MkAny QSet $ applyInversePinaforeLens (literalPinaforeLensMorphism . f) a
        MkAny QPoint a -> MkAny QSet $ applyInversePinaforeLens f a
        MkAny QSet a ->
            MkAny QSet $
            readOnlyEditLens $
            convertEditFunction .
            applyPinaforeFunction
                (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f))
                (lensFunctionValue a)
        MkAny ta _ -> qexception $ pack $ "cannot apply " ++ show QInverseMorphism ++ " to " ++ show ta
qpartialapply (MkAny tf _) = FailureResult $ pack $ "cannot apply " ++ show tf

qapply :: HasPinaforeTableEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qapply vf va =
    case qpartialapply vf of
        SuccessResult f -> f va
        FailureResult ex -> MkAny QException ex

qinvert :: QValue baseedit -> QValue baseedit
qinvert (MkAny QException ex) = MkAny QException ex
qinvert (MkAny QMorphism m) = MkAny QInverseMorphism m
qinvert (MkAny QInverseMorphism m) = MkAny QMorphism m
qinvert (MkAny t _) = qexception $ pack $ "cannot invert " ++ show t

qcombine :: HasPinaforeTableEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qcombine (MkAny QMorphism g) (MkAny QMorphism f) = MkAny QMorphism $ g . f
qcombine (MkAny QInverseMorphism g) (MkAny QInverseMorphism f) = MkAny QInverseMorphism $ g . f
qcombine g f = MkAny QFunction $ qapply g . qapply f

qpredicate :: HasPinaforeTableEdit baseedit => Predicate -> QValue baseedit
qpredicate p = MkAny QMorphism $ predicatePinaforeLensMorphism p

qpoint :: Point -> QValue baseedit
qpoint p = MkAny QPoint $ constEditLens $ Just p

qmeet ::
       PinaforeLensValue baseedit (FiniteSetEdit Point)
    -> PinaforeLensValue baseedit (FiniteSetEdit Point)
    -> PinaforeLensValue baseedit (FiniteSetEdit Point)
qmeet a b = readOnlyEditLens meetEditFunction . pairJoinEditLenses a b

qjoin ::
       PinaforeLensValue baseedit (FiniteSetEdit Point)
    -> PinaforeLensValue baseedit (FiniteSetEdit Point)
    -> PinaforeLensValue baseedit (FiniteSetEdit Point)
qjoin a b = readOnlyEditLens joinEditFunction . pairJoinEditLenses a b

maybeToFiniteSet :: Maybe a -> FiniteSet a
maybeToFiniteSet (Just a) = opoint a
maybeToFiniteSet Nothing = mempty

qdisplay :: HasPinaforeTableEdit baseedit => QValue baseedit -> PinaforeFunctionValue baseedit (FiniteSet Text)
qdisplay val =
    case fromQValue val of
        SuccessResult a -> a
        FailureResult _ -> constEditFunction $ opoint $ pack $ show val

badFromQValue :: QValue baseedit -> Result Text t
badFromQValue (MkAny QException s) = FailureResult s
badFromQValue (MkAny t _) = fail $ "unexpected " ++ show t

data Literal baseedit t
    = LiteralConstant t
    | LiteralFunction (PinaforeFunctionValue baseedit (Maybe t))

instance Functor (Literal baseedit) where
    fmap ab (LiteralConstant a) = LiteralConstant $ ab a
    fmap ab (LiteralFunction a) = LiteralFunction $ funcEditFunction (fmap ab) . a

instance Applicative (Literal baseedit) where
    pure = LiteralConstant
    LiteralConstant ab <*> LiteralConstant a = LiteralConstant $ ab a
    lab <*> la = let
        fab = literalToFunction lab
        fa = literalToFunction la
        in LiteralFunction $ funcEditFunction (\(mab, ma) -> mab <*> ma) . pairWholeEditFunction fab fa

literalToFunction :: Literal baseedit t -> PinaforeFunctionValue baseedit (Maybe t)
literalToFunction (LiteralConstant t) = constEditFunction $ Just t
literalToFunction (LiteralFunction t) = t

qappend :: Literal baseedit Text -> Literal baseedit Text -> Literal baseedit Text
qappend = liftA2 (<>)

fvalIfThenElse ::
       forall baseedit t.
       t
    -> PinaforeFunctionValue baseedit (Maybe Bool)
    -> PinaforeFunctionValue baseedit t
    -> PinaforeFunctionValue baseedit t
    -> PinaforeFunctionValue baseedit t
fvalIfThenElse vdef vi vt ve = let
    mIfThenElse :: (Maybe Bool, (t, t)) -> t
    mIfThenElse (Just True, (v, _)) = v
    mIfThenElse (Just False, (_, v)) = v
    mIfThenElse (Nothing, _) = vdef
    in funcEditFunction mIfThenElse . (pairJoinEditFunctions vi $ pairJoinEditFunctions vt ve)

qfifthenelse ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => PinaforeFunctionValue baseedit (Maybe Bool)
    -> QValue baseedit
    -> QValue baseedit
    -> QValue baseedit
qfifthenelse _ v@(MkAny QException _) _ = v
qfifthenelse _ _ v@(MkAny QException _) = v
qfifthenelse f (MkAny QList lt) (MkAny QList le) =
    if length lt == length le
        then MkAny QList $ zipWith (qfifthenelse f) lt le
        else qexception $ pack $ "cannot match lists of lengths " ++ show (length lt) ++ " and " ++ show (length le)
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(PinaforeFunctionValue baseedit (Maybe Text)) t
    , SuccessResult ve <- fromQValue @baseedit @(PinaforeFunctionValue baseedit (Maybe Text)) e =
        toQValue $ fvalIfThenElse Nothing f vt ve
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(PinaforeFunctionValue baseedit (Maybe Point)) t
    , SuccessResult ve <- fromQValue @baseedit @(PinaforeFunctionValue baseedit (Maybe Point)) e =
        toQValue $ fvalIfThenElse Nothing f vt ve
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(PinaforeFunctionValue baseedit (FiniteSet Point)) t
    , SuccessResult ve <- fromQValue @baseedit @(PinaforeFunctionValue baseedit (FiniteSet Point)) e =
        toQValue $ fvalIfThenElse mempty f vt ve
-- possibly add cases for QMorphism and QInverseMorphism?
qfifthenelse f t e
    | SuccessResult vt <- qpartialapply t
    , SuccessResult ve <- qpartialapply e = MkAny QFunction $ \a -> qfifthenelse f (vt a) (ve a)
qfifthenelse f (MkAny QUISpec vt) (MkAny QUISpec ve) = let
    pickUISpec :: Maybe Bool -> UISpec baseedit
    pickUISpec Nothing = uiNull
    pickUISpec (Just True) = vt
    pickUISpec (Just False) = ve
    in toQValue $ wholeEditFunction pickUISpec . f
qfifthenelse _ (MkAny tt _) (MkAny te _) = qexception $ pack $ "cannot match " ++ show tt ++ " and " ++ show te

qifthenelse ::
       HasPinaforeTableEdit baseedit => Literal baseedit Bool -> QValue baseedit -> QValue baseedit -> QValue baseedit
qifthenelse (LiteralConstant True) vt _ = vt
qifthenelse (LiteralConstant False) _ ve = ve
qifthenelse (LiteralFunction f) vt ve = qfifthenelse f vt ve

class FromQValue baseedit t where
    fromQValue :: QValue baseedit -> Result Text t
    qTypeDescriptionFrom :: Text
    qTypeDescriptionFromSingle :: Text
    qTypeDescriptionFromSingle = qTypeDescriptionFrom @baseedit @t

instance {-# OVERLAPPABLE #-} AsText t => FromQValue baseedit t where
    fromQValue v@(MkAny QConstant text) =
        case fromText text of
            Just t -> return t
            Nothing -> badFromQValue v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = (textTypeDescription @t) <> "-constant"

instance FromQValue baseedit (QValue baseedit) where
    fromQValue = return
    qTypeDescriptionFrom = "value"

instance (AsText t, HasPinaforeTableEdit baseedit) => FromQValue baseedit (Literal baseedit t) where
    fromQValue v@(MkAny QConstant text) =
        case fromText text of
            Just a -> return $ LiteralConstant a
            Nothing -> badFromQValue v
    fromQValue (MkAny QLiteral v) =
        return $ LiteralFunction $ funcEditFunction (\mtext -> mtext >>= fromText) . editLensFunction v
    fromQValue (MkAny QPoint v) =
        return $ LiteralFunction $ editLensFunction $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = textTypeDescription @t

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (PinaforeLensValue baseedit (WholeEdit (Maybe t))) where
    fromQValue v@(MkAny QConstant text) =
        case fromText text of
            Just a -> return $ constEditLens $ Just a
            Nothing -> badFromQValue v
    fromQValue (MkAny QLiteral v) = return $ (funcEditLens $ \mt -> mt >>= fromText) . v
    fromQValue (MkAny QPoint v) = return $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = textTypeDescription @t

instance FromQValue baseedit (PinaforeLensValue baseedit (WholeEdit (Maybe Point))) where
    fromQValue (MkAny QPoint v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "point"

instance FromQValue baseedit (PinaforeLensValue baseedit (FiniteSetEdit Point)) where
    fromQValue (MkAny QPoint v) = return $ (funcEditLens maybeToFiniteSet) . v
    fromQValue (MkAny QSet v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "set"

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (PinaforeFunctionValue baseedit (Maybe t)) where
    fromQValue v = do
        cl :: Literal baseedit t <- fromQValue v
        return $ literalToFunction cl
    qTypeDescriptionFrom = textTypeDescription @t

instance FromQValue baseedit (PinaforeFunctionValue baseedit (Maybe Point)) where
    fromQValue v = do
        a :: PinaforeLensValue baseedit (WholeEdit (Maybe Point)) <- fromQValue v
        return $ lensFunctionValue a
    qTypeDescriptionFrom = qTypeDescriptionFrom @baseedit @(PinaforeLensValue baseedit (WholeEdit (Maybe Point)))

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (PinaforeFunctionValue baseedit (FiniteSet t)) where
    fromQValue v@(MkAny QConstant text) =
        case fromText text of
            Just a -> return $ constEditFunction $ opoint a
            Nothing -> badFromQValue v
    fromQValue (MkAny QLiteral a) =
        return $ (funcEditFunction $ maybePoint . (\mt -> mt >>= fromText)) . editLensFunction a
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

instance FromQValue baseedit (PinaforeFunctionValue baseedit (FiniteSet Point)) where
    fromQValue (MkAny QPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue (MkAny QSet a) =
        return $ applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "point"

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (PinaforeLensMorphism baseedit Point t) where
    fromQValue v = do
        m <- fromQValue v
        return $ literalPinaforeLensMorphism . m
    qTypeDescriptionFrom = "text-morphism"

instance FromQValue baseedit (PinaforeLensMorphism baseedit Point Point) where
    fromQValue (MkAny QMorphism v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "point-morphism"

instance FromQValue baseedit (PinaforeLensMorphism baseedit a b) =>
         FromQValue baseedit (PinaforeFunctionMorphism baseedit a (Maybe b)) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m
    qTypeDescriptionFrom = qTypeDescriptionFrom @baseedit @(PinaforeLensMorphism baseedit a b)

instance FromQValue baseedit (UISpec baseedit) where
    fromQValue (MkAny QUISpec v) = return v
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "uispec"

instance HasPinaforeTableEdit baseedit => FromQValue baseedit (UIWindow baseedit) where
    fromQValue v = do
        (title, content) <- fromQValue v
        return $ MkUIWindow (funcEditFunction @(WholeEdit (Maybe Text)) (fromMaybe "") . title) content
    qTypeDescriptionFrom =
        qTypeDescriptionFrom @baseedit @(EditFunction baseedit (WholeEdit (Maybe Text)), UISpec baseedit)

instance FromQValue baseedit t => FromQValue baseedit [t] where
    fromQValue (MkAny QList v) = for v fromQValue
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "[" <> qTypeDescriptionFrom @baseedit @t <> "]"

instance (FromQValue baseedit a, FromQValue baseedit b) => FromQValue baseedit (a, b) where
    fromQValue (MkAny QList [va, vb]) = do
        a <- fromQValue va
        b <- fromQValue vb
        return (a, b)
    fromQValue v = badFromQValue v
    qTypeDescriptionFrom = "(" <> qTypeDescriptionFrom @baseedit @a <> ", " <> qTypeDescriptionFrom @baseedit @b <> ")"

instance FromQValue baseedit t => FromQValue baseedit (Result Text t) where
    fromQValue v = fmap return $ fromQValue v
    qTypeDescriptionFrom = "result " <> qTypeDescriptionFrom @baseedit @t

instance FromQValue baseedit t => FromQValue baseedit (IO t) where
    fromQValue v = fmap return $ fromQValue v
    qTypeDescriptionFrom = "action " <> qTypeDescriptionFrom @baseedit @t

instance (ToQValue baseedit a, FromQValue baseedit b, HasPinaforeTableEdit baseedit) =>
         FromQValue baseedit (a -> Result Text b) where
    fromQValue vf = do
        f <- qpartialapply vf
        return $ fromQValue . f . toQValue
    qTypeDescriptionFrom = qTypeDescriptionToSingle @baseedit @a <> " -> " <> qTypeDescriptionFrom @baseedit @b
    qTypeDescriptionFromSingle = "(" <> qTypeDescriptionFrom @baseedit @(a -> Result Text b) <> ")"

class ToQValue baseedit t where
    toQValue :: t -> QValue baseedit
    qTypeDescriptionTo :: Text
    qTypeDescriptionToSingle :: Text
    qTypeDescriptionToSingle = qTypeDescriptionTo @baseedit @t

instance {-# OVERLAPPABLE #-} AsText t => ToQValue baseedit t where
    toQValue t = qconstant $ toText t
    qTypeDescriptionTo = (textTypeDescription @t) <> "-constant"

instance ToQValue baseedit (QValue baseedit) where
    toQValue = id
    qTypeDescriptionTo = "value"

instance ToQValue baseedit t => ToQValue baseedit (Result Text t) where
    toQValue (SuccessResult a) = toQValue a
    toQValue (FailureResult e) = qexception e
    qTypeDescriptionTo = "result " <> qTypeDescriptionTo @baseedit @t

instance (FromQValue baseedit a, ToQValue baseedit b) => ToQValue baseedit (a -> b) where
    toQValue ab = qfunction $ toQValue . fmap ab . fromQValue
    qTypeDescriptionTo = qTypeDescriptionFromSingle @baseedit @a <> " -> " <> qTypeDescriptionTo @baseedit @b
    qTypeDescriptionToSingle = "(" <> qTypeDescriptionTo @baseedit @(a -> b) <> ")"

instance HasPinaforeTableEdit baseedit => ToQValue baseedit Predicate where
    toQValue p = qpredicate p
    qTypeDescriptionTo = "predicate"

instance ToQValue baseedit Point where
    toQValue p = qpoint p
    qTypeDescriptionTo = "point"

instance AsText t => ToQValue baseedit (Literal baseedit t) where
    toQValue (LiteralConstant t) = toQValue t
    toQValue (LiteralFunction t) = toQValue t
    qTypeDescriptionTo = textTypeDescription @t

instance ToQValue baseedit t => ToQValue baseedit [t] where
    toQValue t = MkAny QList $ fmap toQValue t
    qTypeDescriptionTo = "[" <> qTypeDescriptionTo @baseedit @t <> "]"

instance (ToQValue baseedit a, ToQValue baseedit b, HasPinaforeTableEdit baseedit) => ToQValue baseedit (a, b) where
    toQValue (a, b) = toQValue @baseedit [toQValue @baseedit a, toQValue @baseedit b]
    qTypeDescriptionTo = "(" <> qTypeDescriptionTo @baseedit @a <> ", " <> qTypeDescriptionTo @baseedit @b <> ")"

instance {-# OVERLAPPABLE #-} AsText t => ToQValue baseedit (PinaforeLensValue baseedit (WholeEdit (Maybe t))) where
    toQValue t = MkAny QLiteral $ (funcEditLens $ fmap toText) . t
    qTypeDescriptionTo = textTypeDescription @t

instance ToQValue baseedit (PinaforeLensValue baseedit (WholeEdit (Maybe Point))) where
    toQValue t = MkAny QPoint t
    qTypeDescriptionTo = "point"

instance ToQValue baseedit (PinaforeLensValue baseedit (FiniteSetEdit Point)) where
    toQValue t = MkAny QSet t
    qTypeDescriptionTo = "set"

instance {-# OVERLAPPABLE #-} ToQValue baseedit (PinaforeLensValue baseedit (WholeEdit t)) =>
                              ToQValue baseedit (PinaforeFunctionValue baseedit t) where
    toQValue ef = toQValue $ readOnlyEditLens ef
    qTypeDescriptionTo = "immutable " <> qTypeDescriptionTo @baseedit @(PinaforeLensValue baseedit (WholeEdit t))

instance ToQValue baseedit (PinaforeFunctionValue baseedit (FiniteSet Point)) where
    toQValue ef =
        toQValue @_ @(PinaforeLensValue baseedit (FiniteSetEdit Point)) $ readOnlyEditLens $ convertEditFunction . ef
    qTypeDescriptionTo = "immutable set"

instance ToQValue baseedit (PinaforeFunctionValue baseedit (UISpec baseedit)) where
    toQValue ef = MkAny QUISpec $ uiSwitch ef
    qTypeDescriptionTo = "uispec"

instance edit ~ baseedit => ToQValue baseedit (UISpec edit) where
    toQValue t = MkAny QUISpec t
    qTypeDescriptionTo = "uispec"
