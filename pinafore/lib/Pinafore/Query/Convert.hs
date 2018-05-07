module Pinafore.Query.Convert
    ( HasQTypeDescription(..)
    , FromQValue(..)
    , ToQValue(..)
    ) where

import Pinafore.File
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Point
import Pinafore.PredicateMorphism
import Pinafore.Query.Lifted
import Pinafore.Query.Order
import Pinafore.Query.Types
import Pinafore.Query.Value
import Shapes
import Truth.Core
import Truth.World.ObjectStore

maybeToFiniteSet :: Maybe a -> FiniteSet a
maybeToFiniteSet (Just a) = opoint a
maybeToFiniteSet Nothing = mempty

badFromQValue ::
       forall baseedit t. HasQTypeDescription t
    => QValue baseedit
    -> Result Text t
badFromQValue (MkAny QTException s) = FailureResult s
badFromQValue (MkAny t _) = fail $ "unexpected " ++ show t ++ " for " ++ unpack (qTypeDescription @t)

class HasQTypeDescription t where
    qTypeDescription :: Text
    qTypeDescriptionSingle :: Text
    qTypeDescriptionSingle = qTypeDescription @t

class HasQTypeDescription t => FromQValue baseedit t where
    fromQValue :: QValue baseedit -> Result Text t

class HasQTypeDescription t => ToQValue baseedit t where
    toQValue :: t -> QValue baseedit

instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription t where
    qTypeDescription = literalTypeDescription @t <> "!"

instance {-# OVERLAPPABLE #-} AsLiteral t => FromQValue baseedit t where
    fromQValue v@(MkAny QTConstLiteral text) =
        case fromLiteral text of
            Just t -> return t
            Nothing -> badFromQValue v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} AsLiteral t => ToQValue baseedit t where
    toQValue t = qconstant $ toLiteral t

-- QValue
--
instance HasQTypeDescription (QValue baseedit) where
    qTypeDescription = "value"

instance edit ~ baseedit => FromQValue baseedit (QValue edit) where
    fromQValue = return

instance edit ~ baseedit => ToQValue baseedit (QValue edit) where
    toQValue = id

-- Lifted
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (Lifted edit t) where
    qTypeDescription = literalTypeDescription @t

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (Lifted edit t) where
    fromQValue v@(MkAny QTConstLiteral text) =
        case fromLiteral text of
            Just a -> return $ LiftedConstant a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTRefLiteral v) =
        return $ LiftedFunction $ funcEditFunction (\mtext -> mtext >>= fromLiteral) . editLensFunction v
    fromQValue (MkAny QTRefPoint v) =
        return $ LiftedFunction $ editLensFunction $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (Lifted edit t) where
    toQValue (LiftedConstant t) = toQValue t
    toQValue (LiftedFunction t) = toQValue t

-- Lifted Point
--
instance HasQTypeDescription (Lifted edit Point) where
    qTypeDescription = "point"

instance (edit ~ baseedit, HasPinaforePointEdit baseedit) => FromQValue baseedit (Lifted edit Point) where
    fromQValue v
        | SuccessResult (fp :: QPoint baseedit) <- fromQValue v = return $ LiftedFunction $ funcEditFunction Just . fp
    fromQValue v = badFromQValue v

-- Lifted FiniteSet
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (Lifted edit (FiniteSet t)) where
    qTypeDescription = literalTypeDescription @t <> "-set"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (Lifted edit (FiniteSet t)) where
    fromQValue v =
        case fromQValue v of
            SuccessResult l -> return $ LiftedConstant $ MkFiniteSet l
            FailureResult _ ->
                case fromQValue v of
                    SuccessResult (fs :: QSetLiteral edit t) -> return $ LiftedFunction $ funcEditFunction Just . fs
                    FailureResult _ -> badFromQValue v

-- Lifted FiniteSet Point
--
instance HasQTypeDescription (Lifted edit (FiniteSet Point)) where
    qTypeDescription = "set"

instance (edit ~ baseedit, HasPinaforePointEdit baseedit) => FromQValue baseedit (Lifted edit (FiniteSet Point)) where
    fromQValue v =
        case fromQValue v of
            SuccessResult (fs :: QSetLiteral edit Point) -> return $ LiftedFunction $ funcEditFunction Just . fs
            FailureResult _ -> badFromQValue v

-- Lifted IO
--
instance AsLiteral t => HasQTypeDescription (Lifted edit (IO t)) where
    qTypeDescription = literalTypeDescription @t

instance (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (Lifted edit (IO t)) where
    toQValue liot = toQValue $ ioLifted liot

-- Lifted Object ByteStringEdit
--
instance HasQTypeDescription (Lifted edit (Object ByteStringEdit)) where
    qTypeDescription = "file"

instance (edit ~ baseedit, HasPinaforePointEdit baseedit, HasPinaforeFileEdit baseedit) =>
             FromQValue baseedit (Lifted edit (Object ByteStringEdit)) where
    fromQValue v = do
        qip :: QPoint baseedit <- fromQValue v
        return $
            LiftedFunction $
            functionEditApply
                (functionLiftEditFunction singleObjectEditFunction . editLensFunction pinaforeFileLens)
                qip

-- QRefLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QRefLiteral edit t) where
    qTypeDescription = literalTypeDescription @t <> "*"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (QRefLiteral edit t) where
    fromQValue v@(MkAny QTConstLiteral text) =
        case fromLiteral text of
            Just a -> return $ constEditLens $ Just a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTRefLiteral v) = return $ (funcEditLens $ \mt -> mt >>= fromLiteral) . v
    fromQValue (MkAny QTRefPoint v) = return $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (QRefLiteral edit t) where
    toQValue t = MkAny QTRefLiteral $ (funcEditLens $ fmap toLiteral) . t

-- Predicate
--
instance HasQTypeDescription Predicate where
    qTypeDescription = "predicate"

instance HasPinaforePointEdit baseedit => ToQValue baseedit Predicate where
    toQValue p = qpredicate p

-- Point
--
instance HasQTypeDescription Point where
    qTypeDescription = "point"

instance ToQValue baseedit Point where
    toQValue p = qpoint p

-- QRefPoint
--
instance HasQTypeDescription (QRefPoint edit) where
    qTypeDescription = "point*"

instance edit ~ baseedit => FromQValue baseedit (QRefPoint edit) where
    fromQValue (MkAny QTRefPoint v) = return v
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QRefPoint edit) where
    toQValue t = MkAny QTRefPoint t

-- QRefSetPoint
--
instance HasQTypeDescription (QRefSetPoint edit) where
    qTypeDescription = "set*"

instance edit ~ baseedit => FromQValue baseedit (QRefSetPoint edit) where
    fromQValue (MkAny QTRefPoint v) = return $ (funcEditLens opoint) . v
    fromQValue (MkAny QTRefSet v) = return v
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QRefSetPoint edit) where
    toQValue t = MkAny QTRefSet t

-- QLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QLiteral edit t) where
    qTypeDescription = literalTypeDescription @t

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (QLiteral edit t) where
    fromQValue v = do
        cl :: Lifted baseedit t <- fromQValue v
        return $ liftedToFunction cl

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (QLiteral edit t) where
    toQValue ef = toQValue $ readOnlyEditLens ef

-- QPoint
--
instance HasQTypeDescription (QPoint edit) where
    qTypeDescription = "point"

instance (HasPinaforePointEdit baseedit, edit ~ baseedit) => FromQValue baseedit (QPoint edit) where
    fromQValue v
        | SuccessResult (a :: QRefPoint baseedit) <- fromQValue v = return $ lensFunctionValue a
    fromQValue v
        | SuccessResult (lit :: QLiteral baseedit Literal) <- fromQValue v =
            return $ applyPinaforeFunction literalPinaforeInverseFunctionMorphism lit
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QPoint edit) where
    toQValue ef = toQValue $ readOnlyEditLens ef

-- QSetLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QSetLiteral edit t) where
    qTypeDescription = literalTypeDescription @t <> "-set"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (QSetLiteral edit t) where
    fromQValue v@(MkAny QTConstLiteral text) =
        case fromLiteral text of
            Just a -> return $ constEditFunction $ opoint a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTRefLiteral a) =
        return $ (funcEditFunction $ maybePoint . (\mt -> mt >>= fromLiteral)) . editLensFunction a
    fromQValue (MkAny QTRefPoint a) =
        return $
        applyPinaforeFunction
            (arr maybeToFiniteSet . lensFunctionMorphism literalPinaforeLensMorphism)
            (lensFunctionValue a)
    fromQValue (MkAny QTRefSet a) =
        return $
        applyPinaforeFunction
            (arr catMaybes . cfmap (lensFunctionMorphism literalPinaforeLensMorphism))
            (lensFunctionValue a)
    fromQValue (MkAny QTList la) = do
        lmt <- for la $ fromQValue @baseedit
        return $ unWholeEditFunction $ fmap (MkFiniteSet . catMaybes) $ for lmt $ \mt -> MkWholeEditFunction mt
    fromQValue v = badFromQValue v

-- QSetPoint
--
instance HasQTypeDescription (QSetPoint edit) where
    qTypeDescription = "set"

instance (edit ~ baseedit, HasPinaforePointEdit baseedit) => FromQValue baseedit (QSetPoint edit) where
    fromQValue (MkAny QTRefPoint a) =
        return $ applyPinaforeFunction (arr opoint . lensFunctionMorphism id) (lensFunctionValue a)
    fromQValue (MkAny QTRefSet a) = return $ applyPinaforeFunction (lensFunctionMorphism id) (lensFunctionValue a)
    fromQValue (MkAny QTList la) = do
        lmt :: [QPoint baseedit] <- for la $ fromQValue @baseedit
        return $ unWholeEditFunction $ fmap MkFiniteSet $ for lmt $ \mt -> MkWholeEditFunction mt
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QSetPoint edit) where
    toQValue ef = toQValue @_ @(QRefSetPoint baseedit) $ readOnlyEditLens $ convertEditFunction . ef

-- QMorphismRefLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QMorphismRefLiteral edit t) where
    qTypeDescription = "point* ~> " <> literalTypeDescription @t <> "*"
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QMorphismRefLiteral edit t) <> ")"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (QMorphismRefLiteral edit t) where
    fromQValue v = do
        m <- fromQValue v
        return $ literalPinaforeLensMorphism . m

-- QMorphismRefPoint
--
instance HasQTypeDescription (QMorphismRefPoint edit) where
    qTypeDescription = "point* ~> point*"
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QMorphismRefPoint edit) <> ")"

instance edit ~ baseedit => FromQValue baseedit (QMorphismRefPoint edit) where
    fromQValue (MkAny QTMorphism v) = return v
    fromQValue v = badFromQValue v

-- QMorphismLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QMorphismLiteral edit t) where
    qTypeDescription = "point ~> " <> literalTypeDescription @t
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QMorphismLiteral edit t) <> ")"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforePointEdit baseedit) =>
                                  FromQValue baseedit (QMorphismLiteral edit t) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m

-- QMorphismPoint
--
instance HasQTypeDescription (QMorphismPoint edit) where
    qTypeDescription = "point ~> point"
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QMorphismPoint edit) <> ")"

instance edit ~ baseedit => FromQValue baseedit (QMorphismPoint edit) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m

-- QAction
--
instance HasQTypeDescription (QAction edit) where
    qTypeDescription = "action"

instance baseedit ~ edit => FromQValue baseedit (QAction edit) where
    fromQValue (MkAny QTAction v) = return v
    fromQValue v = badFromQValue v

instance baseedit ~ edit => ToQValue baseedit (QAction edit) where
    toQValue t = MkAny QTAction t

-- IO
--
instance HasQTypeDescription (IO ()) where
    qTypeDescription = "action"

instance ToQValue baseedit (IO ()) where
    toQValue t = toQValue $ (liftIO t :: QAction baseedit)

-- View
--
instance HasQTypeDescription (View edit ()) where
    qTypeDescription = "action"

instance baseedit ~ edit => FromQValue baseedit (View edit ()) where
    fromQValue (MkAny QTAction (MkComposeM v)) =
        return $ do
            _ <- v -- ignore failure
            return ()
    fromQValue v = badFromQValue v

instance baseedit ~ edit => ToQValue baseedit (View edit ()) where
    toQValue t = MkAny QTAction $ lift t

-- QOrder
--
instance HasQTypeDescription (QOrder edit) where
    qTypeDescription = "order"

instance baseedit ~ edit => FromQValue baseedit (QOrder edit) where
    fromQValue (MkAny QTOrder v) = return v
    fromQValue v = badFromQValue v

instance baseedit ~ edit => ToQValue baseedit (QOrder edit) where
    toQValue t = MkAny QTOrder t

-- UISpec
--
instance HasQTypeDescription (UISpec edit) where
    qTypeDescription = "ui"

instance baseedit ~ edit => FromQValue baseedit (UISpec edit) where
    fromQValue (MkAny QTUserInterface v) = return v
    fromQValue v = badFromQValue v

instance baseedit ~ edit => ToQValue baseedit (UISpec edit) where
    toQValue t = MkAny QTUserInterface t

-- UISpec function
--
instance HasQTypeDescription (PinaforeFunctionValue edit (UISpec edit)) where
    qTypeDescription = "ui"

instance edit ~ baseedit => ToQValue baseedit (PinaforeFunctionValue edit (UISpec edit)) where
    toQValue ef = MkAny QTUserInterface $ uiSwitch ef

-- UIWindow
--
instance HasQTypeDescription (UIWindow edit) where
    qTypeDescription = qTypeDescription @(EditFunction edit (WholeEdit (Maybe Text)), UISpec edit)

instance (edit ~ baseedit, HasPinaforePointEdit baseedit) => FromQValue baseedit (UIWindow edit) where
    fromQValue v = do
        (title, content) <- fromQValue v
        return $ MkUIWindow (funcEditFunction @(WholeEdit (Maybe Text)) (fromMaybe "") . title) content

-- List
--
instance HasQTypeDescription t => HasQTypeDescription [t] where
    qTypeDescription = "[" <> qTypeDescription @t <> "]"

instance FromQValue baseedit t => FromQValue baseedit [t] where
    fromQValue (MkAny QTList v) = for v fromQValue
    fromQValue v = badFromQValue v

instance ToQValue baseedit t => ToQValue baseedit [t] where
    toQValue t = MkAny QTList $ fmap toQValue t

-- Pair
--
instance (HasQTypeDescription a, HasQTypeDescription b) => HasQTypeDescription (a, b) where
    qTypeDescription = "(" <> qTypeDescription @a <> ", " <> qTypeDescription @b <> ")"

instance (FromQValue baseedit a, FromQValue baseedit b) => FromQValue baseedit (a, b) where
    fromQValue (MkAny QTList [va, vb]) = do
        a <- fromQValue va
        b <- fromQValue vb
        return (a, b)
    fromQValue v = badFromQValue v

instance (ToQValue baseedit a, ToQValue baseedit b, HasPinaforePointEdit baseedit) => ToQValue baseedit (a, b) where
    toQValue (a, b) = toQValue @baseedit [toQValue @baseedit a, toQValue @baseedit b]

-- Result
--
instance HasQTypeDescription t => HasQTypeDescription (Result Text t) where
    qTypeDescription = "result " <> qTypeDescription @t

instance FromQValue baseedit t => FromQValue baseedit (Result Text t) where
    fromQValue v = fmap return $ fromQValue v

instance ToQValue baseedit t => ToQValue baseedit (Result Text t) where
    toQValue (SuccessResult a) = toQValue a
    toQValue (FailureResult e) = qexception e

-- Function
--
instance {-# OVERLAPPABLE #-} (HasQTypeDescription a, HasQTypeDescription b) => HasQTypeDescription (a -> b) where
    qTypeDescription = qTypeDescriptionSingle @a <> " -> " <> qTypeDescription @b
    qTypeDescriptionSingle = "(" <> qTypeDescription @(a -> b) <> ")"

instance (HasQTypeDescription a, HasQTypeDescription b) => HasQTypeDescription (a -> Result Text b) where
    qTypeDescription = qTypeDescriptionSingle @a <> " -> " <> qTypeDescription @b
    qTypeDescriptionSingle = "(" <> qTypeDescription @(a -> Result Text b) <> ")"

instance (ToQValue baseedit a, FromQValue baseedit b, HasPinaforePointEdit baseedit) =>
             FromQValue baseedit (a -> Result Text b) where
    fromQValue vf = do
        f <- qpartialapply vf
        return $ fromQValue . f . toQValue

instance (HasPinaforePointEdit baseedit, ToQValue baseedit a, edit ~ baseedit) =>
             FromQValue baseedit (a -> QAction edit) where
    fromQValue vf = do
        f <- qpartialapply vf
        return $ \a -> do
            action <- liftInner $ fromQValue $ f $ toQValue a
            action

instance (FromQValue baseedit a, ToQValue baseedit b) => ToQValue baseedit (a -> b) where
    toQValue ab = qfunction $ toQValue . fmap ab . fromQValue
