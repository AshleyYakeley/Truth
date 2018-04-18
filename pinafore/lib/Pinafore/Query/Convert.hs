module Pinafore.Query.Convert
    ( HasQTypeDescription(..)
    , FromQValue(..)
    , ToQValue(..)
    , qifthenelse
    ) where

import Data.List (zipWith)
import Pinafore.File
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Query.Lifted
import Pinafore.Query.Order
import Pinafore.Query.Types
import Pinafore.Query.Value
import Pinafore.Table
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
    fromQValue v@(MkAny QTConstant text) =
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

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (Lifted edit t) where
    fromQValue v@(MkAny QTConstant text) =
        case fromLiteral text of
            Just a -> return $ LiftedConstant a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTLiteral v) =
        return $ LiftedFunction $ funcEditFunction (\mtext -> mtext >>= fromLiteral) . editLensFunction v
    fromQValue (MkAny QTPoint v) =
        return $ LiftedFunction $ editLensFunction $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (Lifted edit t) where
    toQValue (LiftedConstant t) = toQValue t
    toQValue (LiftedFunction t) = toQValue t

-- Lifted Point
--
instance HasQTypeDescription (Lifted edit Point) where
    qTypeDescription = "point"

instance (edit ~ baseedit, HasPinaforeTableEdit baseedit) => FromQValue baseedit (Lifted edit Point) where
    fromQValue v =
        case fromQValue v of
            SuccessResult fp -> return $ LiftedFunction fp
            FailureResult _ -> badFromQValue v

-- Lifted FiniteSet
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (Lifted edit (FiniteSet t)) where
    qTypeDescription = literalTypeDescription @t <> "-set"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (Lifted edit (FiniteSet t)) where
    fromQValue v =
        case fromQValue v of
            SuccessResult l -> return $ LiftedConstant $ MkFiniteSet l
            FailureResult _ ->
                case fromQValue v of
                    SuccessResult (fs :: QImLiteralSet edit t) -> return $ LiftedFunction $ funcEditFunction Just . fs
                    FailureResult _ -> badFromQValue v

-- Lifted FiniteSet Point
--
instance HasQTypeDescription (Lifted edit (FiniteSet Point)) where
    qTypeDescription = "set"

instance (edit ~ baseedit, HasPinaforeTableEdit baseedit) => FromQValue baseedit (Lifted edit (FiniteSet Point)) where
    fromQValue v =
        case fromQValue v of
            SuccessResult (fs :: QImLiteralSet edit Point) -> return $ LiftedFunction $ funcEditFunction Just . fs
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

instance (edit ~ baseedit, HasPinaforeFileEdit baseedit) => FromQValue baseedit (Lifted edit (Object ByteStringEdit)) where
    fromQValue v = do
        qip :: QImPoint baseedit <- fromQValue v
        return $
            LiftedFunction $
            functionEditApply
                (functionLiftEditFunction singleObjectEditFunction .
                 (functionEditMaybe (immutableEditFunction nullSingleObjectMutableRead) $
                  editLensFunction pinaforeFileLens))
                qip

-- QLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QLiteral edit t) where
    qTypeDescription = literalTypeDescription @t <> "*"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (QLiteral edit t) where
    fromQValue v@(MkAny QTConstant text) =
        case fromLiteral text of
            Just a -> return $ constEditLens $ Just a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTLiteral v) = return $ (funcEditLens $ \mt -> mt >>= fromLiteral) . v
    fromQValue (MkAny QTPoint v) = return $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (QLiteral edit t) where
    toQValue t = MkAny QTLiteral $ (funcEditLens $ fmap toLiteral) . t

-- Predicate
--
instance HasQTypeDescription Predicate where
    qTypeDescription = "predicate"

instance HasPinaforeTableEdit baseedit => ToQValue baseedit Predicate where
    toQValue p = qpredicate p

-- Point
--
instance HasQTypeDescription Point where
    qTypeDescription = "point"

instance ToQValue baseedit Point where
    toQValue p = qpoint p

-- QPoint
--
instance HasQTypeDescription (QPoint edit) where
    qTypeDescription = "point*"

instance edit ~ baseedit => FromQValue baseedit (QPoint edit) where
    fromQValue (MkAny QTPoint v) = return v
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QPoint edit) where
    toQValue t = MkAny QTPoint t

-- QSet
--
instance HasQTypeDescription (QSet edit) where
    qTypeDescription = "set*"

instance edit ~ baseedit => FromQValue baseedit (QSet edit) where
    fromQValue (MkAny QTPoint v) = return $ (funcEditLens maybeToFiniteSet) . v
    fromQValue (MkAny QTSet v) = return v
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QSet edit) where
    toQValue t = MkAny QTSet t

-- QImLiteral
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QImLiteral edit t) where
    qTypeDescription = literalTypeDescription @t

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (QImLiteral edit t) where
    fromQValue v = do
        cl :: Lifted baseedit t <- fromQValue v
        return $ liftedToFunction cl

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t) => ToQValue baseedit (QImLiteral edit t) where
    toQValue ef = toQValue $ readOnlyEditLens ef

-- QImPoint
--
instance HasQTypeDescription (QImPoint edit) where
    qTypeDescription = "point"

instance edit ~ baseedit => FromQValue baseedit (QImPoint edit) where
    fromQValue v = do
        a :: QPoint baseedit <- fromQValue v
        return $ lensFunctionValue a

instance edit ~ baseedit => ToQValue baseedit (QImPoint edit) where
    toQValue ef = toQValue $ readOnlyEditLens ef

-- QImLiteralSet
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QImLiteralSet edit t) where
    qTypeDescription = literalTypeDescription @t <> "-set"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (QImLiteralSet edit t) where
    fromQValue v@(MkAny QTConstant text) =
        case fromLiteral text of
            Just a -> return $ constEditFunction $ opoint a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTLiteral a) =
        return $ (funcEditFunction $ maybePoint . (\mt -> mt >>= fromLiteral)) . editLensFunction a
    fromQValue (MkAny QTPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction
                   (arr mms . cfmap (lensFunctionMorphism literalPinaforeLensMorphism))
                   (lensFunctionValue a)
    fromQValue (MkAny QTSet a) =
        return $
        applyPinaforeFunction
            (arr catMaybes . cfmap (lensFunctionMorphism literalPinaforeLensMorphism))
            (lensFunctionValue a)
    fromQValue (MkAny QTList la) = do
        lmt <- for la $ fromQValue @baseedit
        return $ unWholeEditFunction $ fmap (MkFiniteSet . catMaybes) $ for lmt $ \mt -> MkWholeEditFunction mt
    fromQValue v = badFromQValue v

-- QImSet
--
instance HasQTypeDescription (QImSet edit) where
    qTypeDescription = "set"

instance edit ~ baseedit => FromQValue baseedit (QImSet edit) where
    fromQValue (MkAny QTPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue (MkAny QTSet a) =
        return $ applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue (MkAny QTList la) = do
        lmt <- for la $ fromQValue @baseedit
        return $ unWholeEditFunction $ fmap (MkFiniteSet . catMaybes) $ for lmt $ \mt -> MkWholeEditFunction mt
    fromQValue v = badFromQValue v

instance edit ~ baseedit => ToQValue baseedit (QImSet edit) where
    toQValue ef = toQValue @_ @(QSet baseedit) $ readOnlyEditLens $ convertEditFunction . ef

-- QLiteralMorphism
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QLiteralMorphism edit t) where
    qTypeDescription = "point* ~> " <> literalTypeDescription @t <> "*"
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QLiteralMorphism edit t) <> ")"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (QLiteralMorphism edit t) where
    fromQValue v = do
        m <- fromQValue v
        return $ literalPinaforeLensMorphism . m

-- QPointMorphism
--
instance HasQTypeDescription (QPointMorphism edit) where
    qTypeDescription = "point* ~> point*"
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QPointMorphism edit) <> ")"

instance edit ~ baseedit => FromQValue baseedit (QPointMorphism edit) where
    fromQValue (MkAny QTMorphism v) = return v
    fromQValue v = badFromQValue v

-- QImLiteralMorphism
--
instance {-# OVERLAPPABLE #-} AsLiteral t => HasQTypeDescription (QImLiteralMorphism edit t) where
    qTypeDescription = "point ~> " <> literalTypeDescription @t
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QImLiteralMorphism edit t) <> ")"

instance {-# OVERLAPPABLE #-} (edit ~ baseedit, AsLiteral t, HasPinaforeTableEdit baseedit) =>
                                  FromQValue baseedit (QImLiteralMorphism edit t) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m

-- QImPointMorphism
--
instance HasQTypeDescription (QImPointMorphism edit) where
    qTypeDescription = "point ~> point"
    qTypeDescriptionSingle = "(" <> qTypeDescription @(QImPointMorphism edit) <> ")"

instance edit ~ baseedit => FromQValue baseedit (QImPointMorphism edit) where
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

instance (edit ~ baseedit, HasPinaforeTableEdit baseedit) => FromQValue baseedit (UIWindow edit) where
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

instance (ToQValue baseedit a, ToQValue baseedit b, HasPinaforeTableEdit baseedit) => ToQValue baseedit (a, b) where
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

instance (ToQValue baseedit a, FromQValue baseedit b, HasPinaforeTableEdit baseedit) =>
             FromQValue baseedit (a -> Result Text b) where
    fromQValue vf = do
        f <- qpartialapply vf
        return $ fromQValue . f . toQValue

instance (HasPinaforeTableEdit baseedit, ToQValue baseedit a, edit ~ baseedit) =>
             FromQValue baseedit (a -> QAction edit) where
    fromQValue vf = do
        f <- qpartialapply vf
        return $ \a -> do
            action <- liftInner $ fromQValue $ f $ toQValue a
            action

instance (FromQValue baseedit a, ToQValue baseedit b) => ToQValue baseedit (a -> b) where
    toQValue ab = qfunction $ toQValue . fmap ab . fromQValue

-- Other
--
qifthenelse ::
       HasPinaforeTableEdit baseedit => Lifted baseedit Bool -> QValue baseedit -> QValue baseedit -> QValue baseedit
qifthenelse (LiftedConstant True) valt _ = valt
qifthenelse (LiftedConstant False) _ vale = vale
qifthenelse (LiftedFunction func) valt vale = qfifthenelse func valt vale
  where
    fvalIfThenElse ::
           forall baseedit t.
           t
        -> QImLiteral baseedit Bool
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
        => QImLiteral baseedit Bool
        -> QValue baseedit
        -> QValue baseedit
        -> QValue baseedit
    qfifthenelse _ v@(MkAny QTException _) _ = v
    qfifthenelse _ _ v@(MkAny QTException _) = v
    qfifthenelse f (MkAny QTList lt) (MkAny QTList le) =
        if length lt == length le
            then MkAny QTList $ zipWith (qfifthenelse f) lt le
            else qexception $ pack $ "cannot match lists of lengths " ++ show (length lt) ++ " and " ++ show (length le)
    qfifthenelse f t e
        | SuccessResult vt <- fromQValue @baseedit @(QImLiteral baseedit Text) t
        , SuccessResult ve <- fromQValue @baseedit @(QImLiteral baseedit Text) e =
            toQValue $ fvalIfThenElse Nothing f vt ve
    qfifthenelse f t e
        | SuccessResult vt <- fromQValue @baseedit @(QImPoint baseedit) t
        , SuccessResult ve <- fromQValue @baseedit @(QImPoint baseedit) e = toQValue $ fvalIfThenElse Nothing f vt ve
    qfifthenelse f t e
        | SuccessResult vt <- fromQValue @baseedit @(QImSet baseedit) t
        , SuccessResult ve <- fromQValue @baseedit @(QImSet baseedit) e = toQValue $ fvalIfThenElse mempty f vt ve
    -- possibly add cases for QTMorphism and QTInverseMorphism?
    qfifthenelse f t e
        | SuccessResult vt <- qpartialapply t
        , SuccessResult ve <- qpartialapply e = MkAny QTFunction $ \a -> qfifthenelse f (vt a) (ve a)
    qfifthenelse f (MkAny QTUserInterface vt) (MkAny QTUserInterface ve) = let
        pickUISpec :: Maybe Bool -> UISpec baseedit
        pickUISpec Nothing = uiNull
        pickUISpec (Just True) = vt
        pickUISpec (Just False) = ve
        in toQValue $ wholeEditFunction pickUISpec . f
    qfifthenelse _ (MkAny tt _) (MkAny te _) = qexception $ pack $ "cannot match " ++ show tt ++ " and " ++ show te
