module Pinafore.Query.Convert
    ( HasQTypeDescription(..)
    , FromQValue(..)
    , ToQValue(..)
    , qifthenelse
    ) where

import Data.List (zipWith)
import Pinafore.AsText
import Pinafore.Morphism
import Pinafore.Query.Literal
import Pinafore.Query.Value
import Pinafore.Table
import Shapes
import Truth.Core

maybeToFiniteSet :: Maybe a -> FiniteSet a
maybeToFiniteSet (Just a) = opoint a
maybeToFiniteSet Nothing = mempty

class HasQTypeDescription baseedit t where
    qTypeDescription :: Text
    qTypeDescriptionSingle :: Text
    qTypeDescriptionSingle = qTypeDescription @baseedit @t

class HasQTypeDescription baseedit t =>
      FromQValue baseedit t where
    fromQValue :: QValue baseedit -> Result Text t

class HasQTypeDescription baseedit t =>
      ToQValue baseedit t where
    toQValue :: t -> QValue baseedit

instance {-# OVERLAPPABLE #-} AsText t => HasQTypeDescription baseedit t where
    qTypeDescription = textTypeDescription @t <> "!"

instance {-# OVERLAPPABLE #-} AsText t => FromQValue baseedit t where
    fromQValue v@(MkAny QTConstant text) =
        case fromText text of
            Just t -> return t
            Nothing -> badFromQValue v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} AsText t => ToQValue baseedit t where
    toQValue t = qconstant $ toText t

-- QValue
--
instance HasQTypeDescription baseedit (QValue baseedit) where
    qTypeDescription = "value"

instance FromQValue baseedit (QValue baseedit) where
    fromQValue = return

instance ToQValue baseedit (QValue baseedit) where
    toQValue = id

-- Literal
--
instance AsText t => HasQTypeDescription baseedit (Literal baseedit t) where
    qTypeDescription = textTypeDescription @t

instance (AsText t, HasPinaforeTableEdit baseedit) => FromQValue baseedit (Literal baseedit t) where
    fromQValue v@(MkAny QTConstant text) =
        case fromText text of
            Just a -> return $ LiteralConstant a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTLiteral v) =
        return $ LiteralFunction $ funcEditFunction (\mtext -> mtext >>= fromText) . editLensFunction v
    fromQValue (MkAny QTPoint v) =
        return $ LiteralFunction $ editLensFunction $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance AsText t => ToQValue baseedit (Literal baseedit t) where
    toQValue (LiteralConstant t) = toQValue t
    toQValue (LiteralFunction t) = toQValue t

-- QLiteral
--
instance {-# OVERLAPPABLE #-} AsText t => HasQTypeDescription baseedit (QLiteral baseedit t) where
    qTypeDescription = textTypeDescription @t <> "*"

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (QLiteral baseedit t) where
    fromQValue v@(MkAny QTConstant text) =
        case fromText text of
            Just a -> return $ constEditLens $ Just a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTLiteral v) = return $ (funcEditLens $ \mt -> mt >>= fromText) . v
    fromQValue (MkAny QTPoint v) = return $ applyPinaforeLens literalPinaforeLensMorphism v
    fromQValue v = badFromQValue v

instance {-# OVERLAPPABLE #-} AsText t => ToQValue baseedit (QLiteral baseedit t) where
    toQValue t = MkAny QTLiteral $ (funcEditLens $ fmap toText) . t

-- Predicate
--
instance HasQTypeDescription baseedit Predicate where
    qTypeDescription = "predicate"

instance HasPinaforeTableEdit baseedit => ToQValue baseedit Predicate where
    toQValue p = qpredicate p

-- Point
--
instance HasQTypeDescription baseedit Point where
    qTypeDescription = "point"

instance ToQValue baseedit Point where
    toQValue p = qpoint p

-- QPoint
--
instance HasQTypeDescription baseedit (QPoint baseedit) where
    qTypeDescription = "point*"

instance FromQValue baseedit (QPoint baseedit) where
    fromQValue (MkAny QTPoint v) = return v
    fromQValue v = badFromQValue v

instance ToQValue baseedit (QPoint baseedit) where
    toQValue t = MkAny QTPoint t

-- QSet
--
instance HasQTypeDescription baseedit (QSet baseedit) where
    qTypeDescription = "set*"

instance FromQValue baseedit (QSet baseedit) where
    fromQValue (MkAny QTPoint v) = return $ (funcEditLens maybeToFiniteSet) . v
    fromQValue (MkAny QTSet v) = return v
    fromQValue v = badFromQValue v

instance ToQValue baseedit (QSet baseedit) where
    toQValue t = MkAny QTSet t

-- QImLiteral
--
instance {-# OVERLAPPABLE #-} AsText t => HasQTypeDescription baseedit (QImLiteral baseedit t) where
    qTypeDescription = textTypeDescription @t

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (QImLiteral baseedit t) where
    fromQValue v = do
        cl :: Literal baseedit t <- fromQValue v
        return $ literalToFunction cl

instance {-# OVERLAPPABLE #-} AsText t => ToQValue baseedit (QImLiteral baseedit t) where
    toQValue ef = toQValue $ readOnlyEditLens ef

-- QImPoint
--
instance HasQTypeDescription baseedit (QImPoint baseedit) where
    qTypeDescription = "point"

instance FromQValue baseedit (QImPoint baseedit) where
    fromQValue v = do
        a :: QPoint baseedit <- fromQValue v
        return $ lensFunctionValue a

instance ToQValue baseedit (QImPoint baseedit) where
    toQValue ef = toQValue $ readOnlyEditLens ef

-- im set of t
--
instance {-# OVERLAPPABLE #-} AsText t =>
                              HasQTypeDescription baseedit (PinaforeFunctionValue baseedit (FiniteSet t)) where
    qTypeDescription = textTypeDescription @t <> "-set"

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (PinaforeFunctionValue baseedit (FiniteSet t)) where
    fromQValue v@(MkAny QTConstant text) =
        case fromText text of
            Just a -> return $ constEditFunction $ opoint a
            Nothing -> badFromQValue v
    fromQValue (MkAny QTLiteral a) =
        return $ (funcEditFunction $ maybePoint . (\mt -> mt >>= fromText)) . editLensFunction a
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
    fromQValue v = badFromQValue v

-- QImSet
--
instance HasQTypeDescription baseedit (QImSet baseedit) where
    qTypeDescription = "set"

instance FromQValue baseedit (QImSet baseedit) where
    fromQValue (MkAny QTPoint a) =
        return $ let
            mms mmt = maybeToFiniteSet $ mmt >>= id
            in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue (MkAny QTSet a) =
        return $ applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism id)) (lensFunctionValue a)
    fromQValue v = badFromQValue v

instance ToQValue baseedit (QImSet baseedit) where
    toQValue ef = toQValue @_ @(QSet baseedit) $ readOnlyEditLens $ convertEditFunction . ef

-- QLiteralMorphism
--
instance {-# OVERLAPPABLE #-} AsText t => HasQTypeDescription baseedit (QLiteralMorphism baseedit t) where
    qTypeDescription = "point* ~> " <> textTypeDescription @t <> "*"
    qTypeDescriptionSingle = "(" <> qTypeDescription @baseedit @(QLiteralMorphism baseedit t) <> ")"

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (QLiteralMorphism baseedit t) where
    fromQValue v = do
        m <- fromQValue v
        return $ literalPinaforeLensMorphism . m

-- QPointMorphism
--
instance HasQTypeDescription baseedit (QPointMorphism baseedit) where
    qTypeDescription = "point* ~> point*"
    qTypeDescriptionSingle = "(" <> qTypeDescription @baseedit @(QPointMorphism baseedit) <> ")"

instance FromQValue baseedit (QPointMorphism baseedit) where
    fromQValue (MkAny QTMorphism v) = return v
    fromQValue v = badFromQValue v

-- QImLiteralMorphism
--
instance {-# OVERLAPPABLE #-} AsText t => HasQTypeDescription baseedit (QImLiteralMorphism baseedit t) where
    qTypeDescription = "point ~> " <> textTypeDescription @t
    qTypeDescriptionSingle = "(" <> qTypeDescription @baseedit @(QImLiteralMorphism baseedit t) <> ")"

instance {-# OVERLAPPABLE #-} (AsText t, HasPinaforeTableEdit baseedit) =>
                              FromQValue baseedit (QImLiteralMorphism baseedit t) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m

-- QImPointMorphism
--
instance HasQTypeDescription baseedit (QImPointMorphism baseedit) where
    qTypeDescription = "point ~> point"
    qTypeDescriptionSingle = "(" <> qTypeDescription @baseedit @(QImPointMorphism baseedit) <> ")"

instance FromQValue baseedit (QImPointMorphism baseedit) where
    fromQValue v = do
        m <- fromQValue v
        return $ lensFunctionMorphism m

-- UISpec
--
instance baseedit ~ edit => HasQTypeDescription baseedit (UISpec edit) where
    qTypeDescription = "ui"

instance baseedit ~ edit => FromQValue baseedit (UISpec edit) where
    fromQValue (MkAny QTUserInterface v) = return v
    fromQValue v = badFromQValue v

instance baseedit ~ edit => ToQValue baseedit (UISpec edit) where
    toQValue t = MkAny QTUserInterface t

-- UISpec function
--
instance HasQTypeDescription baseedit (PinaforeFunctionValue baseedit (UISpec baseedit)) where
    qTypeDescription = "ui"

instance ToQValue baseedit (PinaforeFunctionValue baseedit (UISpec baseedit)) where
    toQValue ef = MkAny QTUserInterface $ uiSwitch ef

-- UIWindow
--
instance HasQTypeDescription baseedit (UIWindow baseedit) where
    qTypeDescription = qTypeDescription @baseedit @(EditFunction baseedit (WholeEdit (Maybe Text)), UISpec baseedit)

instance HasPinaforeTableEdit baseedit => FromQValue baseedit (UIWindow baseedit) where
    fromQValue v = do
        (title, content) <- fromQValue v
        return $ MkUIWindow (funcEditFunction @(WholeEdit (Maybe Text)) (fromMaybe "") . title) content

-- List
--
instance HasQTypeDescription baseedit t => HasQTypeDescription baseedit [t] where
    qTypeDescription = "[" <> qTypeDescription @baseedit @t <> "]"

instance FromQValue baseedit t => FromQValue baseedit [t] where
    fromQValue (MkAny QTList v) = for v fromQValue
    fromQValue v = badFromQValue v

instance ToQValue baseedit t => ToQValue baseedit [t] where
    toQValue t = MkAny QTList $ fmap toQValue t

-- Pair
--
instance (HasQTypeDescription baseedit a, HasQTypeDescription baseedit b) => HasQTypeDescription baseedit (a, b) where
    qTypeDescription = "(" <> qTypeDescription @baseedit @a <> ", " <> qTypeDescription @baseedit @b <> ")"

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
instance HasQTypeDescription baseedit t => HasQTypeDescription baseedit (Result Text t) where
    qTypeDescription = "result " <> qTypeDescription @baseedit @t

instance FromQValue baseedit t => FromQValue baseedit (Result Text t) where
    fromQValue v = fmap return $ fromQValue v

instance ToQValue baseedit t => ToQValue baseedit (Result Text t) where
    toQValue (SuccessResult a) = toQValue a
    toQValue (FailureResult e) = qexception e

-- Action
--
instance HasQTypeDescription baseedit t => HasQTypeDescription baseedit (IO t) where
    qTypeDescription = "action " <> qTypeDescription @baseedit @t

instance FromQValue baseedit t => FromQValue baseedit (IO t) where
    fromQValue v = fmap return $ fromQValue v

-- Function
--
instance {-# OVERLAPPABLE #-} (HasQTypeDescription baseedit a, HasQTypeDescription baseedit b) =>
                              HasQTypeDescription baseedit (a -> b) where
    qTypeDescription = qTypeDescriptionSingle @baseedit @a <> " -> " <> qTypeDescription @baseedit @b
    qTypeDescriptionSingle = "(" <> qTypeDescription @baseedit @(a -> b) <> ")"

instance (HasQTypeDescription baseedit a, HasQTypeDescription baseedit b) =>
         HasQTypeDescription baseedit (a -> Result Text b) where
    qTypeDescription = qTypeDescriptionSingle @baseedit @a <> " -> " <> qTypeDescription @baseedit @b
    qTypeDescriptionSingle = "(" <> qTypeDescription @baseedit @(a -> Result Text b) <> ")"

instance (ToQValue baseedit a, FromQValue baseedit b, HasPinaforeTableEdit baseedit) =>
         FromQValue baseedit (a -> Result Text b) where
    fromQValue vf = do
        f <- qpartialapply vf
        return $ fromQValue . f . toQValue

instance (FromQValue baseedit a, ToQValue baseedit b) => ToQValue baseedit (a -> b) where
    toQValue ab = qfunction $ toQValue . fmap ab . fromQValue

-- Other
--
qifthenelse ::
       HasPinaforeTableEdit baseedit => Literal baseedit Bool -> QValue baseedit -> QValue baseedit -> QValue baseedit
qifthenelse (LiteralConstant True) valt _ = valt
qifthenelse (LiteralConstant False) _ vale = vale
qifthenelse (LiteralFunction func) valt vale = qfifthenelse func valt vale
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
