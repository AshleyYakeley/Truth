module Pinafore.Query.Value where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.AsText;
    import Pinafore.Edit;


    data QType t where
    {
        QLiteral :: QType Text;
        QPoint :: QType (PinaforeLensValue (WholeEdit (Maybe Point)));
        QSet :: QType (PinaforeLensValue (FiniteSetEdit Point));
        QMorphism :: QType (PinaforeLensMorphism Point Point);
        QInverseMorphism :: QType (PinaforeLensMorphism Point Point);
        QFunction :: QType (QValue -> Result String QValue);
        QUISpec :: QType (UISpec PinaforeEdit);
    };

    instance Show (QType t) where
    {
        show QLiteral = "literal";
        show QPoint = "point";
        show QSet = "set";
        show QMorphism = "morphism";
        show QInverseMorphism = "inverse morphism";
        show QFunction = "function";
        show QUISpec = "user interface";
    };

    instance TestEquality QType where
    {
        testEquality QLiteral QLiteral = Just Refl;
        testEquality QPoint QPoint = Just Refl;
        testEquality QSet QSet = Just Refl;
        testEquality QMorphism QMorphism = Just Refl;
        testEquality QInverseMorphism QInverseMorphism = Just Refl;
        testEquality QFunction QFunction = Just Refl;
        testEquality _ _ = Nothing;
    };


    type QValue = Any QType;

    instance Show QValue where
    {
        show (MkAny QLiteral val) = unpack val;
        show (MkAny QUISpec val) = show val;
        show (MkAny t _) = "<" ++ show t ++ ">";
    };

    qliteral :: Text -> QValue;
    qliteral = MkAny QLiteral;

    qfunction :: (QValue -> Result String QValue) -> QValue;
    qfunction = MkAny QFunction;

    qapply :: QValue -> QValue -> Result String QValue;
    qapply (MkAny QFunction f) a = f a;
    qapply (MkAny QMorphism f) (MkAny QPoint a) = return $ MkAny QPoint $ applyPinaforeLens f a;
    qapply (MkAny QMorphism f) (MkAny QSet a) = return $ MkAny QSet $ readOnlyGeneralLens $ convertGeneralFunction <.> applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a);
    qapply (MkAny QInverseMorphism f) (MkAny QLiteral a) = return $ MkAny QSet $ applyInversePinaforeLens (primitivePinaforeLensMorphism . f) $ constGeneralLens $ Just a;
    qapply (MkAny QInverseMorphism f) (MkAny QPoint a) = return $ MkAny QSet $ applyInversePinaforeLens f a;
    qapply (MkAny QInverseMorphism f) (MkAny QSet a) = return $ MkAny QSet $ readOnlyGeneralLens $ convertGeneralFunction <.> applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a);
    qapply (MkAny tf _) (MkAny ta _) = fail $ "cannot apply " ++ show tf ++ " to " ++ show ta;

    qinvert :: QValue -> Result String QValue;
    qinvert (MkAny QMorphism m) = return $ MkAny QInverseMorphism m;
    qinvert (MkAny QInverseMorphism m) = return $ MkAny QMorphism m;
    qinvert (MkAny t _) = fail $ "cannot invert " ++ show t;

    qcombine :: QValue -> QValue -> QValue;
    qcombine (MkAny QMorphism g) (MkAny QMorphism f) = MkAny QMorphism $ g . f;
    qcombine (MkAny QInverseMorphism g) (MkAny QInverseMorphism f) = MkAny QInverseMorphism $ g . f;
    qcombine g f = MkAny QFunction $ \a -> do
    {
        b <- qapply f a;
        qapply g b;
    };

    qpredicate :: Predicate -> QValue;
    qpredicate p = MkAny QMorphism $ predicatePinaforeLensMorphism p;

    qpoint :: Point -> QValue;
    qpoint p = MkAny QPoint $ constGeneralLens $ Just p;

    qmeet :: PinaforeLensValue (FiniteSetEdit Point) -> PinaforeLensValue (FiniteSetEdit Point) -> PinaforeLensValue (FiniteSetEdit Point);
    qmeet a b = readOnlyGeneralLens meetGeneralFunction <.> pairJoinGeneralLenses a b;

    qjoin :: PinaforeLensValue (FiniteSetEdit Point) -> PinaforeLensValue (FiniteSetEdit Point) -> PinaforeLensValue (FiniteSetEdit Point);
    qjoin a b = readOnlyGeneralLens joinGeneralFunction <.> pairJoinGeneralLenses a b;

    maybeToFiniteSet :: Maybe a -> FiniteSet a;
    maybeToFiniteSet (Just a) = opoint a;
    maybeToFiniteSet Nothing = mempty;

    qdisplay :: QValue -> PinaforeFunctionValue (FiniteSet Text);
    qdisplay val = case fromQValue val of
    {
        SuccessResult a -> a;
        FailureResult _ -> constGeneralFunction $ opoint $ pack $ show val;
    };

    badFromQValue :: QValue -> Result String t;
    badFromQValue (MkAny t _) = fail $ "unexpected " ++ show t;

    class FromQValue t where
    {
        fromQValue :: QValue -> Result String t;
    };

    instance FromQValue QValue where
    {
        fromQValue = return;
    };

    instance FromQValue Text where
    {
        fromQValue (MkAny QLiteral v) = return v;
        fromQValue v = badFromQValue v;
    };

    instance FromQValue String where
    {
        fromQValue v = do
        {
            text <- fromQValue v;
            case fromText text of
            {
                Just a -> return a;
                Nothing -> fail "couldn't interpret text";
            };
        };
    };

    instance FromQValue (PinaforeLensValue (WholeEdit (Maybe Point))) where
    {
        fromQValue (MkAny QPoint v) = return v;
        fromQValue v = badFromQValue v;
    };

    instance FromQValue (PinaforeLensValue (FiniteSetEdit Point)) where
    {
        fromQValue (MkAny QPoint v) = return $ funcROGeneralLens maybeToFiniteSet <.> v;
        fromQValue (MkAny QSet v) = return v;
        fromQValue v = badFromQValue v;
    };

    instance FromQValue (PinaforeFunctionValue (FiniteSet Text)) where
    {
        fromQValue (MkAny QLiteral a) = return $ constGeneralFunction $ opoint a;
        fromQValue (MkAny QPoint a) = return $ let
        {
            mms mmt = maybeToFiniteSet $ mmt >>= id;
        } in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism primitivePinaforeLensMorphism)) (lensFunctionValue a);
        fromQValue (MkAny QSet a) = return $ applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism primitivePinaforeLensMorphism)) (lensFunctionValue a);
        fromQValue v = badFromQValue v;
    };

    instance FromQValue (UISpec PinaforeEdit) where
    {
        fromQValue (MkAny QUISpec v) = return v;
        fromQValue v = badFromQValue v;
    };

    instance FromQValue t => FromQValue (Result String t) where
    {
        fromQValue v = fmap return $ fromQValue v;
    };

    instance (ToQValue a,FromQValue b) => FromQValue (a -> Result String b) where
    {
        fromQValue (MkAny QFunction f) = return $ \a -> do
        {
            va <- toQValue a;
            vb <- f va;
            fromQValue vb;
        };
        fromQValue v = badFromQValue v;
    };

    class ToQValue t where
    {
        toQValue :: t -> Result String QValue;
    };

    instance ToQValue QValue where
    {
        toQValue = return
    };

    instance ToQValue a => ToQValue (Result String a) where
    {
        toQValue ma = do
        {
            a <- ma;
            toQValue a;
        };
    };

    instance (FromQValue a,ToQValue b) => ToQValue (a -> b) where
    {
        toQValue ab = return $ qfunction $ \v -> do
        {
            a <- fromQValue v;
            toQValue $ ab a;
        };
    };

    instance ToQValue Predicate where
    {
        toQValue p = return $ qpredicate p;
    };

    instance ToQValue Point where
    {
        toQValue p = return $ qpoint p;
    };

    instance ToQValue Text where
    {
        toQValue p = return $ qliteral p;
    };

    instance ToQValue String where
    {
        toQValue t = toQValue $ toText t;
    };

    instance ToQValue Bool where
    {
        toQValue t = toQValue $ toText t;
    };

    instance ToQValue Int where
    {
        toQValue t = toQValue $ toText t;
    };

    instance ToQValue Integer where
    {
        toQValue t = toQValue $ toText t;
    };

    instance ToQValue (PinaforeLensValue (WholeEdit (Maybe Point))) where
    {
        toQValue t = return $ MkAny QPoint t;
    };

    instance ToQValue (PinaforeLensValue (FiniteSetEdit Point)) where
    {
        toQValue t = return $ MkAny QSet t;
    };

    instance edit ~ PinaforeEdit => ToQValue (UISpec edit) where
    {
        toQValue t = return $ MkAny QUISpec t;
    };
}
