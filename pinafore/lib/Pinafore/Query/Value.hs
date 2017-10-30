module Pinafore.Query.Value where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.AsText;
    import Pinafore.Edit;


    data QType t where
    {
        QPoint :: QType Point;
        QLiteral :: QType Text;
        QLensValue :: QType (PinaforeLensValue (WholeEdit (Maybe Point)));
        QLensSet :: QType (PinaforeLensValue (FiniteSetEdit Point));
        QMorphism :: QType (PinaforeLensMorphism Point Point);
        QInverseMorphism :: QType (PinaforeLensMorphism Point Point);
        QFunction :: QType (QValue -> Result String QValue);
    };

    instance Show (QType t) where
    {
        show QPoint = "point";
        show QLiteral = "literal";
        show QLensValue = "value";
        show QLensSet = "set";
        show QMorphism = "morphism";
        show QInverseMorphism = "inverse morphism";
        show QFunction = "function";
    };

    instance TestEquality QType where
    {
        testEquality QPoint QPoint = Just Refl;
        testEquality QLiteral QLiteral = Just Refl;
        testEquality QLensValue QLensValue = Just Refl;
        testEquality QLensSet QLensSet = Just Refl;
        testEquality QMorphism QMorphism = Just Refl;
        testEquality QInverseMorphism QInverseMorphism = Just Refl;
        testEquality QFunction QFunction = Just Refl;
        testEquality _ _ = Nothing;
    };


    type QValue = Any QType;

    instance Show QValue where
    {
        show (MkAny QPoint (MkPoint val)) = "!" ++ show val;
        show (MkAny QLiteral val) = unpack val;
        show (MkAny t _) = "<" ++ show t ++ ">";
    };

    qliteral :: Text -> QValue;
    qliteral = MkAny QLiteral;

    qfunction :: (QValue -> Result String QValue) -> QValue;
    qfunction = MkAny QFunction;

    qapply :: QValue -> QValue -> Result String QValue;
    qapply (MkAny QFunction f) a = f a;
    qapply (MkAny QMorphism f) (MkAny QPoint a) = return $ MkAny QLensValue $ applyPinaforeLens f $ constGeneralLens $ Just a;
    qapply (MkAny QMorphism f) (MkAny QLensValue a) = return $ MkAny QLensValue $ applyPinaforeLens f a;
    qapply (MkAny QMorphism f) (MkAny QLensSet a) = return $ MkAny QLensSet $ readOnlyGeneralLens $ convertGeneralFunction <.> applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a);
    qapply (MkAny QInverseMorphism f) (MkAny QPoint a) = return $ MkAny QLensSet $ applyInversePinaforeLens f $ constGeneralLens $ Just a;
    qapply (MkAny QInverseMorphism f) (MkAny QLiteral a) = return $ MkAny QLensSet $ applyInversePinaforeLens (primitivePinaforeLensMorphism . f) $ constGeneralLens $ Just a;
    qapply (MkAny QInverseMorphism f) (MkAny QLensValue a) = return $ MkAny QLensSet $ applyInversePinaforeLens f a;
    qapply (MkAny QInverseMorphism f) (MkAny QLensSet a) = return $ MkAny QLensSet $ readOnlyGeneralLens $ convertGeneralFunction <.> applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a);
    qapply (MkAny tf _) (MkAny ta _) = fail $ "cannot apply " ++ show tf ++ " to " ++ show ta;

    qinvert :: QValue -> Result String QValue;
    qinvert (MkAny QMorphism m) = return $ MkAny QInverseMorphism m;
    qinvert (MkAny QInverseMorphism m) = return $ MkAny QMorphism m;
    qinvert (MkAny t _) = fail $ "cannot invert " ++ show t;

    qcombine :: QValue -> QValue -> QValue;
    qcombine (MkAny QMorphism g) (MkAny QMorphism f) = MkAny QMorphism $ g . f;
    qcombine (MkAny QInverseMorphism g) (MkAny QInverseMorphism f) = MkAny QMorphism $ g . f;
    qcombine g f = MkAny QFunction $ \a -> do
    {
        b <- qapply f a;
        qapply g b;
    };

    qpredicate :: Predicate -> QValue;
    qpredicate p = MkAny QMorphism $ predicatePinaforeLensMorphism p;

    qpoint :: Point -> QValue;
    qpoint = MkAny QPoint;


    qmeet :: QValue -> QValue -> Result String QValue;
    qmeet (MkAny QLensSet a) (MkAny QLensSet b) = return $ MkAny QLensSet $ readOnlyGeneralLens meetGeneralFunction <.> pairJoinGeneralLenses a b;
    qmeet (MkAny ta _) (MkAny tb _) = fail $ "cannot meet " ++ show ta ++ " and " ++ show tb;

    qjoin :: QValue -> QValue -> Result String QValue;
    qjoin (MkAny QLensSet a) (MkAny QLensSet b) = return $ MkAny QLensSet $ readOnlyGeneralLens joinGeneralFunction <.> pairJoinGeneralLenses a b;
    qjoin (MkAny ta _) (MkAny tb _) = fail $ "cannot meet " ++ show ta ++ " and " ++ show tb;

    qdisplay :: QValue -> PinaforeFunctionValue (FiniteSet Text);
    qdisplay (MkAny QLiteral a) = constGeneralFunction $ opoint a;
    qdisplay (MkAny QPoint a) = let
    {
        ms (Just t) = opoint t;
        ms _ = mempty;
    } in applyPinaforeFunction (arr ms . lensFunctionMorphism primitivePinaforeLensMorphism) $ constGeneralFunction a;
    qdisplay (MkAny QLensValue a) = let
    {
        mms (Just (Just t)) = opoint t;
        mms _ = mempty;
    } in applyPinaforeFunction (arr mms . cfmap (lensFunctionMorphism primitivePinaforeLensMorphism)) (lensFunctionValue a);
    qdisplay (MkAny QLensSet a) = applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism primitivePinaforeLensMorphism)) (lensFunctionValue a);
    qdisplay v = constGeneralFunction $ opoint $ pack $ show v;

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

    instance ToQValue a => ToQValue (QValue -> a) where
    {
        toQValue va = return $ qfunction $ \v -> toQValue $ va v;
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
}
