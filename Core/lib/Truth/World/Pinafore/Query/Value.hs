module Truth.World.Pinafore.Query.Value where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Truth.World.Pinafore.AsText;
    import Truth.World.Pinafore.Edit;


    data QLiteralType t where
    {
        QBool :: QLiteralType Bool;
        QText :: QLiteralType Text;
        QInt :: QLiteralType Int;
    };

    instance TestEquality QLiteralType where
    {
        testEquality QBool QBool = Just Refl;
        testEquality QText QText = Just Refl;
        testEquality QInt QInt = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance Eq1 QLiteralType where
    {
        equals1 a b = isJust $ testEquality a b;
    };

    instance Representative QLiteralType where
    {
        getRepWitness QBool = Dict;
        getRepWitness QText = Dict;
        getRepWitness QInt = Dict;
    };

    instance Is QLiteralType Bool where
    {
        representative = QBool;
    };

    instance Is QLiteralType Text where
    {
        representative = QText;
    };

    instance Is QLiteralType Int where
    {
        representative = QInt;
    };

    instance Show (QLiteralType t) where
    {
        show QBool = "boolean";
        show QText = "text";
        show QInt = "integer";
    };

    qprimAsText :: QLiteralType t -> Dict (AsText t);
    qprimAsText QBool = Dict;
    qprimAsText QText = Dict;
    qprimAsText QInt = Dict;


    data QPrimitiveType t where
    {
        QPoint :: QPrimitiveType Point;
        QLiteral :: QLiteralType t -> QPrimitiveType t;
    };

    instance Show (QPrimitiveType t) where
    {
        show QPoint = "point";
        show (QLiteral t) = show t;
    };

    instance TestEquality QPrimitiveType where
    {
        testEquality QPoint QPoint = Just Refl;
        testEquality (QLiteral t1) (QLiteral t2) = do
        {
            Refl <- testEquality t1 t2;
            return Refl;
        };
        testEquality _ _ = Nothing;
    };


    data QType t where
    {
        QPrimitive :: QPrimitiveType t -> QType t;
        QLensValue :: QType (PinaforeLensValue (WholeEdit (Maybe Point)));
        QLensSet :: QType (PinaforeLensValue (FiniteSetEdit Point));
        QMorphism :: QType (PinaforeLensMorphism Point Point);
        QInverseMorphism :: QType (PinaforeLensMorphism Point Point);
        QFunction :: QType (QValue -> Result String QValue);
    };

    instance Show (QType t) where
    {
        show (QPrimitive t) = show t;
        show QLensValue = "value";
        show QLensSet = "set";
        show QMorphism = "morphism";
        show QInverseMorphism = "inverse morphism";
        show QFunction = "function";
    };

    instance TestEquality QType where
    {
        testEquality (QPrimitive t1) (QPrimitive t2) = do
        {
            Refl <- testEquality t1 t2;
            return Refl;
        };
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
        show (MkAny (QPrimitive QPoint) (MkPoint val)) = show val;
        show (MkAny (QPrimitive (QLiteral t)) val) = case qprimAsText t of
        {
            Dict -> unpack $ toText val;
        };
        show (MkAny t _) = "<" ++ show t ++ ">";
    };

    qapply :: QValue -> QValue -> Result String QValue;
    qapply (MkAny QFunction f) a = f a;
    qapply (MkAny QMorphism f) (MkAny QLensValue a) = return $ MkAny QLensValue $ applyPinaforeLens f a;
    qapply (MkAny QMorphism f) (MkAny (QPrimitive QPoint) a) = return $ MkAny QLensValue $ applyPinaforeLens f $ constGeneralLens $ Just a;
    qapply (MkAny QInverseMorphism f) (MkAny QLensValue a) = return $ MkAny QLensSet $ applyInversePinaforeLens f a;
    qapply (MkAny QInverseMorphism f) (MkAny (QPrimitive QPoint) a) = return $ MkAny QLensSet $ applyInversePinaforeLens f $ constGeneralLens $ Just a;
    qapply (MkAny tf _) (MkAny ta _) = fail $ "cannot apply " ++ show tf ++ " to " ++ show ta;
}
