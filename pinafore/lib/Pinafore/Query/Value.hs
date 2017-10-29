module Pinafore.Query.Value where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.Edit;


    data QPrimitiveType t where
    {
        QPoint :: QPrimitiveType Point;
        QLiteral :: QPrimitiveType Text;
    };

    instance Show (QPrimitiveType t) where
    {
        show QPoint = "point";
        show QLiteral = "literal";
    };

    instance TestEquality QPrimitiveType where
    {
        testEquality QPoint QPoint = Just Refl;
        testEquality QLiteral QLiteral = Just Refl;
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
        show (MkAny (QPrimitive QPoint) (MkPoint val)) = "!" ++ show val;
        show (MkAny (QPrimitive QLiteral) val) = unpack val;
        show (MkAny t _) = "<" ++ show t ++ ">";
    };

    qapply :: QValue -> QValue -> Result String QValue;
    qapply (MkAny QFunction f) a = f a;
    qapply (MkAny QMorphism f) (MkAny (QPrimitive QPoint) a) = return $ MkAny QLensValue $ applyPinaforeLens f $ constGeneralLens $ Just a;
    qapply (MkAny QMorphism f) (MkAny QLensValue a) = return $ MkAny QLensValue $ applyPinaforeLens f a;
    qapply (MkAny QMorphism f) (MkAny QLensSet a) = return $ MkAny QLensSet $ readOnlyGeneralLens $ convertGeneralFunction <.> applyPinaforeFunction (arr catMaybes . cfmap (lensFunctionMorphism f)) (lensFunctionValue a);
    qapply (MkAny QInverseMorphism f) (MkAny (QPrimitive QPoint) a) = return $ MkAny QLensSet $ applyInversePinaforeLens f $ constGeneralLens $ Just a;
    qapply (MkAny QInverseMorphism f) (MkAny (QPrimitive QLiteral) a) = return $ MkAny QLensSet $ applyInversePinaforeLens (primitivePinaforeLensMorphism . f) $ constGeneralLens $ Just a;
    qapply (MkAny QInverseMorphism f) (MkAny QLensValue a) = return $ MkAny QLensSet $ applyInversePinaforeLens f a;
    qapply (MkAny QInverseMorphism f) (MkAny QLensSet a) = return $ MkAny QLensSet $ readOnlyGeneralLens $ convertGeneralFunction <.> applyPinaforeFunction (arr (mconcat . unFiniteSet) . cfmap (lensInverseFunctionMorphism f)) (lensFunctionValue a);
    qapply (MkAny tf _) (MkAny ta _) = fail $ "cannot apply " ++ show tf ++ " to " ++ show ta;

    qinvert :: QValue -> Result String QValue;
    qinvert (MkAny QMorphism m) = return $ MkAny QInverseMorphism m;
    qinvert (MkAny QInverseMorphism m) = return $ MkAny QMorphism m;
    qinvert (MkAny t _) = fail $ "cannot invert " ++ show t;
}
