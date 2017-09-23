module Truth.World.Pinafore.Ontology where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Truth.World.Pinafore.Edit;


    uuid :: String -> UUID;
    uuid s = fromMaybe (error $ "couldn't parse UUID " ++ show s) $ Data.UUID.fromString s;

    point :: String -> Point;
    point s = MkPoint $ uuid s;

    predicate :: String -> Predicate;
    predicate s = MkPredicate $ uuid s;

    data ViewPinaforeSimpleProperty t = MkViewPinaforeSimpleProperty
    {
        spropName :: String,
        spropMorphism :: PinaforeMorphism Point t,
        spropType :: ViewPinaforeType t
    };

    data ViewPinaforeListProperty = MkViewPinaforeListProperty
    {
        lpropName :: String,
        lpropMorphism :: PinaforeInverseMorphism Point Point,
        lpropType :: ViewPinaforeType Point,
        lpropColumns :: [ViewPinaforeSimpleProperty String]
    };

    data ViewPinaforeProperty = forall t. () => SimpleViewPinaforeProperty (ViewPinaforeSimpleProperty t) | ListViewPinaforeProperty ViewPinaforeListProperty;

    type PinaforeEditConstraint edit = FullEdit edit;

    data ViewPinaforePrimitive t where
    {
        MkViewPinaforePrimitive :: forall t. (Serialize t) => UISpec (ContextEdit PinaforeEdit (WholeEdit (Maybe t))) -> ViewPinaforePrimitive t;
    };

    data ViewPinaforeType t where
    {
        EntityViewPinaforeType :: String -> [ViewPinaforeProperty] -> ViewPinaforeType Point;
        PrimitiveViewPinaforeType :: ViewPinaforePrimitive t -> ViewPinaforeType t;
    };

    data ViewPinaforeValue = MkViewPinaforeValue Point (ViewPinaforeType Point);


    data UIEntityPicker edit where
    {
        MkUIEntityPicker :: String -> UIEntityPicker (ContextEdit PinaforeEdit (WholeEdit (Maybe Point)));
    };

    instance Show (UIEntityPicker edit) where
    {
        show (MkUIEntityPicker typename) = "picker " ++ typename;
    };

    instance UIType UIEntityPicker where
    {
        uiWitness = $(iowitness [t|UIEntityPicker|]);
    };

    data UIDraggable edit where
    {
        MkUIDraggable :: String -> UIDraggable (WholeEdit (Maybe Point));
    };

    instance Show (UIDraggable edit) where
    {
        show (MkUIDraggable typename) = "draggable " ++ typename;
    };

    instance UIType UIDraggable where
    {
        uiWitness = $(iowitness [t|UIDraggable|]);
    };

    pinaforePropertyKeyColumn :: ViewPinaforeSimpleProperty String -> KeyColumn (ContextEdit PinaforeEdit (ConstEdit Point));
    pinaforePropertyKeyColumn (MkViewPinaforeSimpleProperty name (MkPointedEditLens lens) _ptype) =
        MkKeyColumn name $ funcEditFunction (fromMaybe "<empty>") <.> editLensFunction lens <.> liftContextEditFunction (funcEditFunction Just);

    maybeWholeFunction :: forall a. ObjectFunction (ConstEdit a) (WholeEdit (Maybe a));
    maybeWholeFunction = let
    {
        editInitial = ();
        editGet :: () -> WholeReader (Maybe a) t -> Readable (WholeReader a) t;
        editGet () ReadWhole = fmap Just $ readable ReadWhole;
        editUpdate = never;
    } in MkEditFunction{..};

    maybeWholeLens :: forall a. GeneralLens (ConstEdit a) (WholeEdit (Maybe a));
    maybeWholeLens = let
    {
        editLensFunction = maybeWholeFunction;
        editLensPutEdit () (MkWholeEdit _) = return Nothing;
    } in MkCloseState MkEditLens{..};

    pinaforePropertySpec :: ViewPinaforeProperty -> UISpec (ContextEdit PinaforeEdit (WholeEdit (Maybe Point)));
    pinaforePropertySpec (SimpleViewPinaforeProperty (MkViewPinaforeSimpleProperty _name lens ptype)) = MkUISpec $ MkUILens (carryPointedEditLens lens) $ pinaforeRefTypeSpec ptype;
    pinaforePropertySpec (ListViewPinaforeProperty (MkViewPinaforeListProperty _name lens ptype cols)) = let
    {
        aspect :: Aspect (ContextEdit PinaforeEdit (MaybeEdit (ConstEdit Point)));
        aspect = MkAspect "item" $ MkUISpec $ MkUILens (liftContextGeneralLens $ MkCloseState convertEditLens) $ pinaforeValueTypeSpec ptype;

        spec :: UISpec (ContextEdit PinaforeEdit (KeyEdit (FiniteSet Point) (ConstEdit Point)));
        spec = MkUISpec $ MkUIContextTable (fmap pinaforePropertyKeyColumn cols) aspect;

        propertyLens :: GeneralLens (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) (ContextEdit PinaforeEdit (KeyEdit (FiniteSet Point) (ConstEdit ( Point))));
        propertyLens = carryPointedEditLens lens;
    }
    in MkUISpec $ MkUILens propertyLens spec;

    pinaforeRefTypeSpec :: ViewPinaforeType t -> UISpec (ContextEdit PinaforeEdit (WholeEdit (Maybe t)));
    pinaforeRefTypeSpec (EntityViewPinaforeType typename _) = MkUISpec $ MkUIEntityPicker typename;
    pinaforeRefTypeSpec (PrimitiveViewPinaforeType (MkViewPinaforePrimitive uispec)) = uispec;

    pinaforeValueTypeSpec :: ViewPinaforeType t -> UISpec (ContextEdit PinaforeEdit (WholeEdit (Maybe t)));
    pinaforeValueTypeSpec (EntityViewPinaforeType typename props) = MkUISpec $ MkUIVertical $ (MkUISpec $ MkUILens contentLens $ MkUISpec $ MkUIDraggable typename) : fmap pinaforePropertySpec props;
    pinaforeValueTypeSpec (PrimitiveViewPinaforeType (MkViewPinaforePrimitive uispec)) = uispec;

    pinaforeValueSpec :: ViewPinaforeValue -> UISpec PinaforeEdit;
    pinaforeValueSpec (MkViewPinaforeValue value tp) = let
    {
        conv :: EditLens ((),()) PinaforeEdit (ContextEdit PinaforeEdit (WholeEdit (Maybe Point)));
        conv = contextJoinEditLenses identityState (constEditLens (Just value));
    } in MkUISpec $ MkUILens (toGeneralLens conv) $ pinaforeValueTypeSpec tp;


    -- example ontology

    personType :: ViewPinaforeType Point;
    personType = EntityViewPinaforeType "Person" [SimpleViewPinaforeProperty personName,personMother,personFather,personChildren];

    personName :: ViewPinaforeSimpleProperty String;
    personName = let
    {
        spropName = "Name";
        spropMorphism :: PinaforeMorphism Point String;
        spropMorphism = primitivePinaforeMorphism <.> (predicatePinaforeMorphism $ predicate "498260df-6a8a-44f0-b285-68a63565a33b");
        spropType :: ViewPinaforeType String;
        spropType = PrimitiveViewPinaforeType $ MkViewPinaforePrimitive $ MkUISpec $ MkUILens (MkCloseState convertEditLens <.> contentLens) $ MkUISpec $ MkUIMaybe Nothing $ MkUISpec MkUITextEntry;
    } in MkViewPinaforeSimpleProperty{..};

    motherPredicate :: Predicate;
    motherPredicate = predicate "3afce58f-b7eb-4b11-8a75-2d66afd4d085";

    fatherPredicate :: Predicate;
    fatherPredicate = predicate "c005705f-9259-4d24-9713-db28a6e4f7d5";

    personMother :: ViewPinaforeProperty;
    personMother = let
    {
        spropName = "Mother";
        spropMorphism :: PinaforeLens (WholeEdit (Maybe Point)) (WholeEdit (Maybe Point));
        spropMorphism = predicatePinaforeMorphism motherPredicate;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personFather :: ViewPinaforeProperty;
    personFather = let
    {
        spropName = "Father";
        spropMorphism :: PinaforeLens (WholeEdit (Maybe Point)) (WholeEdit (Maybe Point));
        spropMorphism = predicatePinaforeMorphism fatherPredicate;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personChildren :: ViewPinaforeProperty;
    personChildren = let
    {
        lpropName = "Children";
        lpropMorphism = readOnlyPointedEditLens $ (pointedEditLensFunction $ predicateInversePinaforeMorphism motherPredicate) \/ (pointedEditLensFunction $ predicateInversePinaforeMorphism fatherPredicate);
        lpropType = personType;
        lpropColumns = [personName];
    } in ListViewPinaforeProperty MkViewPinaforeListProperty{..};

    peopleCollection :: ViewPinaforeProperty;
    peopleCollection = let
    {
        lpropName = "People";
        lpropMorphism = predicateInversePinaforeMorphism $ predicate "f06efa5e-190f-4e5d-8633-495c5683c124";
        lpropType = personType;
        lpropColumns = [personName];
    } in ListViewPinaforeProperty MkViewPinaforeListProperty{..};

    rootType :: ViewPinaforeType Point;
    rootType = EntityViewPinaforeType "People" [peopleCollection];

    rootValue :: ViewPinaforeValue;
    rootValue = MkViewPinaforeValue (point "78baed51-cb05-46b5-bcb4-49031532b890") rootType;
}
