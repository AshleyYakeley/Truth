module Truth.World.Pinafore.Ontology where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Truth.World.Pinafore.Edit;


    uuid :: String -> UUID;
    uuid s = fromMaybe (error $ "couldn't parse UUID " ++ show s) $ Data.UUID.fromString s;

    data ViewPinaforeSimpleProperty edit = MkViewPinaforeSimpleProperty
    {
        spropName :: String,
        spropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) edit,
        spropType :: ViewPinaforeType edit
    };

    data ViewPinaforeListProperty = MkViewPinaforeListProperty
    {
        lpropName :: String,
        lpropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) (FiniteSetEdit UUID),
        lpropType :: ViewPinaforeType (WholeEdit (Maybe UUID)),
        lpropColumns :: [ViewPinaforeSimpleProperty (OneWholeEdit Maybe (WholeEdit String))]
    };

    data ViewPinaforeProperty = forall edit. PinaforeEditConstraint edit => SimpleViewPinaforeProperty (ViewPinaforeSimpleProperty edit) | ListViewPinaforeProperty ViewPinaforeListProperty;

    type PinaforeEditConstraint edit = FullEdit edit;

    data ViewPinaforePrimitive edit where
    {
        MkViewPinaforePrimitive :: forall edit. (PinaforeEditConstraint edit,Serialize (EditSubject edit)) => UISpec (ContextEdit PinaforeEdit edit) -> ViewPinaforePrimitive edit;
    };

    data ViewPinaforeType edit where
    {
        EntityViewPinaforeType :: [ViewPinaforeProperty] -> ViewPinaforeType (WholeEdit (Maybe UUID));
        PrimitiveViewPinaforeType :: ViewPinaforePrimitive edit -> ViewPinaforeType edit;
    };

    data ViewPinaforeValue = MkViewPinaforeValue UUID (ViewPinaforeType (WholeEdit (Maybe UUID)));


    data UIEntityPicker edit where
    {
        MkUIEntityPicker :: UISpec (ContextEdit PinaforeEdit edit) -> UIEntityPicker edit;
    };

    instance Show (UIEntityPicker edit) where
    {
        show (MkUIEntityPicker uispec) = "picker " ++ show uispec;
    };

    instance UIType UIEntityPicker where
    {
        uiWitness = $(iowitness [t|UIEntityPicker|]);
    };

    pinaforePropertyKeyColumn :: ViewPinaforeSimpleProperty (OneWholeEdit Maybe (WholeEdit String)) -> KeyColumn (ContextEdit PinaforeEdit (NoEdit (WholeReader UUID)));
    pinaforePropertyKeyColumn (MkViewPinaforeSimpleProperty name (MkPointedEditLens lens) _ptype) =
        MkKeyColumn name $ funcEditFunction (fromMaybe "<empty>") <.> editLensFunction lens <.> liftContextEditFunction (funcEditFunction Just);

    maybeWholeFunction :: forall a. ObjectFunction (NoEdit (WholeReader a)) (WholeEdit (Maybe a));
    maybeWholeFunction = let
    {
        editInitial = ();
        editGet :: () -> WholeReader (Maybe a) t -> Readable (WholeReader a) t;
        editGet () ReadWhole = fmap Just $ readable ReadWhole;
        editUpdate = never;
    } in MkEditFunction{..};

    maybeWholeLens :: forall a. GeneralLens (NoEdit (WholeReader a)) (WholeEdit (Maybe a));
    maybeWholeLens = let
    {
        editLensFunction = maybeWholeFunction;
        editLensPutEdit () (MkWholeEdit _) = return Nothing;
    } in MkCloseState MkEditLens{..};

    pinaforePropertySpec :: ViewPinaforeProperty -> UISpec (ContextEdit PinaforeEdit (WholeEdit (Maybe UUID)));
    pinaforePropertySpec (SimpleViewPinaforeProperty (MkViewPinaforeSimpleProperty _name (MkPointedEditLens lens) ptype)) = MkUISpec $ MkUILens (MkCloseState lens) $ MkUISpec $ MkUIEntityPicker $ pinaforeTypeSpec ptype;
    pinaforePropertySpec (ListViewPinaforeProperty (MkViewPinaforeListProperty _name lens ptype cols)) = let
        {
            aspect :: Aspect (ContextEdit PinaforeEdit (OneWholeEdit Maybe (NoEdit (WholeReader UUID))));
            aspect = MkAspect "item" $ MkUISpec $ MkUILens (liftContextGeneralLens $ MkCloseState convertEditLens) $ pinaforeTypeSpec ptype;

            spec :: UISpec (ContextEdit PinaforeEdit (KeyEdit (FiniteSet UUID) (NoEdit (WholeReader UUID))));
            spec = MkUISpec $ MkUIContextTable (fmap pinaforePropertyKeyColumn cols) aspect;

            propertyLens :: GeneralLens (ContextEdit PinaforeEdit (WholeEdit (Maybe UUID))) (ContextEdit PinaforeEdit (KeyEdit (FiniteSet UUID) (NoEdit (WholeReader UUID))));
            propertyLens = carryPointedEditLens lens;
        }
        in MkUISpec $ MkUILens propertyLens spec;

    pinaforeTypeSpec :: ViewPinaforeType edit -> UISpec (ContextEdit PinaforeEdit edit);
    pinaforeTypeSpec (EntityViewPinaforeType props) = MkUISpec $ MkUIVertical $ fmap pinaforePropertySpec props;
    pinaforeTypeSpec (PrimitiveViewPinaforeType (MkViewPinaforePrimitive uispec)) = uispec;

    pinaforeValueSpec :: ViewPinaforeValue -> UISpec PinaforeEdit;
    pinaforeValueSpec (MkViewPinaforeValue value tp) = let
    {
        conv :: EditLens ((),()) PinaforeEdit (ContextEdit PinaforeEdit (WholeEdit (Maybe UUID)));
        conv = contextJoinEditLenses identityState (constEditLens (Just value));
    } in MkUISpec $ MkUILens (toGeneralLens conv) $ pinaforeTypeSpec tp;


    -- example ontology

    personType :: ViewPinaforeType (WholeEdit (Maybe UUID));
    personType = EntityViewPinaforeType [SimpleViewPinaforeProperty personName,personMother,personFather,personChildren];

    personName :: ViewPinaforeSimpleProperty (OneWholeEdit Maybe (WholeEdit String));
    personName = let
    {
        spropName = "Name";
        spropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) (OneWholeEdit Maybe (WholeEdit String));
        spropMorphism = primitiveEditPinaforeMorphism <.> (predicatePinaforeMorphism $ uuid "498260df-6a8a-44f0-b285-68a63565a33b");
        spropType :: ViewPinaforeType (OneWholeEdit Maybe (WholeEdit String));
        spropType = PrimitiveViewPinaforeType $ MkViewPinaforePrimitive $ MkUISpec $ MkUILens contentLens $ MkUISpec $ MkUIMaybe Nothing $ MkUISpec MkUITextEntry;
    } in MkViewPinaforeSimpleProperty{..};

    motherUUID :: UUID;
    motherUUID = uuid "3afce58f-b7eb-4b11-8a75-2d66afd4d085";

    fatherUUID :: UUID;
    fatherUUID = uuid "c005705f-9259-4d24-9713-db28a6e4f7d5";

    personMother :: ViewPinaforeProperty;
    personMother = let
    {
        spropName = "Mother";
        spropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) (WholeEdit (Maybe UUID));
        spropMorphism = predicatePinaforeMorphism motherUUID;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personFather :: ViewPinaforeProperty;
    personFather = let
    {
        spropName = "Father";
        spropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) (WholeEdit (Maybe UUID));
        spropMorphism = predicatePinaforeMorphism fatherUUID;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personChildren :: ViewPinaforeProperty;
    personChildren = let
    {
        lpropName = "Children";
        lpropMorphism = readOnlyPointedEditLens $ (pointedEditLensFunction $ predicateInversePinaforeMorphism motherUUID) \/ (pointedEditLensFunction $ predicateInversePinaforeMorphism fatherUUID);
        lpropType = personType;
        lpropColumns = [personName];
    } in ListViewPinaforeProperty MkViewPinaforeListProperty{..};

    peopleCollection :: ViewPinaforeProperty;
    peopleCollection = let
    {
        lpropName = "People";
        lpropMorphism = predicateInversePinaforeMorphism $ uuid "f06efa5e-190f-4e5d-8633-495c5683c124";
        lpropType = personType;
        lpropColumns = [personName];
    } in ListViewPinaforeProperty MkViewPinaforeListProperty{..};

    rootType :: ViewPinaforeType (WholeEdit (Maybe UUID));
    rootType = EntityViewPinaforeType [peopleCollection];

    rootValue :: ViewPinaforeValue;
    rootValue = MkViewPinaforeValue (uuid "78baed51-cb05-46b5-bcb4-49031532b890") rootType;
}
