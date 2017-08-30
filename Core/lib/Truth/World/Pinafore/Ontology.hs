module Truth.World.Pinafore.Ontology where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Truth.World.Pinafore.Edit;


    uuid :: String -> UUID;
    uuid s = fromMaybe (error $ "couldn't parse UUID " ++ show s) $ Data.UUID.fromString s;

    data ViewPinaforeSimpleProperty = MkViewPinaforeSimpleProperty
    {
        spropName :: String,
        spropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) (WholeEdit (Maybe UUID)),
        spropType :: ViewPinaforeType
    };

    data ViewPinaforeListProperty = MkViewPinaforeListProperty
    {
        lpropName :: String,
        lpropMorphism :: PinaforeMorphism (WholeEdit (Maybe UUID)) (FiniteSetEdit UUID),
        lpropType :: ViewPinaforeType,
        lpropColumns :: [ViewPinaforeProperty]
    };

    data ViewPinaforeProperty = SimpleViewPinaforeProperty ViewPinaforeSimpleProperty | ListViewPinaforeProperty ViewPinaforeListProperty;

    data ViewPinaforePrimitive = forall edit. Serialize (EditSubject edit) => MkViewPinaforePrimitive
    {
        primType :: TypeInfo edit
    };

    data ViewPinaforeType = EntityViewPinaforeType [ViewPinaforeProperty] | PrimitiveViewPinaforeType ViewPinaforePrimitive;

    data ViewPinaforeValue = MkViewPinaforeValue UUID ViewPinaforeType;


    -- example ontology

    personType :: ViewPinaforeType;
    personType = EntityViewPinaforeType [personName,personMother,personFather,personChildren];

    personName :: ViewPinaforeProperty;
    personName = let
    {
        spropName = "Name";
        spropMorphism = predicatePinaforeMorphism $ uuid "498260df-6a8a-44f0-b285-68a63565a33b";
        spropType = PrimitiveViewPinaforeType $ MkViewPinaforePrimitive $ typeInfo @(StringEdit Text);
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    motherUUID :: UUID;
    motherUUID = uuid "3afce58f-b7eb-4b11-8a75-2d66afd4d085";

    fatherUUID :: UUID;
    fatherUUID = uuid "c005705f-9259-4d24-9713-db28a6e4f7d5";

    personMother :: ViewPinaforeProperty;
    personMother = let
    {
        spropName = "Mother";
        spropMorphism = predicatePinaforeMorphism motherUUID;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personFather :: ViewPinaforeProperty;
    personFather = let
    {
        spropName = "Father";
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

    rootType :: ViewPinaforeType;
    rootType = EntityViewPinaforeType [peopleCollection];

    rootValue :: ViewPinaforeValue;
    rootValue = MkViewPinaforeValue (uuid "78baed51-cb05-46b5-bcb4-49031532b890") rootType;
}
