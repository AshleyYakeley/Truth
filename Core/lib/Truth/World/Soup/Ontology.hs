module Truth.World.Soup.Ontology where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Truth.World.Soup.Schema;


    uuid :: String -> UUID;
    uuid s = fromMaybe (error $ "couldn't parse UUID " ++ show s) $ Data.UUID.fromString s;

    data ViewSoupSimpleProperty = MkViewSoupSimpleProperty
    {
        spropName :: String,
        spropMorphism :: SoupMorphism (WholeEdit (Maybe UUID)) (WholeEdit (Maybe UUID)),
        spropType :: ViewSoupType
    };

    data ViewSoupListProperty = MkViewSoupListProperty
    {
        lpropName :: String,
        lpropMorphism :: SoupMorphism (WholeEdit (Maybe UUID)) (FiniteSetEdit UUID),
        lpropType :: ViewSoupType,
        lpropColumns :: [ViewSoupProperty]
    };

    data ViewSoupProperty = SimpleViewSoupProperty ViewSoupSimpleProperty | ListViewSoupProperty ViewSoupListProperty;

    data ViewSoupPrimitive = forall edit. MkViewSoupPrimitive
    {
        primType :: TypeInfo edit
    };

    data ViewSoupType = EntityViewSoupType [ViewSoupProperty] | PrimitiveViewSoupType ViewSoupPrimitive;

    data ViewSoupValue = MkViewSoupValue UUID ViewSoupType;


    -- example ontology

    personType :: ViewSoupType;
    personType = EntityViewSoupType [personName,personMother,personFather];

    personName :: ViewSoupProperty;
    personName = let
    {
        spropName = "Name";
        spropMorphism = predicateSoupMorphism $ uuid "498260df-6a8a-44f0-b285-68a63565a33b";
        spropType = PrimitiveViewSoupType $ MkViewSoupPrimitive $ typeInfo @(StringEdit Text);
    } in SimpleViewSoupProperty MkViewSoupSimpleProperty{..};

    motherUUID :: UUID;
    motherUUID = uuid "3afce58f-b7eb-4b11-8a75-2d66afd4d085";

    fatherUUID :: UUID;
    fatherUUID = uuid "c005705f-9259-4d24-9713-db28a6e4f7d5";

    personMother :: ViewSoupProperty;
    personMother = let
    {
        spropName = "Mother";
        spropMorphism = predicateSoupMorphism motherUUID;
        spropType = personType;
    } in SimpleViewSoupProperty MkViewSoupSimpleProperty{..};

    personFather :: ViewSoupProperty;
    personFather = let
    {
        spropName = "Father";
        spropMorphism = predicateSoupMorphism fatherUUID;
        spropType = personType;
    } in SimpleViewSoupProperty MkViewSoupSimpleProperty{..};

    peopleCollection :: ViewSoupProperty;
    peopleCollection = let
    {
        lpropName = "People";
        lpropMorphism = predicateInverseSoupMorphism $ uuid "f06efa5e-190f-4e5d-8633-495c5683c124";
        lpropType = personType;
        lpropColumns = [personName];
    } in ListViewSoupProperty MkViewSoupListProperty{..};

    rootType :: ViewSoupType;
    rootType = EntityViewSoupType [peopleCollection];

    rootValue :: ViewSoupValue;
    rootValue = MkViewSoupValue (uuid "78baed51-cb05-46b5-bcb4-49031532b890") rootType;
}
