module Truth.World.Pinafore.Example where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Truth.World.Pinafore.Edit;
    import Truth.World.Pinafore.Ontology;


    -- example ontology

    personName :: PinaforeLensMorphism Point String;
    personName = primitivePinaforeLensMorphism . predicatePinaforeLensMorphism (predicate "498260df-6a8a-44f0-b285-68a63565a33b");

    personMother :: PinaforeLensMorphism Point Point;
    personMother = predicatePinaforeLensMorphism $ predicate "3afce58f-b7eb-4b11-8a75-2d66afd4d085";

    personFather :: PinaforeLensMorphism Point Point;
    personFather = predicatePinaforeLensMorphism $ predicate "c005705f-9259-4d24-9713-db28a6e4f7d5";

    startPoint :: Maybe Point;
    startPoint = Just $ point "78baed51-cb05-46b5-bcb4-49031532b890";

    typePeople :: PinaforeLensMorphism Point Point;
    typePeople = predicatePinaforeLensMorphism $ predicate "f06efa5e-190f-4e5d-8633-495c5683c124";

    personType :: PinaforeLensValue (FiniteSetEdit Point);
    personType = applyInversePinaforeLens typePeople (constGeneralLens startPoint);


    personTypeView :: ViewPinaforeType Point;
    personTypeView = EntityViewPinaforeType personEntity;

    textEntryTypeView :: ViewPinaforeType String;
    textEntryTypeView = PrimitiveViewPinaforeType $ MkViewPinaforePrimitive $ uiNothingValue "" $ MkUISpec MkUITextEntry;

    personEntity :: ViewPinaforeEntity;
    personEntity = let
    {
        personNameProperty :: ViewPinaforeProperty;
        personNameProperty = MkViewPinaforeProperty (applyPinaforeLens personName) $ SimpleViewPinaforeItem textEntryTypeView;

        personMotherProperty :: ViewPinaforeProperty;
        personMotherProperty = let
        {
            getlens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue (WholeEdit (Maybe Point));
            getlens = applyPinaforeLens personMother;

            item :: ViewPinaforeItem (WholeEdit (Maybe Point));
            item = ReferenceViewPinaforeItem $ MkViewPinaforeReference personType (fmap (fromMaybe "") $ lensFunctionMorphism personName) personEntity;
        } in MkViewPinaforeProperty getlens item;

        personFatherProperty :: ViewPinaforeProperty;
        personFatherProperty = let
        {
            getlens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue (WholeEdit (Maybe Point));
            getlens = applyPinaforeLens personFather;

            item :: ViewPinaforeItem (WholeEdit (Maybe Point));
            item = ReferenceViewPinaforeItem $ MkViewPinaforeReference personType (fmap (fromMaybe "") $ lensFunctionMorphism personName) personEntity;
        } in MkViewPinaforeProperty getlens item;

        personChildrenProperty :: ViewPinaforeProperty;
        personChildrenProperty = let
        {
            getlens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue (FiniteSetEdit Point);
            getlens subj = readOnlyGeneralLens joinGeneralFunction <.> pairJoinGeneralLenses (applyInversePinaforeLens personMother subj) (applyInversePinaforeLens personFather subj);

            item :: ViewPinaforeItem (FiniteSetEdit Point);
            item = InverseReferenceViewPinaforeItem personTypeView [("Name",personName)];
        } in MkViewPinaforeProperty getlens item;
    } in MkViewPinaforeEntity "Person" [("Name",personNameProperty),("Mother",personMotherProperty),("Father",personFatherProperty),("Children",personChildrenProperty)];

    peopleCollection :: ViewPinaforeProperty;
    peopleCollection = let
    {
        getlens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue (FiniteSetEdit Point);
        getlens = applyInversePinaforeLens typePeople;

        item :: ViewPinaforeItem (FiniteSetEdit Point);
        item = InverseReferenceViewPinaforeItem personTypeView [("Name",personName)];
    } in MkViewPinaforeProperty getlens item;

    rootType :: ViewPinaforeType Point;
    rootType = EntityViewPinaforeType $ MkViewPinaforeEntity "Root" [("People",peopleCollection)];

    rootValue :: ViewPinaforeValue;
    rootValue = MkViewPinaforeValue startPoint rootType;
}
