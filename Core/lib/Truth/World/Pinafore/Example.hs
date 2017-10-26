module Truth.World.Pinafore.Example(rootValue) where
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

    typePeople :: PinaforeLensMorphism Point Point;
    typePeople = predicatePinaforeLensMorphism $ predicate "f06efa5e-190f-4e5d-8633-495c5683c124";

    personType :: PinaforeLensValue (FiniteSetEdit Point);
    personType = applyInversePinaforeLens typePeople rootPoint;

    rootPoint :: PinaforeLensValue (WholeEdit (Maybe Point));
    rootPoint = constGeneralLens $ Just $ point "78baed51-cb05-46b5-bcb4-49031532b890";


    textEntryItem :: ViewPinaforeItem (WholeEdit (Maybe String));
    textEntryItem = SimpleViewPinaforeItem $ PrimitiveViewPinaforeType $ MkViewPinaforePrimitive $ uiNothingValue "" uiTextEntry;

    peopleItem :: ViewPinaforeItem (FiniteSetEdit Point);
    peopleItem = InverseReferenceViewPinaforeItem (EntityViewPinaforeType personEntity) [("Name",personName)];

    personReferenceItem :: ViewPinaforeItem (WholeEdit (Maybe Point));
    personReferenceItem = ReferenceViewPinaforeItem $ MkViewPinaforeReference personType (fmap (fromMaybe "") $ lensFunctionMorphism personName) personEntity;

    personEntity :: ViewPinaforeEntity;
    personEntity = let
    {
        childrenLens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue (FiniteSetEdit Point);
        childrenLens subj = readOnlyGeneralLens joinGeneralFunction <.> pairJoinGeneralLenses (applyInversePinaforeLens personMother subj) (applyInversePinaforeLens personFather subj);
    } in MkViewPinaforeEntity "Person"
    [
        MkViewPinaforeProperty "Name" (applyPinaforeLens personName) textEntryItem,
        MkViewPinaforeProperty "Mother" (applyPinaforeLens personMother) personReferenceItem,
        MkViewPinaforeProperty "Father" (applyPinaforeLens personFather) personReferenceItem,
        MkViewPinaforeProperty "Children" childrenLens peopleItem
    ];

    rootItem :: ViewPinaforeItem (WholeEdit (Maybe Point));
    rootItem = SimpleViewPinaforeItem $ EntityViewPinaforeType $ MkViewPinaforeEntity "Root"
    [
        MkViewPinaforeProperty "People" (applyInversePinaforeLens typePeople) peopleItem
    ];

    rootValue :: ViewPinaforeValue;
    rootValue = MkViewPinaforeValue rootPoint rootItem;
}
