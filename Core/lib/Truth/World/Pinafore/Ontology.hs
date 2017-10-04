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
        spropMorphism :: PinaforeLens Point t,
        spropType :: ViewPinaforeType t
    };

    data ViewPinaforeListProperty = MkViewPinaforeListProperty
    {
        lpropMorphism :: PinaforeInverseLens Point Point,
        lpropType :: ViewPinaforeType Point,
        lpropColumns :: [(String,ViewPinaforeSimpleProperty String)]
    };

    data ViewPinaforeProperty = forall t. () => SimpleViewPinaforeProperty (ViewPinaforeSimpleProperty t) | ListViewPinaforeProperty ViewPinaforeListProperty;

    data ViewPinaforePrimitive t where
    {
        MkViewPinaforePrimitive :: forall t. (Serialize t) => UISpec (WholeEdit (Maybe t)) -> ViewPinaforePrimitive t;
    };

    data ViewPinaforeType t where
    {
        EntityViewPinaforeType :: String -> [(String,ViewPinaforeProperty)] -> ViewPinaforeType Point;
        PrimitiveViewPinaforeType :: ViewPinaforePrimitive t -> ViewPinaforeType t;
    };

    data ViewPinaforeValue = forall t. MkViewPinaforeValue (Maybe t) (ViewPinaforeType t);


    type PinaforeSpec edit = PinaforeValue edit -> UISpec PinaforeEdit;
    simplePinaforeSpec :: Edit edit => UISpec edit -> PinaforeSpec edit;
    simplePinaforeSpec spec subjv =  mkUILens subjv spec;

    pinaforePropertyKeyColumn :: (String,ViewPinaforeSimpleProperty String) -> KeyColumn PinaforeEdit Point;
    pinaforePropertyKeyColumn (name,MkViewPinaforeSimpleProperty lens _ptype) =
        MkKeyColumn name $ \key -> return $ (funcROGeneralLens $ fromMaybe "<empty>") <.> lens (constGeneralLens $ Just key);

    pinaforePropertySpec :: ViewPinaforeProperty -> PinaforeSpec (WholeEdit (Maybe Point));
    pinaforePropertySpec (SimpleViewPinaforeProperty (MkViewPinaforeSimpleProperty lens ptype)) subjv = pinaforeRefTypeSpec ptype (lens subjv);
    pinaforePropertySpec (ListViewPinaforeProperty (MkViewPinaforeListProperty lens ptype cols)) subjv = let
    {
        getaspect :: Point -> Aspect PinaforeEdit;
        getaspect pt = return $ Just $ ("item",pinaforeValueTypeSpec ptype $ constGeneralLens $ Just pt);
    }
    in MkUISpec $ MkUITable (fmap pinaforePropertyKeyColumn cols) getaspect $ lens subjv;

    pinaforeRefTypeSpec :: ViewPinaforeType t -> PinaforeSpec (WholeEdit (Maybe t));
    pinaforeRefTypeSpec (EntityViewPinaforeType typename _) = simplePinaforeSpec $ MkUISpec $ MkUIDragDestination typename;
    pinaforeRefTypeSpec (PrimitiveViewPinaforeType (MkViewPinaforePrimitive uispec)) = simplePinaforeSpec uispec;

    pinaforeValueTypeSpec :: ViewPinaforeType t -> PinaforeSpec (WholeEdit (Maybe t));
    pinaforeValueTypeSpec (EntityViewPinaforeType typename props) = \subjv -> MkUISpec $ MkUIVertical $ (mkUILens subjv $ MkUISpec $ MkUIDragSource typename) : fmap (\(name,prop) -> mkUILabelled name $ pinaforePropertySpec prop subjv) props;
    pinaforeValueTypeSpec (PrimitiveViewPinaforeType (MkViewPinaforePrimitive uispec)) = simplePinaforeSpec uispec;

    pinaforeValueSpec :: ViewPinaforeValue -> UISpec PinaforeEdit;
    pinaforeValueSpec (MkViewPinaforeValue value tp) = pinaforeValueTypeSpec tp $ constGeneralLens value;


    -- example ontology

    personType :: ViewPinaforeType Point;
    personType = EntityViewPinaforeType "Person" [("Name",SimpleViewPinaforeProperty personName),("Mother",personMother),("Father",personFather),("Children",personChildren)];

    personName :: ViewPinaforeSimpleProperty String;
    personName = let
    {
        spropMorphism :: PinaforeLens Point String;
        spropMorphism subj = primitivePinaforeLens $ predicatePinaforeLens (predicate "498260df-6a8a-44f0-b285-68a63565a33b") subj;
        spropType :: ViewPinaforeType String;
        spropType = PrimitiveViewPinaforeType $ MkViewPinaforePrimitive $ mkUILens convertGeneralLens $ mkUIMaybe Nothing $ MkUISpec MkUITextEntry;
    } in MkViewPinaforeSimpleProperty{..};

    motherPredicate :: Predicate;
    motherPredicate = predicate "3afce58f-b7eb-4b11-8a75-2d66afd4d085";

    fatherPredicate :: Predicate;
    fatherPredicate = predicate "c005705f-9259-4d24-9713-db28a6e4f7d5";

    personMother :: ViewPinaforeProperty;
    personMother = let
    {
        spropMorphism :: PinaforeEditLens (WholeEdit (Maybe Point)) (WholeEdit (Maybe Point));
        spropMorphism = predicatePinaforeLens motherPredicate;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personFather :: ViewPinaforeProperty;
    personFather = let
    {
        spropMorphism :: PinaforeEditLens (WholeEdit (Maybe Point)) (WholeEdit (Maybe Point));
        spropMorphism = predicatePinaforeLens fatherPredicate;
        spropType = personType;
    } in SimpleViewPinaforeProperty MkViewPinaforeSimpleProperty{..};

    personChildren :: ViewPinaforeProperty;
    personChildren = let
    {
        lpropMorphism :: PinaforeInverseLens Point Point;
        lpropType :: ViewPinaforeType Point;
        lpropColumns :: [(String,ViewPinaforeSimpleProperty String)];
        lpropMorphism subj = readOnlyGeneralLens joinEditFunction <.> pairJoinGeneralLenses (predicateInversePinaforeLens motherPredicate subj) (predicateInversePinaforeLens fatherPredicate subj);
        lpropType = personType;
        lpropColumns = [("Name",personName)];
    } in ListViewPinaforeProperty MkViewPinaforeListProperty{..};

    peopleCollection :: ViewPinaforeProperty;
    peopleCollection = let
    {
        --lpropName = "People";
        lpropMorphism = predicateInversePinaforeLens $ predicate "f06efa5e-190f-4e5d-8633-495c5683c124";
        lpropType = personType;
        lpropColumns = [("Name",personName)];
    } in ListViewPinaforeProperty MkViewPinaforeListProperty{..};

    rootType :: ViewPinaforeType Point;
    rootType = EntityViewPinaforeType "Root" [("People",peopleCollection)];

    rootValue :: ViewPinaforeValue;
    rootValue = MkViewPinaforeValue (Just $ point "78baed51-cb05-46b5-bcb4-49031532b890") rootType;
}
