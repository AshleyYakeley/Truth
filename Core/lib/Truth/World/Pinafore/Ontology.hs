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

    data ViewPinaforeItem edit where
    {
        SimpleViewPinaforeItem :: ViewPinaforeType t -> ViewPinaforeItem (WholeEdit (Maybe t));
        ReferenceViewPinaforeItem :: ViewPinaforeReference -> ViewPinaforeItem (WholeEdit (Maybe Point));
        InverseReferenceViewPinaforeItem :: ViewPinaforeType Point -> [(String,PinaforeLensMorphism Point String)] -> ViewPinaforeItem (FiniteSetEdit Point);
    };

    data ViewPinaforeProperty where
    {
        MkViewPinaforeProperty :: forall edit. (Edit edit) => String -> (PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue edit) -> ViewPinaforeItem edit -> ViewPinaforeProperty;
    };

    data ViewPinaforePrimitive t where
    {
        MkViewPinaforePrimitive :: forall t. (Serialize t) => UISpec (WholeEdit (Maybe t)) -> ViewPinaforePrimitive t;
    };

    data ViewPinaforeEntity where
    {
        MkViewPinaforeEntity :: String -> [ViewPinaforeProperty] -> ViewPinaforeEntity;
    };

    data ViewPinaforeType t where
    {
        EntityViewPinaforeType :: ViewPinaforeEntity -> ViewPinaforeType Point;
        PrimitiveViewPinaforeType :: ViewPinaforePrimitive t -> ViewPinaforeType t;
    };

    data ViewPinaforeReference where
    {
        MkViewPinaforeReference :: PinaforeLensValue (FiniteSetEdit Point) -> PinaforeFunctionMorphism Point String -> ViewPinaforeEntity -> ViewPinaforeReference;
    };

    data ViewPinaforeValue where
    {
        MkViewPinaforeValue :: forall edit. PinaforeLensValue edit -> ViewPinaforeItem edit -> ViewPinaforeValue;
    };

    type PinaforeSpec edit = PinaforeLensValue edit -> UISpec PinaforeEdit;
    simplePinaforeSpec :: Edit edit => UISpec edit -> PinaforeSpec edit;
    simplePinaforeSpec spec subjv = uiLens subjv spec;

    pinaforePropertyKeyColumn :: (String,PinaforeLensMorphism Point String) -> KeyColumn PinaforeEdit Point;
    pinaforePropertyKeyColumn (name,lens) =
        MkKeyColumn name $ \key -> return $ (funcROGeneralLens $ fromMaybe "") <.> applyPinaforeLens lens (constGeneralLens $ Just key);

    pinaforeItemSpec :: ViewPinaforeItem edit -> PinaforeSpec edit;
    pinaforeItemSpec (SimpleViewPinaforeItem ptype) subjv = pinaforeValueTypeSpec ptype subjv;
    pinaforeItemSpec (ReferenceViewPinaforeItem ptype) subjv = pinaforeRefTypeSpec ptype subjv;
    pinaforeItemSpec (InverseReferenceViewPinaforeItem ptype cols) subjv = let
    {
        getaspect :: Point -> Aspect PinaforeEdit;
        getaspect pt = return $ Just $ ("item",pinaforeValueTypeSpec ptype $ constGeneralLens $ Just pt);
    }
    in MkUISpec $ MkUITable (fmap pinaforePropertyKeyColumn cols) getaspect subjv;

    pinaforePropertySpec :: ViewPinaforeProperty -> PinaforeSpec (WholeEdit (Maybe Point));
    pinaforePropertySpec (MkViewPinaforeProperty name lens ptype) subjv = uiLabelled name $ pinaforeItemSpec ptype (lens subjv);

    pinaforeRefTypeSpec :: ViewPinaforeReference -> PinaforeSpec (WholeEdit (Maybe Point));
    pinaforeRefTypeSpec (MkViewPinaforeReference vals name (MkViewPinaforeEntity typename _)) subjv = let
    {
        getName :: PinaforeFunctionMorphism Point (Maybe Point, String);
        getName = proc p -> do
        {
            n <- name -< p;
            returnA -< (Just p,n);
        };

        getNames :: PinaforeFunctionMorphism (FiniteSet Point) (FiniteSet (Maybe Point, String));
        getNames = proc fsp -> do
        {
            pairs <- cfmap getName -< fsp;
            returnA -< insertSet (Nothing,"") pairs;
        };

        orderByName :: PinaforeFunctionValue (FiniteSet Point) -> GeneralFunction PinaforeEdit (ListEdit [(Maybe Point, String)] (WholeEdit (Maybe Point, String)));
        orderByName v = (MkCloseState $ orderedKeyList @(FiniteSet (Maybe Point, String)) $ \(_,a) (_,b) -> compare a b) <.> convertGeneralFunction <.> applyPinaforeFunction getNames v;
    } in uiDragDestination typename subjv $ uiOption (orderByName $ lensFunctionValue vals) subjv;

    pinaforeValueTypeSpec :: ViewPinaforeType t -> PinaforeSpec (WholeEdit (Maybe t));
    pinaforeValueTypeSpec (EntityViewPinaforeType (MkViewPinaforeEntity typename props)) = \subjv -> uiVertical $ (uiLens subjv $ uiDragSource typename) : fmap (\prop -> pinaforePropertySpec prop subjv) props;
    pinaforeValueTypeSpec (PrimitiveViewPinaforeType (MkViewPinaforePrimitive uispec)) = simplePinaforeSpec uispec;

    pinaforeValueSpec :: ViewPinaforeValue -> UISpec PinaforeEdit;
    pinaforeValueSpec (MkViewPinaforeValue subjv ptype) = pinaforeItemSpec ptype subjv;
}
