module Pinafore.Query.Predefined(predefinedBindings) where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.AsText;
    import Pinafore.Edit;
    import Pinafore.Query.Expression;


    valSpec :: AsText val => UISpec (WholeEdit (Maybe val)) -> PinaforeLensValue (WholeEdit (Maybe Point)) -> UISpec PinaforeEdit;
    valSpec spec val = uiLens (applyPinaforeLens primitivePinaforeLensMorphism val) spec;

    predefinedBindings :: QBindings;
    predefinedBindings = mconcat
    [
        -- UI
        --qbind "uiCheckbox" $ \name lens -> (uiLens lens $ uiCheckbox name :: UISpec PinaforeEdit),
        qbind "uiTextEntry" $ valSpec $ uiNothingValue mempty uiTextEntry,
        qbind "uiTextArea" $ valSpec $ uiNothingValue mempty $ uiConvert uiTextText,
        qbind "uiLabelled" $ \text -> uiLabelled $ unpack (text :: Text),
        qbind "uiVertical" uiVertical,
        -- CSS
        -- drag
        -- icon
        qbind "uiPick" $ \(nameMorphism :: PinaforeFunctionMorphism Point (Maybe Text)) (fset :: PinaforeFunctionValue (FiniteSet Point)) -> let
        {
            getName :: PinaforeFunctionMorphism Point (Maybe Point, String);
            getName = proc p -> do
            {
                n <- nameMorphism -< p;
                returnA -< (Just p,unpack $ fromMaybe mempty n);
            };

            getNames :: PinaforeFunctionMorphism (FiniteSet Point) (FiniteSet (Maybe Point, String));
            getNames = proc fsp -> do
            {
                pairs <- cfmap getName -< fsp;
                returnA -< insertSet (Nothing,"") pairs;
            };

            opts :: GeneralFunction PinaforeEdit (ListEdit [(Maybe Point, String)] (WholeEdit (Maybe Point, String)));
            opts = (MkCloseState $ orderedKeyList @(FiniteSet (Maybe Point, String)) $ \(_,a) (_,b) -> compare a b) <.> convertGeneralFunction <.> applyPinaforeFunction getNames fset;
        } in uiOption @PinaforeEdit @(Maybe Point) opts,
        -- switch
        qbind "uiTable" $ \cols (asp :: Point -> Result String (Text,UISpec PinaforeEdit)) (val :: PinaforeLensValue (FiniteSetEdit Point)) -> let
        {
            showCell :: Maybe String -> (String,TableCellProps);
            showCell (Just s) = (s,tableCellPlain);
            showCell Nothing = ("empty",tableCellPlain{tcItalic=True});

            mapLens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeFunctionValue (String,TableCellProps);
            mapLens lens = funcGeneralFunction showCell <.> generalLensFunction (applyPinaforeLens primitivePinaforeLensMorphism lens);

            getColumn :: (Text,Point -> Result String (PinaforeLensValue (WholeEdit (Maybe Point)))) -> KeyColumn PinaforeEdit Point;
            getColumn (name,f) = readOnlyKeyColumn (unpack name) $ \p -> resultToM $ do
            {
                lens <- f p;
                return $ mapLens lens;
            };

            aspect :: Point -> IO (Maybe (String,UISpec PinaforeEdit));
            aspect point = resultToM $ fmap (return . first unpack) $ asp point;
        } in uiTable (fmap getColumn cols) aspect val
    ];
}
