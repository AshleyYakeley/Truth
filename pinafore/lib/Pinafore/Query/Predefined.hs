module Pinafore.Query.Predefined(predefinedBindings) where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.AsText;
    import Pinafore.Edit;
    import Pinafore.Query.Expression;


    valSpec :: AsText val => UISpec (WholeEdit (Maybe val)) -> PinaforeLensValue (WholeEdit (Maybe Point)) -> UISpec PinaforeEdit;
    valSpec spec val = uiLens (applyPinaforeLens primitivePinaforeLensMorphism val) spec;

    resultToM :: MonadFail m => Result String a -> m a;
    resultToM (SuccessResult a) = return a;
    resultToM (FailureResult e) = fail e;

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
        -- option
        -- switch
        qbind "uitable" $ \cols (asp :: Point -> Result String (Text,UISpec PinaforeEdit)) (val :: PinaforeLensValue (FiniteSetEdit Point)) -> let
        {
            mapLens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeLensValue (WholeEdit String);
            mapLens lens = maybeNothingGeneralLens mempty <.> applyPinaforeLens primitivePinaforeLensMorphism lens;

            getColumn :: (Text,Point -> Result String (PinaforeLensValue (WholeEdit (Maybe Point)))) -> KeyColumn PinaforeEdit Point;
            getColumn (name,f) = MkKeyColumn (unpack name) $ \p -> resultToM $ fmap mapLens $ f p;

            aspect :: Point -> IO (Maybe (String,UISpec PinaforeEdit));
            aspect point = return $ resultToM $ fmap (first unpack) $ asp point;
        } in uiTable (fmap getColumn cols) aspect val
    ];
}
