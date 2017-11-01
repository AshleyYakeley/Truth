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
        --qbind "uiCheckbox" $ \name lens -> (uiLens lens $ uiCheckbox name :: UISpec PinaforeEdit),
        qbind "uiTextEntry" $ valSpec $ uiNothingValue mempty uiTextEntry,
        qbind "uiTextArea" $ valSpec $ uiNothingValue mempty $ uiConvert uiTextText,
        qbind "uiLabelled" $ \text -> uiLabelled $ unpack (text :: Text),
        qbind "uiVertical" uiVertical
        -- CSS
        -- drag
        -- icon
        -- option
        -- switch
        -- table
    ];
}
