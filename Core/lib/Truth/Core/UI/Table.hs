module Truth.Core.UI.Table where
{
    import Truth.Core.Import;
    import Truth.Core.Types;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.UI.Specifier;
    import Truth.Core.UI.Lens;


    data KeyColumn edit = MkKeyColumn
    {
        kcName :: String,
        kcFunction :: ObjectFunction edit (WholeEdit String)
    };

    mapKeyColumn :: (Edit edita,Edit editb) => ObjectFunction edita editb -> KeyColumn editb -> KeyColumn edita;
    mapKeyColumn ff (MkKeyColumn n f) = MkKeyColumn n $ f <.> ff;

    data UIContextTable edit where
    {
        MkUIContextTable :: forall cont cedit iedit. (IONewItemKeyContainer cont,FullReader (EditReader iedit),Edit cedit,Edit iedit,HasKeyReader cont (EditReader iedit)) =>
            [KeyColumn (ContextEdit cedit iedit)] -> Aspect (ContextEdit cedit (OneWholeEdit Maybe iedit)) -> UIContextTable (ContextEdit cedit (KeyEdit cont iedit));
    };

    uiTableToContext :: forall cont iedit. (IONewItemKeyContainer cont,FullReader (EditReader iedit),HasKeyReader cont (EditReader iedit),Edit iedit) =>
        [KeyColumn iedit] -> Aspect (OneWholeEdit Maybe iedit) -> UISpec (KeyEdit cont iedit);
    uiTableToContext cols aspect = let
    {
        cols' = fmap (mapKeyColumn $ tupleObjectFunction EditContent) cols;
        aspect' = mapAspect contentLens aspect;
    } in MkUISpec $ MkUILens nullContextGeneralLens $ MkUISpec $ MkUIContextTable cols' aspect';

    instance Show (UIContextTable edit) where
    {
        show (MkUIContextTable cols aspect) = "context-table (" ++ intercalate ", " (fmap kcName cols) ++ ") " ++ show aspect;
    };

    instance UIType UIContextTable where
    {
        uiWitness = $(iowitness [t|UIContextTable|]);
    };
}
