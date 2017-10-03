module Truth.Core.UI.Table where
{
    import Truth.Core.Import;
    import Truth.Core.Types;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.UI.Specifier;
    import Truth.Core.UI.Lens;


    data KeyColumn tedit key = MkKeyColumn
    {
        kcName :: String,
        kcFunction :: key -> GeneralLens tedit (WholeEdit String)
    };

    data UITable tedit where
    {
        MkUITable :: forall cont tedit iedit. (IONewItemKeyContainer cont,FullSubjectReader (EditReader iedit),Edit tedit,Edit iedit,HasKeyReader cont (EditReader iedit)) =>
            [KeyColumn tedit (ContainerKey cont)] -> (ContainerKey cont -> Aspect tedit) -> GeneralLens tedit (KeyEdit cont iedit) -> UITable tedit;
    };

    uiTable :: forall cont tedit iedit. (IONewItemKeyContainer cont,FullSubjectReader (EditReader iedit),Edit tedit,Edit iedit,HasKeyReader cont (EditReader iedit)) =>
        [KeyColumn tedit (ContainerKey cont)] -> (ContainerKey cont -> Aspect tedit) -> GeneralLens tedit (KeyEdit cont iedit) -> UISpec tedit;
    uiTable cols getaspect lens = MkUISpec $ MkUITable cols getaspect lens;

    uiSimpleTable :: forall cont iedit. (IONewItemKeyContainer cont,FullSubjectReader (EditReader iedit),Edit iedit,HasKeyReader cont (EditReader iedit)) =>
        [KeyColumn (KeyEdit cont iedit) (ContainerKey cont)] -> Aspect (MaybeEdit iedit) -> UISpec (KeyEdit cont iedit);
    uiSimpleTable cols aspect = uiTable cols (\key -> mapAspect (keyElementLens key) aspect) cid;

    instance Show (UITable edit) where
    {
        show (MkUITable cols _ _) = "table (" ++ intercalate ", " (fmap kcName cols) ++ ")";
    };

    instance UIType UITable where
    {
        uiWitness = $(iowitness [t|UITable|]);
    };
}
