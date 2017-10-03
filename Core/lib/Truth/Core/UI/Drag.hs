module Truth.Core.UI.Drag where
{
    import Truth.Core.Import;
    import Truth.Core.Types;
    import Truth.Core.UI.Specifier;


    data UIDragSource edit where
    {
        MkUIDragSource :: Serialize t => String -> UIDragSource (WholeEdit t);
    };

    instance Show (UIDragSource edit) where
    {
        show (MkUIDragSource typename) = "drag-source " ++ typename;
    };

    instance UIType UIDragSource where
    {
        uiWitness = $(iowitness [t|UIDragSource|]);
    };

    uiDragSource :: Serialize t => String -> UISpec (WholeEdit t);
    uiDragSource datatype = MkUISpec $ MkUIDragSource datatype;

    data UIDragDestination edit where
    {
        MkUIDragDestination :: Serialize t => String -> UIDragDestination (WholeEdit t);
    };

    instance Show (UIDragDestination edit) where
    {
        show (MkUIDragDestination typename) = "drag-destination " ++ typename;
    };

    instance UIType UIDragDestination where
    {
        uiWitness = $(iowitness [t|UIDragDestination|]);
    };

    uiDragDestination :: Serialize t => String -> UISpec (WholeEdit t);
    uiDragDestination datatype = MkUISpec $ MkUIDragDestination datatype;
}
