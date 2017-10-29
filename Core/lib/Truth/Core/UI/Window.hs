module Truth.Core.UI.Window where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Object;
    import Truth.Core.UI.Specifier;


    data UIWindow actions = forall edit. Edit edit => MkUIWindow
    {
        uiwTitle :: String,
        uiwSpec :: UISpec edit,
        uiwSubscriber :: Subscriber edit actions
    };
}
