module Truth.Object.View where
{
    import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    data Aspect edit where
    {
        MkAspect ::
         forall edita editb state. (Eq state,FullEdit editb) =>
          Info editb -> Info (EditSubject editb) -> FloatingEditLens state edita editb -> Aspect edita;
    };

    data ViewWidgetStuff w edit = MkViewWidgetStuff
    {
        vwsWidget :: w,
        vwsGetSelection :: IO (Maybe (Aspect edit))
    };

    data ViewResult w edit token = MkViewResult
    {
        vrWidgetStuff :: ViewWidgetStuff w edit,
        vrUpdate :: token -> edit -> IO token
    };

    type View w edit = forall token. LockAPI edit token -> IO (ViewResult w edit token,token);

    subscribeView :: View w edit -> Subscribe edit -> IO (ViewWidgetStuff w edit,IO ());
    subscribeView view subscribe = do
    {
        (vr,close) <- subscribe view vrUpdate;
        return (vrWidgetStuff vr,close);
    };
}
