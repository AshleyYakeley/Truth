module Truth.Object.View where
{
    import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    data Aspect edit where
    {
        MkAspect ::
         forall edita editb state. (Eq state,Edit editb) =>
          InfoT editb -> InfoT (Subject editb) -> FloatingEditLens state edita editb -> Aspect edita;
    };

    data ViewWidgetStuff w edit = MkViewWidgetStuff
    {
        vwsWidget :: w,
        vwsGetSelection :: IO (Maybe (Aspect edit))
    };

    data ViewResult w edit = MkViewResult
    {
        vrWidgetStuff :: ViewWidgetStuff w edit,
        vrUpdate :: edit -> IO ()
    };

    type View w edit = Subject edit -> Push edit -> IO (ViewResult w edit);

    subscribeView :: View w edit -> Subscribe edit -> IO (Subscribe edit,ViewWidgetStuff w edit,IO ());
    subscribeView view subscribe = do
    {
        (vr,sub) <- subscribe view vrUpdate;
        return (subCopy sub,vrWidgetStuff vr,subClose sub);
    };
}
