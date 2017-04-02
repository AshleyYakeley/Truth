module Truth.Object.View where
{
    import Truth.Edit.Import;
    import Truth.Edit;
    import Truth.Object.Object;


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

    data ViewResult w edit userstate = MkViewResult
    {
        vrWidgetStuff :: ViewWidgetStuff w edit,
        vrUpdate :: userstate -> [edit] -> IO userstate
    };

    data View w edit = forall userstate. MkView (LockAPI edit userstate -> IO (ViewResult w edit userstate,userstate));

    subscribeView :: View w edit -> Object edit -> IO (ViewWidgetStuff w edit,IO ());
    subscribeView (MkView view) subscribe = do
    {
        (vr,close) <- subscribe view vrUpdate;
        return (vrWidgetStuff vr,close);
    };

    mapJustWholeEditAspect :: forall f edit. (MonadOne f, Edit edit,FullReader (EditReader edit)) =>
     Info f -> Aspect edit -> Maybe (Aspect (JustWholeEdit f edit));
    mapJustWholeEditAspect infoF (MkAspect infoEditB infoSubj (lens :: FloatingEditLens state edit editb)) = do
    {
        let
        {
            knowledge = mconcat [infoKnowledge infoF,infoKnowledge infoEditB,infoKnowledge infoSubj];
        };
        ValueFact (MkEditReaderInfo infoReader) <- ask knowledge $ applyInfo (info @EditReaderInfo) infoEditB;
        let
        {
            infoJustEdit = applyInfo (applyInfo (info @JustEdit) infoF) infoEditB;
            infoJustReader = applyInfo (applyInfo (info @MonadOneReader) infoF) infoReader;

            infoEditB' = applyInfo (applyInfo (info @EitherEdit) $ applyInfo (info @WholeEdit) infoJustReader) infoJustEdit;
            infoSubj' = applyInfo infoF infoSubj;
            lens' = justWholeFloatingEditLens lens;
        };
        return $ MkAspect infoEditB' infoSubj' lens';
    };
}
