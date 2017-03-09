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

    data View w edit = forall token. MkView (LockAPI edit token -> IO (ViewResult w edit token,token));

    subscribeView :: View w edit -> Object edit -> IO (ViewWidgetStuff w edit,IO ());
    subscribeView (MkView view) subscribe = do
    {
        (vr,close) <- subscribe view vrUpdate;
        return (vrWidgetStuff vr,close);
    };

    mapJustWholeEditAspect :: forall f edit. (FunctorOne f, Edit edit,FullReader (EditReader edit)) =>
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
            infoJustReader = applyInfo (applyInfo (info @JustReader) infoF) infoReader;

            infoEditB' = applyInfo (applyInfo (info @EitherEdit) $ applyInfo (info @WholeEdit) infoJustReader) infoJustEdit;
            infoSubj' = applyInfo infoF infoSubj;
            lens' = justWholeFloatingEditLens lens;
        };
        return $ MkAspect infoEditB' infoSubj' lens';
    };
}
