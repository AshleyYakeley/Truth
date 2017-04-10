module Truth.Core.Object.View where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.Object;


    data Aspect edit where
    {
        MkAspect ::
         forall edita editb. (FullEdit editb) =>
          Info editb -> Info (EditSubject editb) -> GeneralLens edita editb -> Aspect edita;
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

    mapOneWholeEditAspect :: forall f edit. (MonadOne f, Edit edit,FullReader (EditReader edit)) =>
     Info f -> Aspect edit -> Maybe (Aspect (OneWholeEdit f edit));
    mapOneWholeEditAspect infoF (MkAspect infoEditB infoSubj lens) = do
    {
        let
        {
            knowledge = mconcat [infoKnowledge infoF,infoKnowledge infoEditB,infoKnowledge infoSubj];
        };
        ValueFact (MkEditReaderInfo infoReader) <- ask knowledge $ applyInfo (info @EditReaderInfo) infoEditB;
        let
        {
            infoOneEdit = applyInfo (applyInfo (info @OneEdit) infoF) infoEditB;
            infoJustReader = applyInfo (applyInfo (info @OneReader) infoF) infoReader;

            infoEditB' = applyInfo (applyInfo (info @SumEdit) $ applyInfo (info @WholeEdit) infoJustReader) infoOneEdit;
            infoSubj' = applyInfo infoF infoSubj;
            lens' = oneWholeGeneralLens lens;
        };
        return $ MkAspect infoEditB' infoSubj' lens';
    };
}
