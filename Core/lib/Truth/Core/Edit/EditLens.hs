module Truth.Core.Edit.EditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;
    import Truth.Core.Edit.EditFunction;


    data EditLens state edita editb = MkEditLens
    {
        editLensFunction :: EditFunction state edita editb,
        editLensPutEdit :: state -> editb -> Readable (EditReader edita) (Maybe (state,[edita]))
    };

    editLensPutEdits :: Edit edita => EditLens state edita editb -> state -> [editb] -> Readable (EditReader edita) (Maybe (state,[edita]));
    editLensPutEdits _ oldstate [] = return $ pure $ (oldstate,[]);
    editLensPutEdits lens oldstate (e:ee) = getCompose $ do
    {
        (midstate,ea) <- Compose $ editLensPutEdit lens oldstate e;
        Compose $ mapReadable (applyEdits ea) $ getCompose $ do
        {
            (newstate,eea) <- Compose $ editLensPutEdits lens midstate ee;
            return (newstate,ea ++ eea);
        };
    };

    editLensAllowed :: EditLens state edita editb -> state -> editb -> Readable (EditReader edita) Bool;
    editLensAllowed lens st editb = do
    {
        medita <- editLensPutEdit lens st editb;
        return (isJust (getMaybeOne medita));
    };

    type PureEditLens = EditLens ();

    instance ConstrainedCategory PureEditLens where
    {
        type CategoryConstraint PureEditLens t = Edit t;
        cid = let
        {
            editLensFunction = cid;
            editLensPutEdit () edit = pure $ pure ((),[edit]);
        } in MkEditLens{..};
        fel2 <.> fel1 = MkEditLens
        {
            editLensFunction = editLensFunction fel2 <.> editLensFunction fel1,
            editLensPutEdit = \() editc -> do
            {
                meditb <- mapGenReadable (editGet (editLensFunction fel1) ()) (editLensPutEdit fel2 () editc);
                case retrieveOne meditb of
                {
                    SuccessResult ((),editbs) -> editLensPutEdits fel1 () editbs;
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    instance StateCategory EditLens where
    {
        identityState = let
        {
            editLensFunction = identityState;
            editLensPutEdit st edit = pure $ pure (st,[edit]);
        } in MkEditLens{..};
        composeState fel2 fel1 = MkEditLens
        {
            editLensFunction = composeState (editLensFunction fel2) (editLensFunction fel1),
            editLensPutEdit = \(olds1,olds2) editc -> do
            {
                meditb <- mapGenReadable (editGet (editLensFunction fel1) olds1) (editLensPutEdit fel2 olds2 editc);
                case retrieveOne meditb of
                {
                    SuccessResult (news2,editbs) -> do
                    {
                        mn1ea <- editLensPutEdits fel1 olds1 editbs;
                        return $ fmap (\(news1,edita) -> ((news1,news2),edita)) mn1ea;
                    };
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    readOnlyEditLens :: forall state edita editb. EditFunction state edita editb -> EditLens state edita editb;
    readOnlyEditLens editLensFunction = let
    {
        editLensPutEdit _ _ = pure Nothing;
    } in MkEditLens{..};

    constEditLens :: forall edita editb. SubjectReader (EditReader editb) => EditSubject editb -> PureEditLens edita editb;
    constEditLens b = readOnlyEditLens $ constEditFunction b;

    convertEditLens :: forall edita editb. (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) =>
        PureEditLens edita editb;
    convertEditLens = let
    {
        editLensFunction :: PureEditFunction edita editb;
        editLensFunction = convertEditFunction;
        editLensPutEdit :: () -> editb -> Readable (EditReader edita) (Maybe ((),[edita]));
        editLensPutEdit () editb = do
        {
            newsubject <- fromReadFunctionM (applyEdit editb) subjectFromReader;
            editbs <- getReplaceEditsM newsubject;
            return $ pure $ ((),editbs);
        };
    } in MkEditLens{..};
}
