module Truth.Core.Types.OneWholeEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Sum;
    import Truth.Core.Types.SumWhole;
    import Truth.Core.Types.OneReader;
    import Truth.Core.Types.OneEdit;


    type OneWholeEdit (f :: * -> *) edit = SumWholeEdit (OneEdit f edit);

    type MaybeEdit edit = OneWholeEdit Maybe edit;

    extractOneWholeEdit :: forall m f edit. (MonadOne f,FullEdit edit,MonadIO m) => OneWholeEdit f edit -> m [edit];
    extractOneWholeEdit (SumEditRight (MkOneEdit edit)) = return [edit];
    extractOneWholeEdit (SumEditLeft (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> getReplaceEditsM a;
        _ -> return [];
    };

    oneWholeLiftEditFunction :: forall f state edita editb. (MonadOne f,SubjectReader (EditReader edita),FullSubjectReader (EditReader editb)) =>
        EditFunction state edita editb -> EditFunction state (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeLiftEditFunction = sumWholeLiftEditFunction . oneLiftEditFunction;

    -- suitable for Results, trying to put a failure code will be rejected
    oneWholeLiftGeneralLens :: forall f edita editb. (MonadOne f,FullSubjectReader (EditReader edita),Edit edita,FullEdit editb) =>
     GeneralLens edita editb -> GeneralLens (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeLiftGeneralLens (MkCloseState (lens :: EditLens state edita editb)) = MkCloseState $ sumWholeLiftEditLens pushback (oneLiftEditLens lens) where
    {
        ff1 :: forall a. state -> f (state,a) -> (state,f a);
        ff1 oldstate fsa = case retrieveOne fsa of
        {
            FailureResult (MkLimit fx) -> (oldstate,fx);
            SuccessResult (newstate,a) -> (newstate,fmap (\_ ->  a) fsa);
        };

        pushback :: state -> f (EditSubject editb) -> Readable (OneReader f (EditReader edita)) (Maybe (state,f (EditSubject edita)));
        pushback oldstate fb = case retrieveOne fb of
        {
            FailureResult (MkLimit fx) -> return $ return (oldstate,fx);

            SuccessResult b -> fmap (fmap (ff1 oldstate) . sequenceA) $ liftMaybeReadable $ do
            {
                editbs <- getReplaceEditsM b;
                fstateedita <- editLensPutEdits lens oldstate editbs;
                for fstateedita $ \(newstate,editas) -> do
                {
                    a <- mapReadable (applyEdits editas) subjectFromReader;
                    return (newstate,a);
                };
            };
        };
    };

    mustExistMaybeObjectFunction :: forall edit. FullEdit edit => String -> ObjectFunction (MaybeEdit edit) edit;
    mustExistMaybeObjectFunction err = let
    {
        editInitial = ();
        editGet :: () -> EditReader edit t -> Readable (OneReader Maybe (EditReader edit)) t;
        editGet () reader = do
        {
            mt <- readable $ ReadOne reader;
            case mt of
            {
                Just t -> return t;
                Nothing -> error $ err ++ ": not found";
            };
        };
        editUpdate :: MaybeEdit edit -> () -> Readable (OneReader Maybe (EditReader edit)) ((), [edit]);
        editUpdate (SumEditLeft (MkWholeEdit Nothing)) () = error $ err ++ ": deleted";
        editUpdate (SumEditLeft (MkWholeEdit (Just t))) () = do
        {
            edits <- getReplaceEditsM t;
            return $ pure edits;
        };
        editUpdate (SumEditRight (MkOneEdit edit)) () = return $ pure [edit];
    } in MkEditFunction{..};

    mustExistMaybeObjectLens :: forall edit. FullEdit edit => String -> ObjectLens (MaybeEdit edit) edit;
    mustExistMaybeObjectLens err = let
    {
        editLensFunction = mustExistMaybeObjectFunction err;
        editLensPutEdit :: () -> edit -> Readable (OneReader Maybe (EditReader edit)) (Maybe ((), [MaybeEdit edit]));
        editLensPutEdit () edit = return $ Just $ pure [SumEditRight $ MkOneEdit edit];
    } in MkEditLens{..};

    mustExistMaybeGeneralLens :: forall edit. FullEdit edit => String -> GeneralLens (MaybeEdit edit) edit;
    mustExistMaybeGeneralLens err = MkCloseState $ mustExistMaybeObjectLens err;
}
