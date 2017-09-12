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

    extractOneWholeEdit :: forall m f edit. (MonadOne f,FullEdit edit,MonadIO m) => OneWholeEdit f edit -> m [edit];
    extractOneWholeEdit (SumEditRight (MkOneEdit edit)) = return [edit];
    extractOneWholeEdit (SumEditLeft (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> getReplaceEditsM a;
        _ -> return [];
    };

    oneWholeLiftEditFunction :: forall f state edita editb. (MonadOne f,Reader (EditReader edita),FullReader (EditReader editb)) =>
        EditFunction state edita editb -> EditFunction state (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeLiftEditFunction = sumWholeLiftEditFunction . oneLiftEditFunction;

    -- suitable for Results, trying to put a failure code will be rejected
    oneWholeLiftGeneralLens' :: forall ff f edita editb. (Monad ff,Traversable ff,MonadOne f,FullReader (EditReader edita),Edit edita,FullEdit editb) =>
     (forall a. f a -> ff a) ->
     GeneralLens' ff edita editb -> GeneralLens' ff (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeLiftGeneralLens' faffa (MkCloseState (lens :: EditLens' ff state edita editb)) = MkCloseState $ sumWholeLiftEditLens pushback (oneLiftEditLens faffa lens) where
    {
        ff1 :: forall a. state -> f (state,a) -> (state,f a);
        ff1 oldstate fsa = case retrieveOne fsa of
        {
            FailureResult (MkLimit fx) -> (oldstate,fx);
            SuccessResult (newstate,a) -> (newstate,fmap (\_ ->  a) fsa);
        };

        pushback :: state -> f (EditSubject editb) -> Readable (OneReader f (EditReader edita)) (ff (state,f (EditSubject edita)));
        pushback oldstate fb = case retrieveOne fb of
        {
            FailureResult (MkLimit fx) -> return $ return (oldstate,fx);

            SuccessResult b -> fmap (fmap (ff1 oldstate) . sequenceA) $ liftMaybeReadable $ do
            {
                editbs <- getReplaceEditsM b;
                fstateedita <- editLensPutEdits lens oldstate editbs;
                for fstateedita $ \(newstate,editas) -> do
                {
                    a <- mapReadable (applyEdits editas) fromReader;
                    return (newstate,a);
                };
            };
        };
    };

    oneWholeLiftGeneralLens :: forall f edita editb. (MonadOne f,FullReader (EditReader edita),Edit edita,FullEdit editb) =>
     GeneralLens edita editb -> GeneralLens (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeLiftGeneralLens = oneWholeLiftGeneralLens' getMaybeOne;
}
