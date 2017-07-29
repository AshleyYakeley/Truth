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

    extractOneWholeEdit :: forall c m f edit. (MonadOne f,ReadableConstraint c,GenFullEdit c edit,Monad m,c m) => OneWholeEdit f edit -> m [edit];
    extractOneWholeEdit (SumEditRight (MkOneEdit edit)) = return [edit];
    extractOneWholeEdit (SumEditLeft (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> getReplaceEditsM @c a;
        _ -> return [];
    };

    -- suitable for Results, trying to put a failure code will be rejected
    liftOneWholeGeneralLens :: forall ff f edita editb. (Monad ff,Traversable ff,MonadOne f,IOFullReader (EditReader edita),Edit edita,IOFullEdit editb) =>
     (forall a. f a -> ff a) ->
     GeneralLens' ff edita editb -> GeneralLens' ff (OneWholeEdit f edita) (OneWholeEdit f editb);
    liftOneWholeGeneralLens faffa (MkCloseFloat (lens :: IOFloatingEditLens' ff state edita editb)) = MkCloseFloat $ sumWholeLiftFloatingEditLens pushback (oneLiftFloatingEditLens faffa lens) where
    {
        ff1 :: forall a. state -> f (state,a) -> (state,f a);
        ff1 oldstate fsa = case retrieveOne fsa of
        {
            FailureResult (MkLimit fx) -> (oldstate,fx);
            SuccessResult (newstate,a) -> (newstate,fmap (\_ ->  a) fsa);
        };

        pushback :: state -> f (EditSubject editb) -> IOReadable (OneReader f (EditReader edita)) (ff (state,f (EditSubject edita)));
        pushback oldstate fb = case retrieveOne fb of
        {
            FailureResult (MkLimit fx) -> return $ return (oldstate,fx);

            SuccessResult b -> fmap (fmap (ff1 oldstate) . sequenceA) $ liftMaybeReadable $ do
            {
                editbs <- getReplaceEditsM @MonadIO b;
                fstateedita <- floatingEditLensPutEdits lens oldstate editbs;
                for fstateedita $ \(newstate,editas) -> do
                {
                    a <- mapReadable (applyEdits editas) genFromReader;
                    return (newstate,a);
                };
            };
        };
    };
}
