module Truth.Core.Types.OneWholeEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Either;
    import Truth.Core.Types.EitherWhole;
    import Truth.Core.Types.OneReader;
    import Truth.Core.Types.OneEdit;


    type OneWholeEdit (f :: * -> *) edit = EitherWholeEdit (OneEdit f edit);

    extractOneWholeEdit :: forall f edit. (MonadOne f,FullEdit edit) => OneWholeEdit f edit -> [edit];
    extractOneWholeEdit (RightEdit (MkOneEdit edit)) = return edit;
    extractOneWholeEdit (LeftEdit (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> fromReadable replaceEdit a;
        _ -> [];
    };

    oneWholeFloatingEditFunction :: forall f state edita editb. (MonadOne f,Edit edita,Edit editb,FullReader (EditReader editb)) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeFloatingEditFunction lens = eitherWholeFloatingEditFunction (oneFloatingEditFunction lens);
    -- suitable for Results, trying to put a failure code will be rejected

    oneWholeFloatingEditLens :: forall f state edita editb. (MonadOne f,FullReader (EditReader edita),Edit edita,FullEdit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (OneWholeEdit f edita) (OneWholeEdit f editb);
    oneWholeFloatingEditLens lens = eitherWholeFloatingEditLens pushback (oneFloatingEditLens lens) where
    {
        ff1 :: forall a. state -> f (state,a) -> (state,f a);
        ff1 oldstate fsa = case retrieveOne fsa of
        {
            FailureResult (MkLimit fx) -> (oldstate,fx);
            SuccessResult (newstate,a) -> (newstate,fmap (\_ ->  a) fsa);
        };

    -- floatingEditLensPutEdits :: FloatingEditLens' Maybe state edita editb -> state -> [editb] -> Readable (EditReader edita) (Maybe (state,[edita]));

        pushback :: state -> f (EditSubject editb) -> Readable (OneReader f (EditReader edita)) (Maybe (state,f (EditSubject edita)));
        pushback oldstate fb = case retrieveOne fb of
        {
            FailureResult (MkLimit fx) -> return $ return (oldstate,fx);

            SuccessResult b -> fmap (fmap (ff1 oldstate) . sequenceA) $ liftMaybeReadable $ do
            {
                mstateedita <- floatingEditLensPutEdits lens oldstate (fromReadable replaceEdit b);
                case mstateedita of
                {
                    Nothing -> return Nothing;
                    Just (newstate,editas) -> do
                    {
                        a <- mapReadable (applyEdits editas) fromReader;
                        return $ Just (newstate,a);
                    };
                };
                -- traverse (\edita -> mapReadable (applyEdit edita) fromReader) mstateedita;
            };
        };
    };
}
