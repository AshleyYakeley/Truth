module Truth.Edit.FloatingEditLens where
{
    import Truth.Edit.EditLens;
    import Truth.Edit.FloatingEditFunction;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.Either;
    import Truth.Edit.MaybeReader;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Read;
    import Truth.Edit.Import;


    data FloatingEditLens' m state edita editb = MkFloatingEditLens
    {
        floatingEditLensFunction :: FloatingEditFunction state edita editb,
        floatingEditLensPutEdit :: state -> editb -> Readable (EditReader edita) (m (state,edita))
    };

    floatingEditLensPutEdits :: (Monad m,Traversable m) => FloatingEditLens' m state edita editb -> state -> [editb] -> Readable (EditReader edita) (m (state,[edita]));
    floatingEditLensPutEdits _ oldstate [] = return $ pure $ (oldstate,[]);
    floatingEditLensPutEdits lens oldstate (e:ee) = getCompose $ do
    {
        (midstate,ea) <- MkCompose $ floatingEditLensPutEdit lens oldstate e;
        (newstate,eea) <- MkCompose $ floatingEditLensPutEdits lens midstate ee;
        return (newstate,ea:eea);
    };

    floatingEditLensAllowed :: (FunctorOne m) =>
     FloatingEditLens' m state edita editb -> state -> editb -> Readable (EditReader edita) Bool;
    floatingEditLensAllowed lens state editb = do
    {
        medita <- floatingEditLensPutEdit lens state editb;
        return (isJust (getMaybeOne medita));
    };

    type FloatingEditLens = FloatingEditLens' Maybe;
{-
    instance IsBiMap (FloatingEditLens' state) where
    {
        mapBiMapM ff felens = MkFloatingEditLens
        {
            floatingEditLensFunction = floatingEditLensFunction felens,
            floatingEditLensPutEdit = \state edit -> fmap ff (floatingEditLensPutEdit felens state edit)
        };
    };
-}
    fixedFloatingEditLens :: Functor m => EditLens' m edita editb -> FloatingEditLens' m () edita editb;
    fixedFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = fixedFloatingEditFunction (editLensFunction lens),
        floatingEditLensPutEdit = \state edit -> fmap (fmap ((,) state)) $ editLensPutEdit lens edit
    };

    instance (Applicative m,FunctorOne m) => FloatingMap (FloatingEditLens' m) where
    {
        identityFloating = fixedFloatingEditLens id;
        composeFloating fel2 fel1 = MkFloatingEditLens
        {
            floatingEditLensFunction = composeFloating (floatingEditLensFunction fel2) (floatingEditLensFunction fel1),
            floatingEditLensPutEdit = \(olds1,olds2) editc -> do
            {
                meditb <- mapReadable (floatingEditGet (floatingEditLensFunction fel1) olds1) (floatingEditLensPutEdit fel2 olds2 editc);
                case retrieveOne meditb of
                {
                    SuccessResult (news2,editb) -> do
                    {
                        mn1ea <- floatingEditLensPutEdit fel1 olds1 editb;
                        return $ fmap (\(news1,edita) -> ((news1,news2),edita)) mn1ea;
                    };
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    eitherWholeFloatingEditLens :: (Reader (EditReader edita),FullReader (EditReader editb)) =>
     (state -> EditSubject editb -> Readable (EditReader edita) (Maybe (state,EditSubject edita))) ->
     FloatingEditLens state edita editb ->
     FloatingEditLens state (EitherWholeEdit edita) (EitherWholeEdit editb);
    eitherWholeFloatingEditLens pushback lens = MkFloatingEditLens
    {
        floatingEditLensFunction = eitherWholeFloatingEdit (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \state peditb -> case peditb of
        {
            LeftEdit (MkWholeEdit b) -> do
            {
                ma <- pushback state b;
                return $ fmap (fmap (LeftEdit . MkWholeEdit)) ma;
            };
            RightEdit editb -> do
            {
                mstateedita <- floatingEditLensPutEdit lens state editb;
                return $ fmap (fmap RightEdit) mstateedita;
            };
        }
    };

    justFloatingEditLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustEdit f edita) (JustEdit f editb);
    justFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = justFloatingEdit (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \oldstate (MkJustEdit pushb) -> do
        {

            -- floatingEditLensPutEdit lens state pushb :: Readable ra (Maybe (state,edita))
            -- liftJustReadable (floatingEditLensPutEdit lens state pushb) :: Readable (MaybeReader f ra) (f edita);

            fpusha <- liftJustReadable (floatingEditLensPutEdit lens oldstate pushb);
            return $ case getMaybeOne fpusha of
            {
                Just (Just (newstate,edita)) -> Just (newstate,MkJustEdit edita);
                _ -> Nothing;
            };
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected

    justWholeFloatingEditLens :: forall f state edita editb. (FunctorOne f,FullReader (EditReader edita),Edit edita,FullEdit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustWholeEdit f edita) (JustWholeEdit f editb);
    justWholeFloatingEditLens lens = eitherWholeFloatingEditLens pushback (justFloatingEditLens lens) where
    {
        ff1 :: forall a. state -> f (state,a) -> (state,f a);
        ff1 oldstate fsa = case retrieveOne fsa of
        {
            FailureResult (MkLimit fx) -> (oldstate,fx);
            SuccessResult (newstate,a) -> (newstate,fmap (\_ ->  a) fsa);
        };

    -- floatingEditLensPutEdits :: FloatingEditLens' Maybe state edita editb -> state -> [editb] -> Readable (EditReader edita) (Maybe (state,[edita]));

        pushback :: state -> f (EditSubject editb) -> Readable (MaybeReader f (EditReader edita)) (Maybe (state,f (EditSubject edita)));
        pushback oldstate fb = case retrieveOne fb of
        {
            FailureResult (MkLimit fx) -> return $ return (oldstate,fx);

            SuccessResult b -> fmap (fmap (ff1 oldstate) . sequenceA) $ liftJustReadable $ do
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
