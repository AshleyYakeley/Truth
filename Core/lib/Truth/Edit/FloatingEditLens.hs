module Truth.Edit.FloatingEditLens where
{
    import Truth.Edit.EditLens;
    import Truth.Edit.FloatingEditFunction;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.Either;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.ReadFunction;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

    data FloatingEditLens' m state edita editb = MkFloatingEditLens
    {
        floatingEditLensFunction :: FloatingEditFunction state edita editb,
        -- floatingEditLensPutEdit :: state -> Readable (EditReader edita) (editb -> m (Readable (EditReader edita) edita))
        floatingEditLensPutEdit :: state -> editb -> Readable (EditReader edita) (m edita)
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
    fixedFloatingEditLens :: EditLens' m edita editb -> FloatingEditLens' m () edita editb;
    fixedFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = fixedFloatingEditFunction (editLensFunction lens),
        floatingEditLensPutEdit = \_ -> editLensPutEdit lens
    };

    instance (Applicative m,FunctorOne m) => FloatingMap (FloatingEditLens' m) where
    {
        identityFloating = fixedFloatingEditLens id;
        composeFloating fel2 fel1 = MkFloatingEditLens
        {
            floatingEditLensFunction = composeFloating (floatingEditLensFunction fel2) (floatingEditLensFunction fel1),
            floatingEditLensPutEdit = \(s1,s2) editc -> do
            {
                meditb <- mapReadable (floatingEditGet (floatingEditLensFunction fel1) s1) (floatingEditLensPutEdit fel2 s2 editc);
                case retrieveOne meditb of
                {
                    SuccessResult editb -> floatingEditLensPutEdit fel1 s1 editb;
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    eitherWholeFloatingEditLens :: (Reader (EditReader edita),FullReader (EditReader editb)) =>
     (state -> EditSubject editb -> Readable (EditReader edita) (Maybe (EditSubject edita))) ->
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
                return (fmap (LeftEdit . MkWholeEdit) ma);
            };
            RightEdit editb -> do
            {
                medita <- floatingEditLensPutEdit lens state editb;
                return (fmap RightEdit medita);
            };
        }
    };

    justFloatingEditLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustEdit f edita) (JustEdit f editb);
    justFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = justFloatingEdit (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \state (MkJustEdit pushb) -> do
        {

            -- floatingEditLensPutEdit lens state pushb :: Readable ra (Maybe edita)
            -- liftJustReadable (floatingEditLensPutEdit lens state pushb) :: Readable (JustReader f ra) (f edita);

            fpusha <- liftJustReadable (floatingEditLensPutEdit lens state pushb);
            return (case getMaybeOne fpusha of
            {
                Just (Just edita) -> Just (MkJustEdit edita);
                _ -> Nothing;
            });
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected

    justWholeFloatingEditLens :: forall f state edita editb. (FunctorOne f,FullReader (EditReader edita),Edit edita,FullEdit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustWholeEdit f edita) (JustWholeEdit f editb);
    justWholeFloatingEditLens lens = eitherWholeFloatingEditLens pushback (justFloatingEditLens lens) where
    {
        pushback :: state -> f (EditSubject editb) -> Readable (JustReader f (EditReader edita)) (Maybe (f (EditSubject edita)));
        pushback state fb = case retrieveOne fb of
        {
            FailureResult (MkLimit fx) -> return (Just fx);
            SuccessResult b -> fmap sequenceA (liftJustReadable (do
            {
                medita <- floatingEditLensPutEdit lens state (replaceEdit b);
                traverse (\edita -> mapReadable (applyEdit edita) fromReader) medita;
            }));
        };
    };
}
