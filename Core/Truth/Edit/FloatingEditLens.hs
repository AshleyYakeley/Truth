module Truth.Edit.FloatingEditLens where
{
    import Truth.Edit.EditLens;
    import Truth.Edit.FloatingEditFunction;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data FloatingEditLens' state m edita editb = MkFloatingEditLens
    {
        floatingEditLensFunction :: FloatingEditFunction state edita editb,
        floatingEditLensPutEdit :: state -> editb -> ConstFunction (Subject edita) (m edita)
    };

    type FloatingEditLens state = FloatingEditLens' state Maybe;

    instance IsBiMap (FloatingEditLens' state) where
    {
        mapBiMapM ff felens = MkFloatingEditLens
        {
            floatingEditLensFunction = floatingEditLensFunction felens,
            floatingEditLensPutEdit = \state edit -> fmap ff (floatingEditLensPutEdit felens state edit)
        };
    };

    fixedFloatingEditLens :: EditLens edita editb -> FloatingEditLens () edita editb;
    fixedFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = fixedFloatingEditFunction (editLensFunction lens),
        floatingEditLensPutEdit = \_ -> editLensPutEdit lens
    };

    eitherWholeFloatingEditLens ::
     (state -> Subject editb -> ConstFunction (Subject edita) (Maybe (Subject edita))) ->
     FloatingEditLens state edita editb ->
     FloatingEditLens state (Either (WholeEdit (Subject edita)) edita) (Either (WholeEdit (Subject editb)) editb);
    eitherWholeFloatingEditLens pushback lens = MkFloatingEditLens
    {
        floatingEditLensFunction = eitherWholeFloatingEdit (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \state peditb -> case peditb of
        {
            Left (MkWholeEdit b) -> do
            {
                ma <- pushback state b;
                return (fmap (Left . MkWholeEdit) ma);
            };
            Right editb -> do
            {
                medita <- floatingEditLensPutEdit lens state editb;
                return (fmap Right medita);
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
            mpusha <- cofmap1CF getMaybeOne (cfmap (floatingEditLensPutEdit lens state pushb));
            return (case mpusha of
            {
                Just (Just edita) -> Just (MkJustEdit edita);
                _ -> Nothing;
            });
        }
    };

    constFunctionAp :: (FunctorOne f) => f (ConstFunction a b) -> ConstFunction (f a) (f b);
    constFunctionAp fcab = case retrieveOne fcab of
    {
        FailureResult fx -> return fx;
        SuccessResult cab -> cfmap cab;
    };

    -- suitable for Results, trying to put a failure code will be rejected
    justWholeFloatingEditLens :: forall f state edita editb. (FunctorOne f,Edit edita,FullEdit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustWholeEdit f edita) (JustWholeEdit f editb);
    justWholeFloatingEditLens lens = eitherWholeFloatingEditLens (\state fb -> fmap sequenceA (constFunctionAp (fmap (\b -> do
    {
        -- look, you're not supposed to understand this. All I know is, it has the correct type.
        medit <- floatingEditLensPutEdit lens state (replaceEdit b);
        traverse applyEdit medit;
    }) fb))) (justFloatingEditLens lens);
}
