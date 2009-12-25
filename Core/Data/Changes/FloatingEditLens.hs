module Data.Changes.FloatingEditLens where
{
    import Data.Changes.JustWholeEdit;
    import Data.Changes.JustEdit;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Lens;
    import Data.FunctorOne;
    import Data.ConstFunction;
    import Data.Chain;

    data FloatingEditLens' m state edita editb = MkFloatingLens
    {
        floatingLensInitial :: state,
        floatingLensSimple :: state -> Lens' m (Subject edita) (Subject editb),
        floatingLensUpdate :: edita -> state -> ConstFunction (Subject edita) (state,Maybe editb),
        floatingLensPutEdit :: state -> editb -> ConstFunction (Subject edita) (m edita)    -- m failure means impossible
    };

    floatingLensGet :: FloatingEditLens' m state edita editb -> state -> Subject edita -> Subject editb;
    floatingLensGet lens state = lensGet (floatingLensSimple lens state);

    floatingLensPutback :: FloatingEditLens' m state edita editb -> state -> Subject editb -> ConstFunction (Subject edita) (m (Subject edita));
    floatingLensPutback lens state = lensPutback (floatingLensSimple lens state);

    type FloatingEditLens = FloatingEditLens' Maybe;

    toFloatingLens :: (FunctorOne m) => FloatingEditLens' m state edita editb -> FloatingEditLens state edita editb;
    toFloatingLens lens = MkFloatingLens
    {
        floatingLensInitial = floatingLensInitial lens,
        floatingLensSimple = \state -> toLens (floatingLensSimple lens state),
        floatingLensUpdate = floatingLensUpdate lens,
        floatingLensPutEdit = \state edit -> fmap getMaybeOne (floatingLensPutEdit lens state edit)
    };

    fullLens ::
     FloatingEditLens state edita editb ->
     FloatingEditLens state (Either (WholeEdit (Subject edita)) edita) (Either (WholeEdit (Subject editb)) editb);
    fullLens lens = MkFloatingLens
    {
        floatingLensInitial = floatingLensInitial lens,
        floatingLensSimple = floatingLensSimple lens,
        floatingLensUpdate = \pedita oldstate -> case pedita of
        {
            Left (MkWholeEdit a) -> return (oldstate,Just (Left (MkWholeEdit (floatingLensGet lens oldstate a))));
            Right edita -> do
            {
                (newstate,meditb) <- floatingLensUpdate lens edita oldstate;
                return (newstate,fmap Right meditb);
            };
        },
        floatingLensPutEdit = \state peditb -> case peditb of
        {
            Left (MkWholeEdit b) -> do
            {
                ma <- floatingLensPutback lens state b;
                return (fmap (Left . MkWholeEdit) ma);
            };
            Right editb -> do
            {
                medita <- floatingLensPutEdit lens state editb;
                return (fmap Right medita);
            };
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected
    resultJustLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustEdit f edita) (JustEdit f editb);
    resultJustLens lens = MkFloatingLens
    {
        floatingLensInitial = floatingLensInitial lens,
        floatingLensUpdate = \(MkJustEdit edita) state -> do
        {
            msmeb <-  cofmap1CF getMaybeOne (cfmap (floatingLensUpdate lens edita state));
            return (case msmeb of
            {
                Just (newstate,meditb) -> (newstate,fmap MkJustEdit meditb);
                Nothing -> (state,Nothing);
            });
        },
        floatingLensSimple = \state -> cfmap (floatingLensSimple lens state),
        floatingLensPutEdit = \state (MkJustEdit editb) -> do
        {
            mmea <- cofmap1CF getMaybeOne (cfmap (floatingLensPutEdit lens state editb));
            return (case mmea of
            {
                Just (Just edita) -> Just (MkJustEdit edita);
                _ -> Nothing;
            });
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected
    resultLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustWholeEdit f edita) (JustWholeEdit f editb);
    resultLens lens = fullLens (resultJustLens lens);
}
