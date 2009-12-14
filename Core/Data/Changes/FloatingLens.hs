module Data.Changes.FloatingLens where
{
    import Data.Changes.SimpleLens;
    import Data.Changes.JustEdit;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.FunctorOne;
    import Data.ConstFunction;
    import Data.Chain;

    data FloatingLens' m state edita editb = MkFloatingLens
    {
        lensInitial :: state,
        lensSimple :: state -> SimpleLens' m (Subject edita) (Subject editb),
        lensUpdate :: edita -> state -> ConstFunction (Subject edita) (state,Maybe editb),
        lensPutEdit :: state -> editb -> ConstFunction (Subject edita) (m edita)    -- m failure means impossible
    };

    lensGet :: FloatingLens' m state edita editb -> state -> Subject edita -> Subject editb;
    lensGet lens state = simpleLensGet (lensSimple lens state);

    lensPutback :: FloatingLens' m state edita editb -> state -> Subject editb -> ConstFunction (Subject edita) (m (Subject edita));
    lensPutback lens state = simpleLensPutback (lensSimple lens state);

    type FloatingLens = FloatingLens' Maybe;

    toFloatingLens :: (FunctorOne m) => FloatingLens' m state edita editb -> FloatingLens state edita editb;
    toFloatingLens lens = MkFloatingLens
    {
        lensInitial = lensInitial lens,
        lensSimple = \state -> toSimpleLens (lensSimple lens state),
        lensUpdate = lensUpdate lens,
        lensPutEdit = \state edit -> fmap getMaybeOne (lensPutEdit lens state edit)
    };

    fullLens ::
     FloatingLens state edita editb ->
     FloatingLens state (Either (WholeEdit (Subject edita)) edita) (Either (WholeEdit (Subject editb)) editb);
    fullLens lens = MkFloatingLens
    {
        lensInitial = lensInitial lens,
        lensSimple = lensSimple lens,
        lensUpdate = \pedita oldstate -> case pedita of
        {
            Left (MkWholeEdit a) -> return (oldstate,Just (Left (MkWholeEdit (lensGet lens oldstate a))));
            Right edita -> do
            {
                (newstate,meditb) <- lensUpdate lens edita oldstate;
                return (newstate,fmap Right meditb);
            };
        },
        lensPutEdit = \state peditb -> case peditb of
        {
            Left (MkWholeEdit b) -> do
            {
                ma <- lensPutback lens state b;
                return (fmap (Left . MkWholeEdit) ma);
            };
            Right editb -> do
            {
                medita <- lensPutEdit lens state editb;
                return (fmap Right medita);
            };
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected
    resultJustLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingLens state edita editb -> FloatingLens state (JustEdit f edita) (JustEdit f editb);
    resultJustLens lens = MkFloatingLens
    {
        lensInitial = lensInitial lens,
        lensUpdate = \(MkJustEdit edita) state -> do
        {
            msmeb <-  cofmap1CF getMaybeOne (cfmap (lensUpdate lens edita state));
            return (case msmeb of
            {
                Just (newstate,meditb) -> (newstate,fmap MkJustEdit meditb);
                Nothing -> (state,Nothing);
            });
        },
        lensSimple = \state -> cfmap (lensSimple lens state),
        lensPutEdit = \state (MkJustEdit editb) -> do
        {
            mmea <- cofmap1CF getMaybeOne (cfmap (lensPutEdit lens state editb));
            return (case mmea of
            {
                Just (Just edita) -> Just (MkJustEdit edita);
                _ -> Nothing;
            });
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected
    resultLens :: forall f state edita editb. (FunctorOne f,FullEdit edita,FullEdit editb) =>
     FloatingLens state edita editb -> FloatingLens state (JustWholeEdit f edita) (JustWholeEdit f editb);
    resultLens lens = fullLens (resultJustLens lens);
}
