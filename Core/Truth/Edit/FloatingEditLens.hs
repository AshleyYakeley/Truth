module Truth.Edit.FloatingEditLens where
{
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data FloatingEditLens' state m edita editb = MkFloatingEditLens
    {
        floatingEditLensSimple :: FloatingLens' state m (Subject edita) (Subject editb),
        floatingEditLensUpdate :: edita -> state -> ConstFunction (Subject edita) (state,Maybe editb),
        floatingEditLensPutEdit :: state -> editb -> ConstFunction (Subject edita) (m (state,edita))    -- m failure means impossible
    };

    floatingEditLensInitial :: FloatingEditLens' state m edita editb -> state;
    floatingEditLensInitial = floatingLensInitial . floatingEditLensSimple;

    floatingEditLensGet :: FloatingEditLens' state m edita editb -> state -> Subject edita -> Subject editb;
    floatingEditLensGet = floatingLensGet . floatingEditLensSimple;

    floatingEditLensPutback :: FloatingEditLens' state m edita editb -> state -> Subject editb -> ConstFunction (Subject edita) (m (state,Subject edita));
    floatingEditLensPutback = floatingLensPutback . floatingEditLensSimple;

    type FloatingEditLens state = FloatingEditLens' state Maybe;

    instance IsBiMap (FloatingEditLens' state) where
    {
        mapBiMapM ff felens = MkFloatingEditLens
        {
            floatingEditLensSimple = mapBiMapM ff (floatingEditLensSimple felens),
            floatingEditLensUpdate = floatingEditLensUpdate felens,
            floatingEditLensPutEdit = \state edit -> fmap ff (floatingEditLensPutEdit felens state edit)
        };
    };

    fullLens ::
     FloatingEditLens state edita editb ->
     FloatingEditLens state (Either (WholeEdit (Subject edita)) edita) (Either (WholeEdit (Subject editb)) editb);
    fullLens lens = MkFloatingEditLens
    {
        floatingEditLensSimple = floatingEditLensSimple lens,
        floatingEditLensUpdate = \pedita oldstate -> case pedita of
        {
            Left (MkWholeEdit a) -> return (oldstate,Just (Left (MkWholeEdit (floatingEditLensGet lens oldstate a))));
            Right edita -> do
            {
                (newstate,meditb) <- floatingEditLensUpdate lens edita oldstate;
                return (newstate,fmap Right meditb);
            };
        },
        floatingEditLensPutEdit = \state peditb -> case peditb of
        {
            Left (MkWholeEdit b) -> do
            {
                msa <- floatingEditLensPutback lens state b;
                return (do
                {
                    (newstate,a) <- msa;
                    return (newstate,Left (MkWholeEdit a));
                });
            };
            Right editb -> do
            {
                msedita <- floatingEditLensPutEdit lens state editb;
                return (do
                {
                    (newstate,edita) <- msedita;
                    return (newstate,Right edita);
                });
            };
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected
    resultJustLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustEdit f edita) (JustEdit f editb);
    resultJustLens lens = MkFloatingEditLens
    {
        floatingEditLensSimple = cfmap (floatingEditLensSimple lens),
        floatingEditLensUpdate = \(MkJustEdit edita) state -> do
        {
            msmeb <-  cofmap1CF getMaybeOne (cfmap (floatingEditLensUpdate lens edita state));
            return (case msmeb of
            {
                Just (newstate,meditb) -> (newstate,fmap MkJustEdit meditb);
                Nothing -> (state,Nothing);
            });
        },
        floatingEditLensPutEdit = \state (MkJustEdit editb) -> do
        {
            mmsea <- cofmap1CF getMaybeOne (cfmap (floatingEditLensPutEdit lens state editb));
            return (case mmsea of
            {
                Just (Just (newstate,edita)) -> Just (newstate,MkJustEdit edita);
                _ -> Nothing;
            });
        }
    };

    -- suitable for Results, trying to put a failure code will be rejected
    resultLens :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditLens state edita editb -> FloatingEditLens state (JustWholeEdit f edita) (JustWholeEdit f editb);
    resultLens lens = fullLens (resultJustLens lens);
}
