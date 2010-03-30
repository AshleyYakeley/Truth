module Truth.Edit.Context where
{
    import Truth.Edit.Tuple;
    import Truth.Edit.EditLens;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data WithContext context content = MkWithContext context content;

    instance Functor (WithContext context) where
    {
        fmap ab (MkWithContext context a) = MkWithContext context (ab a);
    };

    instance Foldable (WithContext context) where
    {
        foldMap am (MkWithContext _ a) = am a;
    };

    instance Traversable (WithContext context) where
    {
        traverse afb (MkWithContext context a) = fmap (MkWithContext context) (afb a);
        sequenceA (MkWithContext context fa) = fmap (MkWithContext context) fa;
    };

    instance FunctorOne (WithContext context) where
    {
        retrieveOne (MkWithContext _ a) = SuccessResult a;
        getPureOne = arr (\(MkWithContext context _) content -> (MkWithContext context content));
    };

    instance HasInfoKTKTT WithContext where
    {
        infoKTKTT = MkInfoKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Truth.Edit.Context.WithContext"))
            (mkKTTFactsKTKTT (\_ -> do
                {
                    return MkFunctorOneInst;
                })
            );
    };

    instance IsTuple (WithContext context content) where
    {
        type ListTuple (WithContext context content) = (content,(context,()));
        fromListTuple (content,(context,())) = MkWithContext context content;
        toListTuple (MkWithContext context content) = (content,(context,()));
    };

    type ContextContentEdit editx editn = TupleWholeEdit (editn,(editx,())) (WithContext (Subject editx) (Subject editn));

    contextCleanLens :: (FullEdit editx,FullEdit editn) => CleanEditLens' Identity (ContextContentEdit editx editn) editx;
    contextCleanLens = tupleElementCleanLens (SuccNat ZeroNat);

    contentCleanLens :: (FullEdit editx,FullEdit editn) => CleanEditLens' Identity (ContextContentEdit editx editn) editn;
    contentCleanLens = tupleElementCleanLens ZeroNat;
}

