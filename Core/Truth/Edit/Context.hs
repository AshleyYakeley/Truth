module Truth.Edit.Context where
{
    import Truth.Edit.Tuple;
    import Truth.Edit.CleanEditLens;
    import Truth.Edit.Edit;
    import Truth.Edit.Read;
    import Truth.Edit.Import;
    import Data.ConstFunction;

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

    instance FunctorAp (WithContext context) where
    {
        fap (MkWithContext _n ab) (MkWithContext n a) = MkWithContext n (ab a);
    };

    instance FunctorBind (WithContext context) where
    {
        bind (MkWithContext _ content) afb = afb content;
    };

    instance FunctorGetPure (WithContext context);

    instance FunctorOne (WithContext context) where
    {
        retrieveOne (MkWithContext _ a) = SuccessResult a;
    };

    instance (HasNewValue context,HasNewValue content) => HasNewValue (WithContext context content) where
    {
        newValue = MkWithContext newValue newValue;
    };

    instance HasInfo (Type_KTKTT WithContext) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTKTT WithContext |])
        [
            mkFacts (MkFactS (\a0 -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a0;
                return FunctorOne_Inst;
            }))
            :: FactS FactZ FunctorOne_Inst (Type_KTKTT WithContext)
            )
        ];
    };

    data WithContextAggregate editx editn edit where
    {
        EditContext :: WithContextAggregate editx editn editx;
        EditContent :: WithContextAggregate editx editn editn;
    };

    instance SimpleWitness (WithContextAggregate ea eb) where
    {
        matchWitness EditContext EditContext = Just MkEqualType;
        matchWitness EditContent EditContent = Just MkEqualType;
        matchWitness _ _ = Nothing;
    };

    instance (Edit editx,FullReader (EditReader editx),Edit editn,FullReader (EditReader editn)) =>
        IsAggregate (WithContextAggregate editx editn) where
    {
        type AggregateSubject (WithContextAggregate editx editn) = WithContext (EditSubject editx) (EditSubject editn);
        aggregateIsFullReaderEdit EditContext = MkIsFullReaderEdit;
        aggregateIsFullReaderEdit EditContent = MkIsFullReaderEdit;
        aggregateReadFrom EditContext (MkWithContext x _n) = x;
        aggregateReadFrom EditContent (MkWithContext _x n) = n;
        aggregateConstruct f = do
        {
            x <- f EditContext;
            n <- f EditContent;
            return (MkWithContext x n);
        };
    };

    contextCleanLens :: CleanEditLens' Identity (AggregateEdit (WithContextAggregate editx editn)) editx;
    contextCleanLens = aggregateLens EditContext;
    contentCleanLens :: CleanEditLens' Identity (AggregateEdit (WithContextAggregate editx editn)) editn;
    contentCleanLens = aggregateLens EditContent;

}

