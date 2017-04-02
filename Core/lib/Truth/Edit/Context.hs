module Truth.Edit.Context where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.CleanEditLens;
    import Truth.Edit.Tuple;


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

    instance (HasNewValue context,HasNewValue content) => HasNewValue (WithContext context content) where
    {
        newValue = MkWithContext newValue newValue;
    };
{-
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
-}
    data WithContextAggregate editx editn edit where
    {
        EditContext :: WithContextAggregate editx editn editx;
        EditContent :: WithContextAggregate editx editn editn;
    };

    instance TestEquality (WithContextAggregate ea eb) where
    {
        testEquality EditContext EditContext = Just Refl;
        testEquality EditContent EditContent = Just Refl;
        testEquality _ _ = Nothing;
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

