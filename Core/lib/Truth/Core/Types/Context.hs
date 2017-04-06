module Truth.Core.Types.Context where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


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

    instance Comonad (WithContext context) where
    {
        extract (MkWithContext _ content) = content;
        extend wab wa@(MkWithContext context _) = MkWithContext context $ wab wa;
    };

    instance (HasNewValue context,HasNewValue content) => HasNewValue (WithContext context content) where
    {
        newValue = MkWithContext newValue newValue;
    };

    $(return []);
    instance HasInfo WithContext where
    {
        info = mkSimpleInfo $(iowitness[t|WithContext|]) [$(declInfo [d|
            instance Functor (WithContext context);
            instance Foldable (WithContext context);
            instance Traversable (WithContext context);
            instance Comonad (WithContext context);
            instance (HasNewValue context,HasNewValue content) => HasNewValue (WithContext context content);
        |])];
    };

    data WithContextTuple editx editn edit where
    {
        EditContext :: WithContextTuple editx editn editx;
        EditContent :: WithContextTuple editx editn editn;
    };

    instance TestEquality (WithContextTuple ea eb) where
    {
        testEquality EditContext EditContext = Just Refl;
        testEquality EditContent EditContent = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Edit editx,FullReader (EditReader editx),Edit editn,FullReader (EditReader editn)) =>
        TupleSelector (WithContextTuple editx editn) where
    {
        type TupleSubject (WithContextTuple editx editn) = WithContext (EditSubject editx) (EditSubject editn);
        tupleIsFullReaderEdit EditContext = MkConstraintWitness;
        tupleIsFullReaderEdit EditContent = MkConstraintWitness;
        tupleReadFrom EditContext (MkWithContext x _n) = x;
        tupleReadFrom EditContent (MkWithContext _x n) = n;
        tupleConstruct f = do
        {
            x <- f EditContext;
            n <- f EditContent;
            return (MkWithContext x n);
        };
    };

    contextCleanLens :: CleanEditLens' Identity (TupleEdit (WithContextTuple editx editn)) editx;
    contextCleanLens = tupleLens EditContext;
    contentCleanLens :: CleanEditLens' Identity (TupleEdit (WithContextTuple editx editn)) editn;
    contentCleanLens = tupleLens EditContent;

    instance (FullEdit ex,FullEdit en) => FiniteTupleSelector (WithContextTuple ex en) where
    {
        tupleAllSelectors = [MkAnyWitness EditContext,MkAnyWitness EditContent];
        tupleIsFullEdit EditContext = MkConstraintWitness;
        tupleIsFullEdit EditContent = MkConstraintWitness;
    };
}

