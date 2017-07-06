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

    data WithContextSelector (editx :: *) (editn :: *) (edit :: *) where
    {
        EditContext :: WithContextSelector editx editn editx;
        EditContent :: WithContextSelector editx editn editn;
    };

    instance TestEquality (WithContextSelector ea eb) where
    {
        testEquality EditContext EditContext = Just Refl;
        testEquality EditContent EditContent = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Edit editx,Edit editn) => TupleSelector (WithContextSelector editx editn) where
    {
        type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn);
        tupleReadFrom EditContext (MkWithContext x _n) = x;
        tupleReadFrom EditContent (MkWithContext _x n) = n;
    };

    instance (Edit ex,Edit en) => FiniteTupleSelector (WithContextSelector ex en) where
    {
        tupleConstruct f = MkWithContext <$> f EditContext <*> f EditContent;
    };

    instance (c (EditReader ex),c (EditReader en)) => TupleReaderWitness c (WithContextSelector ex en) where
    {
        tupleReaderWitness _ EditContext = MkConstraintWitness;
        tupleReaderWitness _ EditContent = MkConstraintWitness;
    };

    instance (c ex,c en) => TupleWitness c (WithContextSelector ex en) where
    {
        tupleWitness _ EditContext = MkConstraintWitness;
        tupleWitness _ EditContent = MkConstraintWitness;
    };

    instance (Edit ex,HasInfo ex,Edit en,HasInfo en) => TupleHasInfo (WithContextSelector ex en) where
    {
        tupleHasInfo EditContext = info;
        tupleHasInfo EditContent = info;
    };

    $(return []);
    instance HasInfo WithContextSelector where
    {
        info = mkSimpleInfo $(iowitness[t|WithContextSelector|]) [$(declInfo [d|
            instance (TestEquality :: (* -> *) -> Constraint) (WithContextSelector ea eb);
            instance (Edit editx,Edit editn) =>
                TupleSelector (WithContextSelector editx editn) where
            {
                type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn);
            };
            instance (Edit ex,FullReader (EditReader ex),Edit en,FullReader (EditReader en)) =>
                FiniteTupleSelector (WithContextSelector ex en);
            instance (c (EditReader ex),c (EditReader en)) => TupleReaderWitness c (WithContextSelector ex en);
            instance (c ex,c en) => TupleWitness c (WithContextSelector ex en);
        |])];
    };

    contextCleanLens :: CleanEditLens' Identity (TupleEdit (WithContextSelector editx editn)) editx;
    contextCleanLens = tupleCleanEditLens EditContext;
    contentCleanLens :: CleanEditLens' Identity (TupleEdit (WithContextSelector editx editn)) editn;
    contentCleanLens = tupleCleanEditLens EditContent;
}

