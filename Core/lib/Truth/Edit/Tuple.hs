{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Edit.Tuple where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.EditFunction;
    import Truth.Edit.CleanEditLens;


    data IsFullReaderEdit edit where
    {
        MkIsFullReaderEdit :: (FullReader (EditReader edit),Edit edit) => IsFullReaderEdit edit;
    };

    class TestEquality agg => IsAggregate (agg :: * -> *) where
    {
        type AggregateSubject agg :: *;
        aggregateIsFullReaderEdit :: agg edit -> IsFullReaderEdit edit;
        aggregateReadFrom :: agg edit -> AggregateSubject agg -> EditSubject edit;
        aggregateConstruct :: Monad m => (forall edit. agg edit -> m (EditSubject edit)) -> m (AggregateSubject agg);
    };

        data PairAggregate ea eb et where
        {
            EditFirst :: PairAggregate ea eb ea;
            EditSecond :: PairAggregate ea eb eb;
        };

        instance TestEquality (PairAggregate ea eb) where
        {
            testEquality EditFirst EditFirst = Just Refl;
            testEquality EditSecond EditSecond = Just Refl;
            testEquality _ _ = Nothing;
        };

        instance (Edit ea,FullReader (EditReader ea),Edit eb,FullReader (EditReader eb)) =>
            IsAggregate (PairAggregate ea eb) where
        {
            type AggregateSubject (PairAggregate ea eb) = (EditSubject ea,EditSubject eb);
            aggregateIsFullReaderEdit EditFirst = MkIsFullReaderEdit;
            aggregateIsFullReaderEdit EditSecond = MkIsFullReaderEdit;
            aggregateReadFrom EditFirst (a,_b) = a;
            aggregateReadFrom EditSecond (_a,b) = b;
            aggregateConstruct f = do
            {
                a <- f EditFirst;
                b <- f EditSecond;
                return (a,b);
            };
        };
{-
    data AggregateReader aggr t where
    {
        MkAggregateReader :: aggr reader -> reader t -> AggregateReader agg t;
    };
-}
    data AggregateEditReader agg t where
    {
        MkAggregateEditReader :: agg edit -> EditReader edit t -> AggregateEditReader agg t;
    };

    instance (IsAggregate agg) => Reader (AggregateEditReader agg) where
    {
        type ReaderSubject (AggregateEditReader agg) = AggregateSubject agg;
        readFrom a (MkAggregateEditReader aggedit reader) = case aggregateIsFullReaderEdit aggedit of
        {
            MkIsFullReaderEdit -> readFrom (aggregateReadFrom aggedit a) reader;
        };
    };

    instance (IsAggregate agg) => FullReader (AggregateEditReader agg) where
    {
        fromReader = aggregateConstruct (\(aggedit :: agg edit) -> case aggregateIsFullReaderEdit aggedit of
        {
            MkIsFullReaderEdit -> mapCleanReadable (MkAggregateEditReader aggedit) fromReader;
        });
    };


    data AggregateEdit agg where
    {
        MkAggregateEdit :: agg edit -> edit -> AggregateEdit agg;
    };

    instance Floating (AggregateEdit agg) (AggregateEdit agg);

    instance (IsAggregate agg) => Edit (AggregateEdit agg) where
    {
        type EditReader (AggregateEdit agg) = AggregateEditReader agg;

        applyEdit (MkAggregateEdit aggedite edit) aggreader@(MkAggregateEditReader aggeditr reader) =
            case (aggregateIsFullReaderEdit aggedite,testEquality aggedite aggeditr) of
            {
                (MkIsFullReaderEdit,Just Refl) -> mapCleanReadable (MkAggregateEditReader aggedite) (applyEdit edit reader);
                _ -> readable aggreader;
            };

        invertEdit (MkAggregateEdit aggedit edit) = case aggregateIsFullReaderEdit aggedit of
        {
            MkIsFullReaderEdit -> fmap (fmap (MkAggregateEdit aggedit))
                (mapCleanReadable (MkAggregateEditReader aggedit) (invertEdit edit));
        };
    };

    aggregateLens :: TestEquality agg => agg edit -> CleanEditLens' Identity (AggregateEdit agg) edit;
    aggregateLens aggedit = MkCleanEditLens
    {
        cleanEditLensFunction = MkCleanEditFunction
        {
            cleanEditGet = MkAggregateEditReader aggedit,
            cleanEditUpdate = \(MkAggregateEdit aggedit' edit) -> case testEquality aggedit aggedit' of
            {
                Just Refl -> [edit];
                _ -> [];
            }
        },
        cleanEditLensPutEdit = Identity . (MkAggregateEdit aggedit)
    };
}
