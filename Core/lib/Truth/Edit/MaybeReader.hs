module Truth.Edit.MaybeReader where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;


    newtype AnyReturn f = MkAnyReturn (forall a. a -> f a);

    data MaybeReader (f :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadOther :: MaybeReader f reader (AnyReturn f);
        ReadIsJust :: MaybeReader f reader (Maybe (Limit f));
        ReadWholeJust :: forall f reader t. reader t -> MaybeReader f reader (f t);
    };

    instance (MonadOne f,Reader reader) => Reader (MaybeReader f reader) where
    {
        type ReaderSubject (MaybeReader f reader) = f (ReaderSubject reader);

        -- readFrom :: ReaderSubject (MaybeReader f reader) -> (forall t. MaybeReader f reader t -> t);
        readFrom fsubj ReadOther = MkAnyReturn (\a -> fmap (\_ -> a) fsubj);
        readFrom fsubj ReadIsJust = case retrieveOne fsubj of
        {
            FailureResult lfa -> Just lfa;
            SuccessResult _ -> Nothing;
        };
        readFrom fsubj (ReadWholeJust reader) = fmap (\subj -> readFrom subj reader) fsubj;
    };

    liftMaybeReadable :: (Traversable f,Monad f) => Readable reader a -> Readable (MaybeReader f reader) (f a);
    liftMaybeReadable rra = getCompose $ unReadable rra $ \rt -> MkCompose $ readable $ ReadWholeJust rt;

    liftMaybeReadFunction :: (Traversable f,Monad f) => ReadFunction ra rb -> ReadFunction (MaybeReader f ra) (MaybeReader f rb);
    liftMaybeReadFunction _rfrarb ReadOther = readable ReadOther;
    liftMaybeReadFunction _rfrarb ReadIsJust = readable ReadIsJust;
    liftMaybeReadFunction rfrarb (ReadWholeJust rt) = liftMaybeReadable (rfrarb rt);

    instance (MonadOne f,FullReader reader) => FullReader (MaybeReader f reader) where
    {
        -- fromReader :: ReadFunction (MaybeReader f reader) (f (ReaderSubject reader));
        fromReader = liftMaybeReadable fromReader;
    };

    instance HasInfo MaybeReader where
    {
        info = mkSimpleInfo $(iowitness[t|MaybeReader|])
        [
            -- instance (MonadOne f,Reader reader) => Reader (MaybeReader f reader)
            MkKnowledge $ \knowledge rjfr -> do
            {
                MkSplitInfo reader jfr <- matchInfo rjfr;
                ReflH <- testHetEquality (info @Reader) reader;
                MkSplitInfo jf readerVar <- matchInfo jfr;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @MaybeReader) j;
                ConstraintFact <- ask knowledge $ applyInfo (info @MonadOne) fVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Reader) readerVar;
                return ConstraintFact;
            }
        ];
    };
}
