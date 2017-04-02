module Truth.Edit.MonadOneReader where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;


    data MonadOneReader (f :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadHasOne :: forall f reader. MonadOneReader f reader (f ());
        ReadOne :: forall f reader t. reader t -> MonadOneReader f reader (f t);
    };

    instance (MonadOne f,Reader reader) => Reader (MonadOneReader f reader) where
    {
        type ReaderSubject (MonadOneReader f reader) = f (ReaderSubject reader);

        -- readFrom :: ReaderSubject (MonadOneReader f reader) -> (forall t. MonadOneReader f reader t -> t);
        readFrom fsubj ReadHasOne = fmap (\_ -> ()) fsubj;
        readFrom fsubj (ReadOne reader) = fmap (\subj -> readFrom subj reader) fsubj;
    };

    liftMaybeReadable :: (Traversable f,Monad f) => Readable reader a -> Readable (MonadOneReader f reader) (f a);
    liftMaybeReadable rra = getCompose $ unReadable rra $ \rt -> MkCompose $ readable $ ReadOne rt;

    liftMaybeReadFunction :: (Traversable f,Monad f) => ReadFunction ra rb -> ReadFunction (MonadOneReader f ra) (MonadOneReader f rb);
    liftMaybeReadFunction _rfrarb ReadHasOne = readable ReadHasOne;
    liftMaybeReadFunction rfrarb (ReadOne rt) = liftMaybeReadable (rfrarb rt);

    instance (MonadOne f,FullReader reader) => FullReader (MonadOneReader f reader) where
    {
        -- fromReader :: ReadFunction (MonadOneReader f reader) (f (ReaderSubject reader));
        fromReader = liftMaybeReadable fromReader;
    };

    instance HasInfo MonadOneReader where
    {
        info = mkSimpleInfo $(iowitness[t|MonadOneReader|])
        [
            -- instance (MonadOne f,Reader reader) => Reader (MonadOneReader f reader)
            MkKnowledge $ \knowledge rjfr -> do
            {
                MkSplitInfo reader jfr <- matchInfo rjfr;
                ReflH <- testHetEquality (info @Reader) reader;
                MkSplitInfo jf readerVar <- matchInfo jfr;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @MonadOneReader) j;
                ConstraintFact <- ask knowledge $ applyInfo (info @MonadOne) fVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Reader) readerVar;
                return ConstraintFact;
            }
        ];
    };
}
