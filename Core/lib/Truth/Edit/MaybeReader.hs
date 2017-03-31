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

    instance (FunctorOne f,Reader reader) => Reader (MaybeReader f reader) where
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

    liftJustReadable :: (Traversable f,FunctorBind f) => Readable reader a -> Readable (MaybeReader f reader) (f a);
    liftJustReadable rra = do
    {
        fmfa <- getCompose (unReadable rra (\ra -> MkCompose (fmap toFreeMonad (readable (ReadWholeJust ra)))));
        (MkAnyReturn return') <- readable ReadOther;
        return (fromFreeMonad return' bind fmfa);
    };

    liftJustReadFunction :: (Traversable f,FunctorBind f) => ReadFunction ra rb -> ReadFunction (MaybeReader f ra) (MaybeReader f rb);
    liftJustReadFunction _rfrarb ReadOther = readable ReadOther;
    liftJustReadFunction _rfrarb ReadIsJust = readable ReadIsJust;
    liftJustReadFunction rfrarb (ReadWholeJust rt) = liftJustReadable (rfrarb rt);

    instance (FunctorOne f,FullReader reader) => FullReader (MaybeReader f reader) where
    {
        -- fromReader :: ReadFunction (MaybeReader f reader) (f (ReaderSubject reader));
        fromReader = liftJustReadable fromReader;
    };

    instance HasInfo MaybeReader where
    {
        info = mkSimpleInfo $(iowitness[t|MaybeReader|])
        [
            -- instance (FunctorOne f,Reader reader) => Reader (MaybeReader f reader)
            MkKnowledge $ \knowledge rjfr -> do
            {
                MkSplitInfo reader jfr <- matchInfo rjfr;
                ReflH <- testHetEquality (info @Reader) reader;
                MkSplitInfo jf readerVar <- matchInfo jfr;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @MaybeReader) j;
                ConstraintFact <- ask knowledge $ applyInfo (info @FunctorOne) fVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Reader) readerVar;
                return ConstraintFact;
            }
        ];
    };
}
