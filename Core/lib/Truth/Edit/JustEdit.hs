module Truth.Edit.JustEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;


    newtype AnyReturn f = MkAnyReturn (forall a. a -> f a);

    data JustReader (f :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadOther :: JustReader f reader (AnyReturn f);
        ReadIsJust :: JustReader f reader (Maybe (Limit f));
        ReadWholeJust :: forall f reader t. reader t -> JustReader f reader (f t);
    };

    instance (FunctorOne f,Reader reader) => Reader (JustReader f reader) where
    {
        type ReaderSubject (JustReader f reader) = f (ReaderSubject reader);

        -- readFrom :: ReaderSubject (JustReader f reader) -> (forall t. JustReader f reader t -> t);
        readFrom fsubj ReadOther = MkAnyReturn (\a -> fmap (\_ -> a) fsubj);
        readFrom fsubj ReadIsJust = case retrieveOne fsubj of
        {
            FailureResult lfa -> Just lfa;
            SuccessResult _ -> Nothing;
        };
        readFrom fsubj (ReadWholeJust reader) = fmap (\subj -> readFrom subj reader) fsubj;
    };

    liftJustReadable :: (Traversable f,FunctorBind f) => Readable reader a -> Readable (JustReader f reader) (f a);
    liftJustReadable rra = do
    {
        fmfa <- getCompose (unReadable rra (\ra -> MkCompose (mmap toFreeMonad (readable (ReadWholeJust ra)))));
        (MkAnyReturn return') <- readable ReadOther;
        return (fromFreeMonad return' bind fmfa);
    };

    liftJustReadFunction :: (Traversable f,FunctorBind f) => ReadFunction ra rb -> ReadFunction (JustReader f ra) (JustReader f rb);
    liftJustReadFunction _rfrarb ReadOther = readable ReadOther;
    liftJustReadFunction _rfrarb ReadIsJust = readable ReadIsJust;
    liftJustReadFunction rfrarb (ReadWholeJust rt) = liftJustReadable (rfrarb rt);

    instance (FunctorOne f,FullReader reader) => FullReader (JustReader f reader) where
    {
        -- fromReader :: ReadFunction (JustReader f reader) (f (ReaderSubject reader));
        fromReader = liftJustReadable fromReader;
    };

    instance HasInfo JustReader where
    {
        info = mkSimpleInfo $(iowitness[t|JustReader|])
        [
            -- instance (FunctorOne f,Reader reader) => Reader (JustReader f reader)
            MkKnowledge $ \knowledge rjfr -> do
            {
                MkSplitInfo reader jfr <- matchInfo rjfr;
                ReflH <- testHetEquality (info @Reader) reader;
                MkSplitInfo jf readerVar <- matchInfo jfr;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @JustReader) j;
                MkConstraintWitness <- ask knowledge $ applyInfo (info @FunctorOne) fVar;
                MkConstraintWitness <- ask knowledge $ applyInfo (info @Reader) readerVar;
                return MkConstraintWitness;
            }
        ];
    };

    newtype JustEdit (f :: * -> *) edit = MkJustEdit edit;

    instance Floating (JustEdit f edit) (JustEdit f edit);

    instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit) where
    {
        type EditReader (JustEdit f edit) = JustReader f (EditReader edit);

        -- applyEdit :: JustEdit f edit -> ReadMap (JustReader f (EditReader edit)) (JustReader f (EditReader edit));
        applyEdit (MkJustEdit _edita) ReadOther = readable ReadOther;
        applyEdit (MkJustEdit _edita) ReadIsJust = readable ReadIsJust;
        applyEdit (MkJustEdit edita) (ReadWholeJust reader) = liftJustReadable (applyEdit edita reader);

        -- invertEdit :: JustEdit f edit -> Readable (JustReader f reader) (Maybe (JustEdit f edit));    -- "Nothing" means no change
        invertEdit (MkJustEdit edita) = do
        {
            fme <- liftJustReadable (invertEdit edita);
            return (case getMaybeOne fme of
            {
                Just edits -> fmap MkJustEdit edits;
                _ -> [];
            });
        };
    };

    instance HasInfo JustEdit where
    {
        info = mkSimpleInfo $(iowitness[t|JustEdit|])
        [
            -- instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit)
            MkKnowledge $ \knowledge ejfe -> do
            {
                MkSplitInfo edit jfe <- matchInfo ejfe;
                ReflH <- testHetEquality (info @Edit) edit;
                MkSplitInfo jf editVar <- matchInfo jfe;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @JustEdit) j;
                MkConstraintWitness <- ask knowledge $ applyInfo (info @FunctorOne) fVar;
                MkConstraintWitness <- ask knowledge $ applyInfo (info @Edit) editVar;
                return MkConstraintWitness;
            }
        ];
    };
}
