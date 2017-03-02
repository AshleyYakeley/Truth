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
{-
    instance HasInfo (Type_KKTTKKTTKTT JustReader) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KKTTKKTTKTT JustReader |])
        [
            -- instance (FunctorOne f,Reader reader) => Reader (JustReader f reader)
            mkFacts (MkFactS (\tf -> MkFactS (\treader -> MkFactZ (do
            {
                Reader_Inst tsubj <- matchProp $(type1[t|Reader_Inst|]) treader;
                FunctorOne_Inst <- matchProp $(type1[t|FunctorOne_Inst|]) tf;
                return (Reader_Inst (applyInfo tf tsubj));
            })))
            :: FactS (FactS FactZ) Reader_Inst (Type_KKTTKKTTKTT JustReader)
            )
        ];
    };
-}
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
{-
    instance HasInfo (Type_KKTTKTT JustEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KKTTKTT JustEdit |])
        [
            -- instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit)
            mkFacts (MkFactS (\tf -> MkFactS (\tedit -> MkFactZ (do
            {
                Edit_Inst treader <- matchProp $(type1[t|Edit_Inst|]) tedit;
                FunctorOne_Inst <- matchProp $(type1[t|FunctorOne_Inst|]) tf;
                return (Edit_Inst (applyInfo (applyInfo (info :: Info (Type_KKTTKKTTKTT JustReader)) tf) treader));
            })))
            :: FactS (FactS FactZ) Edit_Inst (Type_KKTTKTT JustEdit)
            )
        ];
    };
-}
}
