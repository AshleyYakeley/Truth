module Truth.Edit.Either where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;


    data EitherReader ra rb t = LeftReader (ra t) | RightReader (rb t);

    instance (Reader ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => Reader (EitherReader ra rb) where
    {
        type ReaderSubject (EitherReader ra rb) = ReaderSubject ra;

        readFromM msubj (LeftReader reader) = readFromM msubj reader;
        readFromM msubj (RightReader reader) = readFromM msubj reader;

        readFrom subj (LeftReader reader) = readFrom subj reader;
        readFrom subj (RightReader reader) = readFrom subj reader;
    };

    data EitherEdit ea eb = LeftEdit ea | RightEdit eb;

    instance Floating (EitherEdit ea eb) (EitherEdit ea eb);

    instance (Edit ea,Edit eb,EditReader ea ~ EditReader eb) => Edit (EitherEdit ea eb) where
    {
        type EditReader (EitherEdit ea eb) = EditReader ea;

        applyEdit (LeftEdit edit) = applyEdit edit;
        applyEdit (RightEdit edit) = applyEdit edit;

        invertEdit (LeftEdit edit) = fmap (fmap LeftEdit) (invertEdit edit);
        invertEdit (RightEdit edit) = fmap (fmap RightEdit) (invertEdit edit);
    };

    instance (FullEdit ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit (EitherEdit ea eb) where
    {
        replaceEdit s = LeftEdit (replaceEdit s);
    };

    instance HasInfo EitherEdit where
    {
        info = mkSimpleInfo $(iowitness[t|EitherEdit|])
        [
            -- instance (Edit a,Edit b,EditReader a ~ EditReader b) => Edit (Either a b)
            MkKnowledge $ \knowledge eeab -> do
            {
                MkSplitInfo edit eab <- matchInfo eeab;
                ReflH <- testHetEquality (info @Edit) edit;
                MkSplitInfo ea bVar <- matchInfo eab;
                MkSplitInfo e aVar <- matchInfo ea;
                ReflH <- testHetEquality (info @EitherEdit) e;
                ConstraintFact <- ask knowledge $ applyInfo (info @Edit) aVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Edit) bVar;
                ValueFact (MkEditReaderInfo arVar) <- ask knowledge $ applyInfo (info @EditReaderInfo) aVar;
                ValueFact (MkEditReaderInfo brVar) <- ask knowledge $ applyInfo (info @EditReaderInfo) bVar;
                ReflH <- testHetEquality arVar brVar;
                return ConstraintFact;
            },

            -- type EditReader (EitherEdit a b) = EditReader a;
            MkKnowledge $ \knowledge eeab -> do
            {
                MkSplitInfo er eab <- matchInfo eeab;
                ReflH <- testHetEquality (info @EditReaderInfo) er;
                MkSplitInfo ea _bVar <- matchInfo eab;
                MkSplitInfo e aVar <- matchInfo ea;
                ReflH <- testHetEquality (info @EitherEdit) e;
                ValueFact (MkEditReaderInfo ra) <- ask knowledge $ applyInfo (info @EditReaderInfo) aVar;
                return $ ValueFact (MkEditReaderInfo ra);
            },

            -- instance (FullEdit ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit (Either ea eb)
            MkKnowledge $ \knowledge eeab -> do
            {
                MkSplitInfo edit eab <- matchInfo eeab;
                ReflH <- testHetEquality (info @FullEdit) edit;
                MkSplitInfo ea bVar <- matchInfo eab;
                MkSplitInfo e aVar <- matchInfo ea;
                ReflH <- testHetEquality (info @EitherEdit) e;
                ConstraintFact <- ask knowledge $ applyInfo (info @FullEdit) aVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Edit) bVar;
                ValueFact (MkEditReaderInfo arVar) <- ask knowledge $ applyInfo (info @EditReaderInfo) aVar;
                ValueFact (MkEditReaderInfo brVar) <- ask knowledge $ applyInfo (info @EditReaderInfo) bVar;
                ReflH <- testHetEquality arVar brVar;
                return ConstraintFact;
            }
        ];
    };
}
