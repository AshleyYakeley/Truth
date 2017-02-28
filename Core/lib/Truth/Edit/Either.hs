module Truth.Edit.Either where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

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

    instance Floating (EitherEdit ea eb) where
    {
        type FloatingEdit (EitherEdit ea eb) = EitherEdit ea eb;
    };

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
{-
    instance HasInfo (Type_KTKTT EitherEdit) where
    {
        info = MkInfo
            (SimpleWit $(iowitness[t| Type_KTKTT EitherEdit |]))
            (mconcat
            [
                -- instance (Edit ea,Edit eb,EditReader ea ~ EditReader eb) => Edit (Either ea eb)
                mkFacts (MkFactS (\ea -> MkFactS (\eb -> MkFactZ (do
                {
                    Edit_Inst sa <- matchProp $(type1[t|Edit_Inst|]) ea;
                    Edit_Inst sb <- matchProp $(type1[t|Edit_Inst|]) eb;
                    Refl <- testEquality sa sb;
                    return (Edit_Inst sa);
                })))
                :: FactS (FactS FactZ) Edit_Inst (Type_KTKTT EitherEdit)
                ),

                -- instance (FullEdit ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit (Either ea eb)
                mkFacts (MkFactS (\ea -> MkFactS (\eb -> MkFactZ (do
                {
                    Edit_Inst sa <- matchProp $(type1[t|Edit_Inst|]) ea;
                    FullEdit_Inst <- matchProp $(type1[t|FullEdit_Inst|]) ea;
                    Edit_Inst sb <- matchProp $(type1[t|Edit_Inst|]) eb;
                    Refl <- testEquality sa sb;
                    return FullEdit_Inst;
                })))
                :: FactS (FactS FactZ) FullEdit_Inst (Type_KTKTT EitherEdit)
                )
            ]);
    };

    data MatchEitherEdit t where
    {
        MkMatchEitherEdit :: forall a b. Info (Type_T a) -> Info (Type_T b) -> MatchEitherEdit (Type_T (EitherEdit a b));
    };

    instance Property MatchEitherEdit where
    {
        matchProperty a = do
        {
            MkMatch tea tb <- matchProp $(type1[t|Match|]) a;
            MkMatch teither ta <- matchProp $(type1[t|Match|]) tea;
            Refl <- matchProp $(type1[t|EqualType (Type_KTKTT EitherEdit)|]) teither;
            Kind_T <- matchProp $(type1[t|Kind_T|]) ta;
            Kind_T <- matchProp $(type1[t|Kind_T|]) tb;
            return (MkMatchEitherEdit ta tb);
        };
    };
-}
}
