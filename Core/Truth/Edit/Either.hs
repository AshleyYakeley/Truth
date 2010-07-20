{-# OPTIONS -fno-warn-orphans #-}
module Truth.Edit.Either where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    instance (Edit ea,Edit eb,Subject ea ~ Subject eb) => Edit (Either ea eb) where
    {
        type Subject (Either ea eb) = Subject ea;

        applyEdit (Left edit) = applyEdit edit;
        applyEdit (Right edit) = applyEdit edit;

        invertEdit (Left edit) s = fmap Left (invertEdit edit s);
        invertEdit (Right edit) s = fmap Right (invertEdit edit s);
    };

    instance (FullEdit ea,Edit eb,Subject ea ~ Subject eb) => FullEdit (Either ea eb) where
    {
        replaceEdit s = Left (replaceEdit s);
    };

    instance HasInfo (Type_KTKTT Either) where
    {
        info = MkInfo
            (SimpleWit $(iowitness[t| Type_KTKTT Either |]))
            (mconcat
            [
                -- instance (Edit ea,Edit eb,Subject ea ~ Subject eb) => Edit (Either ea eb)
                mkFacts (MkFactS (\ea -> MkFactS (\eb -> MkFactZ (do
                {
                    Edit_Inst sa <- matchProp $(type1[t|Edit_Inst|]) ea;
                    Edit_Inst sb <- matchProp $(type1[t|Edit_Inst|]) eb;
                    MkEqualType <- matchWitness sa sb;
                    return (Edit_Inst sa);
                })))
                :: FactS (FactS FactZ) Edit_Inst (Type_KTKTT Either)
                ),

                -- instance (FullEdit ea,Edit eb,Subject ea ~ Subject eb) => FullEdit (Either ea eb)
                mkFacts (MkFactS (\ea -> MkFactS (\eb -> MkFactZ (do
                {
                    Edit_Inst sa <- matchProp $(type1[t|Edit_Inst|]) ea;
                    FullEdit_Inst <- matchProp $(type1[t|FullEdit_Inst|]) ea;
                    Edit_Inst sb <- matchProp $(type1[t|Edit_Inst|]) eb;
                    MkEqualType <- matchWitness sa sb;
                    return FullEdit_Inst;
                })))
                :: FactS (FactS FactZ) FullEdit_Inst (Type_KTKTT Either)
                )
            ]);
    };

    data MatchEither t where
    {
        MkMatchEither :: forall a b. Info (Type_T a) -> Info (Type_T b) -> MatchEither (Type_T (Either a b));
    };

    instance Property MatchEither where
    {
        matchProperty a = do
        {
            MkMatch tea tb <- matchProp $(type1[t|Match|]) a;
            MkMatch teither ta <- matchProp $(type1[t|Match|]) tea;
            MkEqualType <- matchProp $(type1[t|EqualType (Type_KTKTT Either)|]) teither;
            Kind_T <- matchProp $(type1[t|Kind_T|]) ta;
            Kind_T <- matchProp $(type1[t|Kind_T|]) tb;
            return (MkMatchEither ta tb);
        };
    };

}
