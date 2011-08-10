module Truth.Edit.JustWholeEdit where
{
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Either;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    type EitherWholeEdit edit = EitherEdit (WholeEdit (EditReader edit)) edit;

    type JustWholeEdit (f :: * -> *) edit = EitherWholeEdit (JustEdit f edit);

    extractJustWholeEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustWholeEdit f edit -> Maybe edit;
    extractJustWholeEdit (RightEdit (MkJustEdit edit)) = Just edit;
    extractJustWholeEdit (LeftEdit (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };

    data MatchJustWholeEdit t where
    {
        MkMatchJustWholeEdit ::
         forall f edit. Edit_Inst (Type_T edit) -> Info (Type_KTT f) -> Info (Type_T edit) -> MatchJustWholeEdit (Type_T (JustWholeEdit f edit));
    };

    instance Property MatchJustWholeEdit where
    {
        matchProperty tt = do
        {
            MkMatchEitherEdit twjrfr tjefe <- matchProp $(type1[t|MatchEitherEdit|]) tt;
            MkMatch tjef te <- matchProp $(type1[t|Match|]) tjefe;
            MkMatch tw tjrfr <- matchProp $(type1[t|Match|]) twjrfr;
            MkEqualType <- matchProp $(type1[t|EqualType (Type_KKTTT WholeEdit)|]) tw;
            MkMatch tje tf <- matchProp $(type1[t|Match|]) tjef;
            Kind_KTT <- matchProp $(type1[t|Kind_KTT|]) tf;
            MkEqualType <- matchProp $(type1[t|EqualType (Type_KKTTKTT JustEdit)|]) tje;
            MkMatch tjrf tr <- matchProp $(type1[t|Match|]) tjrfr;
            MkMatch tjr tf' <- matchProp $(type1[t|Match|]) tjrf;
            MkEqualType <- matchProp $(type1[t|EqualType (Type_KKTTKKTTKTT JustReader)|]) tjr;
            MkEqualType <- matchWitness tf tf';
            ei@(Edit_Inst tr') <- matchProp $(type1[t|Edit_Inst|]) te;
            MkEqualType <- matchWitness tr tr';
            return (MkMatchJustWholeEdit ei tf te);
        };
    };

    instance Construct MatchJustWholeEdit where
    {
        construct (MkMatchJustWholeEdit (Edit_Inst tr) tf te) =
            applyInfo
             (applyInfo (info :: Info (Type_KTKTT EitherEdit))
              (applyInfo (info :: Info (Type_KKTTT WholeEdit))
               (applyInfo (applyInfo (info :: Info (Type_KKTTKKTTKTT JustReader)) tf) tr)
              ))
             (applyInfo (applyInfo (info :: Info (Type_KKTTKTT JustEdit)) tf) te);
    };
}
