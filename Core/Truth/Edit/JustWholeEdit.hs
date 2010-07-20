module Truth.Edit.JustWholeEdit where
{
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Either;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    type JustWholeEdit f edit = Either (WholeEdit (f (Subject edit))) (JustEdit f edit);

    extractJustWholeEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustWholeEdit f edit -> Maybe edit;
    extractJustWholeEdit (Right (MkJustEdit edit)) = Just edit;
    extractJustWholeEdit (Left (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };

    data MatchJustWholeEdit t where
    {
        MkMatchJustWholeEdit ::
         forall f edit. Info (Type_KTT f) -> Info (Type_T edit) -> Info (Type_T (Subject edit)) -> MatchJustWholeEdit (Type_T (JustWholeEdit f edit));
    };

    instance Property MatchJustWholeEdit where
    {
        matchProperty tt = do
        {
            MkMatchEither twfa tjfe <- matchProp $(type1[t|MatchEither|]) tt;
            MkMatch tjf te <- matchProp $(type1[t|Match|]) tjfe;
            MkMatch tw tfa <- matchProp $(type1[t|Match|]) twfa;
            MkEqualType <- matchProp $(type1[t|EqualType (Type_KTT WholeEdit)|]) tw;
            MkMatch tj tf <- matchProp $(type1[t|Match|]) tjf;
            Kind_KTT <- matchProp $(type1[t|Kind_KTT|]) tf;
            MkEqualType <- matchProp $(type1[t|EqualType (Type_KKTTKTT JustEdit)|]) tj;
            MkMatch tf' ta <- matchProp $(type1[t|Match|]) tfa;
            MkEqualType <- matchWitness tf tf';
            Edit_Inst ta' <- matchProp $(type1[t|Edit_Inst|]) te;
            MkEqualType <- matchWitness ta ta';
            return (MkMatchJustWholeEdit tf te ta);
        };
    };

    instance Construct MatchJustWholeEdit where
    {
        construct (MkMatchJustWholeEdit tf te ta) =
            applyInfo
             (applyInfo (info :: Info (Type_KTKTT Either))
              (applyInfo (info :: Info (Type_KTT WholeEdit)) (applyInfo tf ta)))
             (applyInfo (applyInfo (info :: Info (Type_KKTTKTT JustEdit)) tf) te);
    };
}
