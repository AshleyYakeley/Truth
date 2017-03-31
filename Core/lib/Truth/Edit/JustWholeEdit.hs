module Truth.Edit.JustWholeEdit where
{
    import Truth.Edit.JustEdit;
    import Truth.Edit.MaybeReader;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Either;
    import Truth.Edit.Edit;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

    type EitherWholeEdit edit = EitherEdit (WholeEdit (EditReader edit)) edit;

    type JustWholeEdit (f :: * -> *) edit = EitherWholeEdit (JustEdit f edit);

    extractJustWholeEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustWholeEdit f edit -> [edit];
    extractJustWholeEdit (RightEdit (MkJustEdit edit)) = return edit;
    extractJustWholeEdit (LeftEdit (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> fromReadable replaceEdit a;
        _ -> [];
    };

    data MatchEitherWholeEdit :: (forall k. k -> Type) where
    {
        MkMatchEitherWholeEdit ::
         forall edit. Info edit -> Info (EditReader edit) -> MatchEitherWholeEdit (EitherWholeEdit edit);
    };

    instance MatchInfo MatchEitherWholeEdit where
    {
        matchInfo i = do
        {
            MkSplitInfo ea eVar <- matchInfo i;
            MkSplitInfo e a <- matchInfo ea;
            ReflH <- testHetEquality e $ info @EitherEdit;
            MkSplitInfo w r <- matchInfo a;
            ReflH <- testHetEquality w $ info @WholeEdit;
            ValueFact (MkEditReaderInfo rVar) <- ask (infoKnowledge i) $ applyInfo (info @EditReaderInfo) eVar;
            ReflH <- testHetEquality r rVar;
            return $ MkMatchEitherWholeEdit eVar r;
        };
    };

    data MatchJustWholeEdit :: (forall k. k -> Type) where
    {
        MkMatchJustWholeEdit ::
         forall f edit. Info f -> Info edit -> Info (EditReader edit) -> MatchJustWholeEdit (JustWholeEdit f edit);
    };

    instance MatchInfo MatchJustWholeEdit where
    {
        matchInfo i = do
        {
            MkMatchEitherWholeEdit jfe rjfe <- matchInfo i;
            MkSplitInfo jf e <- matchInfo jfe;
            MkSplitInfo j f <- matchInfo jf;
            ReflH <- isInfo @JustEdit j;
            MkSplitInfo rjf r <- matchInfo rjfe;
            MkSplitInfo rj f' <- matchInfo rjf;
            ReflH <- isInfo @MaybeReader rj;
            ReflH <- sameInfo f f';
            return $ MkMatchJustWholeEdit f e r;
        };
    };
}
