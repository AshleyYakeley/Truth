module Truth.Edit.OneWholeEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.Either;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.MonadOneReader;
    import Truth.Edit.OneEdit;


    type EitherWholeEdit edit = EitherEdit (WholeEdit (EditReader edit)) edit;

    type OneWholeEdit (f :: * -> *) edit = EitherWholeEdit (OneEdit f edit);

    extractOneWholeEdit :: forall f edit. (MonadOne f,FullEdit edit) => OneWholeEdit f edit -> [edit];
    extractOneWholeEdit (RightEdit (MkOneEdit edit)) = return edit;
    extractOneWholeEdit (LeftEdit (MkWholeEdit fa)) = case retrieveOne fa of
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

    data MatchOneWholeEdit :: (forall k. k -> Type) where
    {
        MkMatchOneWholeEdit ::
         forall f edit. Info f -> Info edit -> Info (EditReader edit) -> MatchOneWholeEdit (OneWholeEdit f edit);
    };

    instance MatchInfo MatchOneWholeEdit where
    {
        matchInfo i = do
        {
            MkMatchEitherWholeEdit jfe rjfe <- matchInfo i;
            MkSplitInfo jf e <- matchInfo jfe;
            MkSplitInfo j f <- matchInfo jf;
            ReflH <- isInfo @OneEdit j;
            MkSplitInfo rjf r <- matchInfo rjfe;
            MkSplitInfo rj f' <- matchInfo rjf;
            ReflH <- isInfo @MonadOneReader rj;
            ReflH <- sameInfo f f';
            return $ MkMatchOneWholeEdit f e r;
        };
    };
}
