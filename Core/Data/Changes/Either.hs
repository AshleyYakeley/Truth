{-# OPTIONS -fno-warn-orphans #-}
module Data.Changes.Either where
{
    import Data.Changes.Edit;
    import Data.Witness;
    import Data.TypeKT;
    import Data.OpenWitness;

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

    instance HasInfoKTKTT Either where
    {
        infoKTKTT = MkInfoKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Data.Changes.Edit.Either"))
            (
                (mkTFactsKTKTT_ (witFactT :: IOWitness (SatKTT EditInst)) (\ta tb -> do
                {
                    MkEditInst sa <- matchPropertyT ta;
                    MkEditInst sb <- matchPropertyT tb;
                    MkEqualType <- matchWitnessT sa sb;
                    return (MkEditInst sa);
                }) :: FactsKTKTT Either) `mappend`
                (mkTFactsKTKTT_ (witFactT :: IOWitness (SatKTT FullEditInst)) (\ta tb -> do
                {
                    MkEditInst sa <- matchPropertyT ta;
                    MkFullEditInst <- matchPropertyT ta;
                    MkEditInst sb <- matchPropertyT tb;
                    MkEqualType <- matchWitnessT sa sb;
                    return MkFullEditInst;
                }) :: FactsKTKTT Either)
            );
    };

    data MatchEither t where
    {
        MkMatchEither :: forall a b. InfoT a -> InfoT b -> MatchEither (Either a b);
    };

    instance PropertyT MatchEither where
    {
        matchPropertyT tt = do
        {
            MkTMatchT tea tb <- matchPropertyT_ (Type :: Type (TMatchT FT)) tt;
            MkTMatchKTT teither ta <- matchPropertyKTT_ (Type :: Type (TMatchKTT FKTT)) tea;
            MkEqualType <- matchWitnessKTKTT teither (infoKTKTT :: InfoKTKTT Either);
            return (MkMatchEither ta tb);
        };
    };
}
