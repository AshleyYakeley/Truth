module Data.Changes.JustWholeEdit where
{
    import Data.Changes.JustEdit;
    import Data.Changes.WholeEdit;
    import Data.Changes.Either;
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.FunctorOne;
    import Data.Result;
    import Data.Witness;
    import Prelude;

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
        MkMatchJustWholeEdit :: forall f edit. InfoKTT f -> InfoT edit -> InfoT (Subject edit) -> MatchJustWholeEdit (JustWholeEdit f edit);
    };

    instance PropertyT MatchJustWholeEdit where
    {
        matchPropertyT tt = do
        {
            MkMatchEither twfa tjfe <- matchPropertyT_ (Type :: Type (MatchEither FT)) tt;
            MkTMatchT tjf te <- matchPropertyT_ (Type :: Type (TMatchT FT)) tjfe;
            MkTMatchT tw tfa <- matchPropertyT_ (Type :: Type (TMatchT FT)) twfa;
            MkEqualType <- matchWitnessKTT (infoKTT :: InfoKTT WholeEdit) tw;
            MkKTTMatchKTT tj tf <- matchPropertyKTT_ (Type :: Type (KTTMatchKTT FKTT)) tjf;
            MkEqualType <- matchWitnessKKTTKTT (infoKKTTKTT :: InfoKKTTKTT JustEdit) tj;
            MkTMatchT tf' ta <- matchPropertyT_ (Type :: Type (TMatchT FT)) tfa;
            MkEqualType <- matchWitnessKTT tf tf';
            MkEditInst ta' <- matchPropertyT te;
            MkEqualType <- matchWitnessT ta ta';
            return (MkMatchJustWholeEdit tf te ta);
        };
    };

    instance ConstructT MatchJustWholeEdit where
    {
        constructT (MkMatchJustWholeEdit tf te ta) =
            applyTInfoT
             (applyTInfoKTT (infoKTKTT :: InfoKTKTT Either)
              (applyTInfoT (infoKTT :: InfoKTT WholeEdit) (applyTInfoT tf ta)))
             (applyTInfoT (applyKTTInfoKTT (infoKKTTKTT :: InfoKKTTKTT JustEdit) tf) te);
    };
}
