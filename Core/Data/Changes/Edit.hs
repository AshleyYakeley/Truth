module Data.Changes.Edit where
{
{-
--    import Data.Changes.Context;
--    import Data.Changes.Tuple;
    import Data.Changes.List;
    import Data.Changes.JustEdit;
    import Data.Changes.WholeEdit;
    import Data.Changes.EditScheme;
    import Data.ByteString;
    import Data.Result;
    import Data.Word;

--    data Edit' pe a = ReplaceEdit a | PartEdit pe;

    class (CompleteEditScheme a (Edit a)) => Editable a where
    {
        type Edit a :: *;
    };

    instance Editable () where
    {
        type Edit () = WholeEdit ();
    };

    instance Editable Bool where
    {
        type Edit Bool = WholeEdit Bool;
    };

    instance Editable Word8 where
    {
        type Edit Word8 = WholeEdit Word8;
    };

    instance Editable Char where
    {
        type Edit Char = WholeEdit Char;
    };

    instance Editable ByteString where
    {
        type Edit ByteString = WholeEdit ByteString;
    };

    instance (Editable a) => Editable (Result err a) where
    {
        type Edit (Result err a) = JustEdit (Result err a) (Edit a);
    };

    instance (Editable a) => Editable (Maybe a) where
    {
        type Edit (Maybe a) = JustEdit (Maybe a) (Edit a);
    };

    instance (Editable a) => Editable [a] where
    {
        type Edit [a] = ListEdit [a] (Edit a);
    };
    
-}    

{-
    instance (Editable a,Editable b) => Editable (a,b) where
    {
        type Edit (a,b) = TListEdit (a,(b,())) (Edit a,(Edit b,()));
    };
    
    instance (Editable context,Editable content) => Editable (WithContext context content) where
    {
        type Edit (WithContext context content) = TListEdit (content,(context,())) (Edit content,(Edit context,()));
    };
-}

{-
    instance (Editable context,Editable content) => Editable (WithContext context content) where
    {
        type Edit (WithContext context content) = ContextContentEdit (Edit content) (Edit context);
    };
-}


{-
    data WitnessJustEdit f a where
    {
        MkWitnessJustEdit :: (PartEdit (f a) ~ JustEdit a) => WitnessJustEdit f a;
    };
    
    class HasJustEdit f where
    {
        witnessJustEdit :: forall a. WitnessJustEdit f a;
    };

    instance HasJustEdit (Result err) where
    {
        witnessJustEdit = MkWitnessJustEdit;
    };

    instance HasJustEdit Maybe where
    {
        witnessJustEdit = MkWitnessJustEdit;
    };

    justEdit :: forall f a. (HasJustEdit f) => Type (f ()) -> Edit a -> PartEdit (f a);
    justEdit _ = case witnessJustEdit :: WitnessJustEdit f a of
    {
        MkWitnessJustEdit -> JustEdit;
    };

    extractJustEdit :: forall f a. (FunctorOne f,HasJustEdit f) => Edit (f a) -> Maybe (Edit a);
    extractJustEdit (PartEdit jea) = case witnessJustEdit :: WitnessJustEdit f a of
    {
        MkWitnessJustEdit -> case jea of
        {
            JustEdit ea -> Just ea;
        };
    };
    extractJustEdit (ReplaceEdit fa) = case retrieveOne fa of
    {
        SuccessResult a -> Just (ReplaceEdit a);
        _ -> Nothing;
    };
-}
}
