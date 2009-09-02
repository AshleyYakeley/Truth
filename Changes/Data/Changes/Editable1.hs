module Data.Changes.Editable1 where
{
{-
    import Data.Changes.List();
    import Data.Changes.Edit;
    import Data.Result;
    import Data.Witness;
    
    class Editable1 p where
    {
        editable1 :: forall a r. (Editable a) => Type (p a) -> ((Editable (p a)) => r) -> r;
    };

    instance Editable1 [] where
    {
        editable1 _ r = r;
    };

    instance Editable1 Maybe where
    {
        editable1 _ r = r;
    };

    instance Editable1 (Result err) where
    {
        editable1 _ r = r;
    };
-}
}
