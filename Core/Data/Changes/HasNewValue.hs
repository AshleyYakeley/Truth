module Data.Changes.HasNewValue where
{
    import Data.Result;
    import Data.Witness;
    import Data.ByteString;
    import Data.Word;

    class HasNewValue a where
    {
        newValue :: a;
    };

    data HasNewValueInst f where
    {
        MkHasNewValueInst :: forall f. (HasNewValue f) => HasNewValueInst f;
    };

    instance HasNewValue ByteString where
    {
        newValue = empty;
    };

    instance HasNewValue Word8 where
    {
        newValue = 0;
    };

    instance HasNewValue Char where
    {
        newValue = '\0';
    };

    instance HasNewValue [a] where
    {
        newValue = [];
    };

    instance HasNewValue (Maybe a) where
    {
        newValue = Nothing;
    };

    instance (HasNewValue a) => HasNewValue (Result err a) where
    {
        newValue = SuccessResult newValue;
    };
    
    class HasNewValue1 p where
    {
        newValue1 :: forall a r. (HasNewValue a) => Type (p a) -> ((HasNewValue (p a)) => r) -> r;
    };

    instance HasNewValue1 [] where
    {
        newValue1 _ r = r;
    };

    instance HasNewValue1 Maybe where
    {
        newValue1 _ r = r;
    };

    instance HasNewValue1 (Result err) where
    {
        newValue1 _ r = r;
    };
}
