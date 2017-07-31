module Data.HasNewValue where
{
    import Data.Word;
    import Data.ByteString.Lazy;
    import Data.Result;


    class HasNewValue a where
    {
        newValue :: a;
    };

    instance HasNewValue () where
    {
        newValue = ();
    };

    instance HasNewValue Bool where
    {
        newValue = False;
    };

    instance HasNewValue Int where
    {
        newValue = 0;
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
        newValue1 :: forall proxy a r. (HasNewValue a) => proxy (p a) -> ((HasNewValue (p a)) => r) -> r;
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
