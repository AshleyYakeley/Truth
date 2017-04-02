module Truth.Edit.Read.Readable where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read.Reader;


    newtype Readable reader a = MkReadable { unReadable :: forall m. Monad m => Structure m reader -> m a};

    fromReadable :: (Reader reader) => Readable reader t -> (ReaderSubject reader) -> t;
    fromReadable rrt a = runIdentity (unReadable rrt (Identity . (readFrom a)));

    instance Functor (Readable reader) where
    {
        fmap ab (MkReadable sma) = MkReadable (\s -> fmap ab (sma s));
    };

    instance Applicative (Readable reader) where
    {
        pure a = MkReadable $ \_ -> pure a;
        (MkReadable smab) <*> (MkReadable sma) = MkReadable $ \s -> smab s <*> sma s;
    };

    instance Monad (Readable reader) where
    {
        return = pure;
        (MkReadable sma) >>= f = MkReadable $ \s -> do
        {
            a <- sma s;
            unReadable (f a) s;
        };
    };
}
