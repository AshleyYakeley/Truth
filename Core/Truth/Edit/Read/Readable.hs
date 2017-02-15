module Truth.Edit.Read.Readable where
{
    import Truth.Edit.Read.Reader;
    import Truth.Edit.Import;

    newtype Readable reader a = MkReadable { unReadable :: forall m. (Applicative m,Monad m) => Structure m reader -> m a};

    fromReadable :: (Reader reader) => Readable reader t -> (Subject reader) -> t;
    fromReadable rrt a = runIdentity (unReadable rrt (Identity . (readFrom a)));

    instance Functor (Readable reader) where
    {
        fmap ab (MkReadable sma) = MkReadable (\s -> mmap ab (sma s));
    };

    instance Applicative (Readable reader) where
    {
        pure a = MkReadable (\_ -> return a);
        (MkReadable smab) <*> (MkReadable sma) = MkReadable (\s -> do
        {
            ab <- smab s;
            a <- sma s;
            return (ab a);
        });
    };

    instance Monad (Readable reader) where
    {
        return a = MkReadable (\_ -> return a);
        (MkReadable sma) >>= f = MkReadable (\s -> do
        {
            a <- sma s;
            unReadable (f a) s;
        });
    };
}
