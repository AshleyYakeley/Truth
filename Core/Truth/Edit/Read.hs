module Truth.Edit.Read where
{
    import Truth.Edit.Import;

    type Structure m reader = forall t. reader t -> m t;

    -- | The values of the reader type are API calls that read parts of something of type (Subject reader).
    class Reader (reader :: * -> *) where
    {
        type Subject reader :: *;

        -- | Make API calls when you've actually got the subject
        readFromM :: forall m. (Applicative m,Monad m) => m (Subject reader) -> Structure m reader;
        readFromM msubj reader = fmap (\subj -> readFrom subj reader) msubj;

        readFrom :: Subject reader -> (forall t. reader t -> t);
        readFrom subj reader = runIdentity (readFromM (Identity subj) reader);
    };
    data Reader_Inst t where
    {
        Reader_Inst :: forall reader. (Reader reader) => Info (Type_T (Subject reader)) -> Reader_Inst (Type_KTT reader);
    };
    $(factInstances [t|Reader_Inst|]);

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

    class (Reader reader) => FullReader (reader :: * -> *) where
    {
        -- | Construct the subject by making API calls
        fromReader :: Readable reader (Subject reader);
    };
    data FullReader_Inst t where
    {
        FullReader_Inst :: forall reader. (FullReader reader) => FullReader_Inst (Type_KTT reader);
    };
    $(factInstances [t|FullReader_Inst|]);


    -- not terribly useful
    data ConstReader a t where
    {
        MkConstReader :: a -> ConstReader a a;
    };

    -- only reason to specify readFromM instead of readFrom?
    instance Reader (ConstReader a) where
    {
        type Subject (ConstReader a) = a;
        readFromM _ (MkConstReader a) = return a;
    };
}
