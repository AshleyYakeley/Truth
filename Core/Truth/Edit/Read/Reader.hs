module Truth.Edit.Read.Reader where
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

    data Reader_Inst :: (* -> *) -> * where
    {
        Reader_Inst :: forall reader. (Reader reader) => Info_W (WrapType (Subject reader)) -> Reader_Inst reader;
    };

    instance HasInfo Reader_Inst where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType Reader_Inst|]) [];
    };
}
