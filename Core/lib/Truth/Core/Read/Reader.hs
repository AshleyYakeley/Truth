module Truth.Core.Read.Reader where
{
    import Truth.Core.Import;


    type MutableRead m reader = forall t. reader t -> m t;

    -- | The values of the reader type are MutableEdit calls that read parts of something of type (ReaderSubject reader).
    class Reader (reader :: * -> *) where
    {
        type ReaderSubject reader :: *;

        -- | Make MutableEdit calls when you've actually got the subject
        readFromM :: forall m. Monad m => m (ReaderSubject reader) -> MutableRead m reader;
        readFromM msubj reader = fmap (\subj -> readFrom subj reader) msubj;

        readFrom :: ReaderSubject reader -> (forall t. reader t -> t);
        readFrom subj reader = runIdentity (readFromM (Identity subj) reader);
    };

    instance HasInfo Reader where
    {
        info = mkSimpleInfo $(iowitness[t|Reader|]) [];
    };

    $(typeFamilyProxy "ReaderSubject");
}
