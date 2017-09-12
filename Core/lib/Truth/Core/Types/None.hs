module Truth.Core.Types.None where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    newtype NoReader (a :: *) (t :: *) = MkNoReader None deriving (Eq,Countable,Searchable);

    instance Finite (NoReader a t) where
    {
        allValues = [];
    };

    deriving instance Empty (NoReader a t);

    instance Reader (NoReader a) where
    {
        type ReaderSubject (NoReader a) = a;
        readFromM _ = never;
        readFrom _ = never;
    };

    instance FullReader (NoReader ()) where
    {
        fromReader = return ();
    };

    -- | Can't touch this.
    ;
    newtype NoEdit (reader :: * -> *) = MkNoEdit None deriving (Eq,Countable,Searchable);

    instance Finite (NoEdit reader) where
    {
        allValues = [];
    };

    deriving instance Empty (NoEdit reader);

    instance Floating (NoEdit reader) (NoEdit reader);

    instance (Reader reader) => Edit (NoEdit reader) where
    {
        type EditReader (NoEdit reader) = reader;
        applyEdit = never;
        invertEdit = never;
    };

    instance (FullReader reader,ReaderSubject reader ~ ()) => FullEdit (NoEdit reader) where
    {
        replaceEdit = return ();
    };

    noEditFunction :: EditFunction () (NoEdit (EditReader edit)) edit;
    noEditFunction = let
    {
        editInitial = ();
        editGet _ = readable;
        editUpdate = never;
    } in MkEditFunction{..};

    noEditLens :: EditLens' Maybe () (NoEdit (EditReader edit)) edit;
    noEditLens = let
    {
        editLensFunction = noEditFunction;
        editLensPutEdit () _ = return Nothing;
    } in MkEditLens{..};
}
