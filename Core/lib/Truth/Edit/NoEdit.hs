module Truth.Edit.NoEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;


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

{-
    instance HasInfo NoReader where
    {
        info = mkSimpleInfo $(iowitness[t|NoReader|])
        [
            -- instance Reader (NoReader a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return (Reader_Inst a);
            }))
            :: FactS FactZ Reader_Inst (Type_KTKTT NoReader)
            )
        ];
    };
-}
    -- | Can't touch this.
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
        replaceEdit = return [];
    };


{-
    instance HasInfo NoEdit where
    {
        info = mkSimpleInfo $(iowitness[t|NoEdit|])
        [
            -- instance (Reader reader) => Edit (NoEdit reader)
            mkFacts (MkFactS (\reader -> MkFactZ (do
            {
                Kind_KTT <- matchProp $(type1[t|Kind_KTT|]) reader;
                Reader_Inst _subject <- matchProp $(type1[t|Reader_Inst|]) reader;
                return (Edit_Inst reader);
            }))
            :: FactS FactZ Edit_Inst (Type_KKTTT NoEdit)
            )
        ];
    };
-}
}
