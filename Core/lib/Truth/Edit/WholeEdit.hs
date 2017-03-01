module Truth.Edit.WholeEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;


    data WholeReader (a :: *) (t :: *) where
    {
        ReadWhole :: forall t. WholeReader t t;
    };

    instance Reader (WholeReader a) where
    {
        type ReaderSubject (WholeReader a) = a;
        readFrom msubj ReadWhole = msubj;
    };

    instance FullReader (WholeReader a) where
    {
        fromReader = readable ReadWhole;
    };
    instance HasInfo WholeReader where
    {
        info = mkSimpleInfo $(iowitness[t|WholeReader|])
        [
{-
            -- instance Reader (WholeReader a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return (Reader_Inst a);
            }))
            :: FactS FactZ Reader_Inst (Type_KTKTT WholeReader)
            )
-}
        ];
    };

    newtype WholeEdit (reader :: * -> *) = MkWholeEdit (ReaderSubject reader);

    instance Floating (WholeEdit reader) where
    {
        type FloatingEdit (WholeEdit reader) = WholeEdit reader;
    };

    instance (FullReader reader) => Edit (WholeEdit reader) where
    {
        type EditReader (WholeEdit reader) = reader;
        applyEdit (MkWholeEdit a) = readFromM (return a);
        invertEdit _ = do
        {
            a <- fromReader;
            return [MkWholeEdit a];
        };
    };

    instance (FullReader reader) => FullEdit (WholeEdit reader) where
    {
        replaceEdit = MkWholeEdit;
    };

    instance HasInfo WholeEdit where
    {
        info = mkSimpleInfo $(iowitness[t|WholeEdit|])
        [
    {-
            -- instance Edit (WholeEdit reader)
            mkFacts (MkFactS (\reader -> MkFactZ (do
            {
                Kind_KTT <- matchProp $(type1[t|Kind_KTT|]) reader;
                FullReader_Inst <- matchProp $(type1[t|FullReader_Inst|]) reader;
                return (Edit_Inst reader);
            }))
            :: FactS FactZ Edit_Inst (Type_KKTTT WholeEdit)
            ),
            mkFacts (MkFactS (\reader -> MkFactZ (do
            {
                Kind_KTT <- matchProp $(type1[t|Kind_KTT|]) reader;
                FullReader_Inst <- matchProp $(type1[t|FullReader_Inst|]) reader;
                return FullEdit_Inst;
            }))
            :: FactS FactZ FullEdit_Inst (Type_KKTTT WholeEdit)
            )
    -}
        ];
    };
}
