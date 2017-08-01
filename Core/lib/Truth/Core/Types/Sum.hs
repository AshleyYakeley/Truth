module Truth.Core.Types.Sum where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data SumReader ra rb (t :: *) = SumReadLeft (ra t) | SumReadRight (rb t);

    instance (Reader ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => Reader (SumReader ra rb) where
    {
        type ReaderSubject (SumReader ra rb) = ReaderSubject ra;

        readFromM msubj (SumReadLeft reader) = readFromM msubj reader;
        readFromM msubj (SumReadRight reader) = readFromM msubj reader;

        readFrom subj (SumReadLeft reader) = readFrom subj reader;
        readFrom subj (SumReadRight reader) = readFrom subj reader;
    };

    $(return []);
    instance HasTypeInfo SumReader where
    {
        typeWitness = $(generateWitness [t|SumReader|]);
        typeName _ = "SumReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (Reader ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => Reader (SumReader ra rb) where
            {
                type ReaderSubject (SumReader ra rb) = ReaderSubject ra;
            };
        |]);
    };

    data SumEdit ea eb = SumEditLeft ea | SumEditRight eb;

    instance (Floating ea ea,Floating eb eb) => Floating (SumEdit ea eb) (SumEdit ea eb) where
    {
        floatingUpdate (SumEditLeft e1) (SumEditLeft e2) = SumEditLeft $ floatingUpdate e1 e2;
        floatingUpdate (SumEditRight e1) (SumEditRight e2) = SumEditRight $ floatingUpdate e1 e2;
        floatingUpdate _ t = t;
    };

    instance (Edit ea,Edit eb,EditReader ea ~ EditReader eb) => Edit (SumEdit ea eb) where
    {
        type EditReader (SumEdit ea eb) = EditReader ea;

        applyEdit (SumEditLeft edit) = applyEdit edit;
        applyEdit (SumEditRight edit) = applyEdit edit;

        invertEdit (SumEditLeft edit) = fmap (fmap SumEditLeft) (invertEdit edit);
        invertEdit (SumEditRight edit) = fmap (fmap SumEditRight) (invertEdit edit);
    };

    instance (FullEdit c ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit c (SumEdit ea eb) where
    {
        replaceEdit = reWriterReadable SumEditLeft replaceEdit;
    };

    $(return []);
    instance HasTypeInfo SumEdit where
    {
        typeWitness = $(generateWitness [t|SumEdit|]);
        typeName _ = "SumEdit";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (Edit ea,Edit eb,EditReader ea ~ EditReader eb) => Edit (SumEdit ea eb) where
            {
                type EditReader (SumEdit ea eb) = EditReader ea;
            };
            instance (FullEdit c ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit c (SumEdit ea eb);
        |]);
    };

    sumEditFunction :: (EditReader edit ~ EditReader edit') => PureEditFunction () edit (SumEdit edit' edit);
    sumEditFunction = let
    {
        editInitial = ();
        editGet () rt = readable rt;
        editUpdate edit () = return ((),[SumEditRight edit]);
    } in MkEditFunction{..};
}
