module Truth.Core.Types.Either where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data EitherReader ra rb t = LeftReader (ra t) | RightReader (rb t);

    instance (Reader ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => Reader (EitherReader ra rb) where
    {
        type ReaderSubject (EitherReader ra rb) = ReaderSubject ra;

        readFromM msubj (LeftReader reader) = readFromM msubj reader;
        readFromM msubj (RightReader reader) = readFromM msubj reader;

        readFrom subj (LeftReader reader) = readFrom subj reader;
        readFrom subj (RightReader reader) = readFrom subj reader;
    };

    data EitherEdit ea eb = LeftEdit ea | RightEdit eb;

    instance Floating (EitherEdit ea eb) (EitherEdit ea eb);

    instance (Edit ea,Edit eb,EditReader ea ~ EditReader eb) => Edit (EitherEdit ea eb) where
    {
        type EditReader (EitherEdit ea eb) = EditReader ea;

        applyEdit (LeftEdit edit) = applyEdit edit;
        applyEdit (RightEdit edit) = applyEdit edit;

        invertEdit (LeftEdit edit) = fmap (fmap LeftEdit) (invertEdit edit);
        invertEdit (RightEdit edit) = fmap (fmap RightEdit) (invertEdit edit);
    };

    instance (FullEdit ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit (EitherEdit ea eb) where
    {
        replaceEdit = fmap (fmap LeftEdit) replaceEdit;
    };

    $(return []);
    instance HasInfo EitherEdit where
    {
        info = mkSimpleInfo $(iowitness[t|EitherEdit|]) [$(declInfo [d|
            instance (Edit ea,Edit eb,EditReader ea ~ EditReader eb) => Edit (EitherEdit ea eb) where
            {
                type EditReader (EitherEdit ea eb) = EditReader ea;
            };
            instance (FullEdit ea,Edit eb,EditReader ea ~ EditReader eb) => FullEdit (EitherEdit ea eb);
        |])];
    };

    eitherEditFunction :: (EditReader edit ~ EditReader edit') => CleanEditFunction edit (EitherEdit edit' edit);
    eitherEditFunction = MkCleanEditFunction
    {
        cleanEditGet = id,
        cleanEditUpdate = \edit -> return (RightEdit edit)
    };
}
