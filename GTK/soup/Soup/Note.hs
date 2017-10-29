module Soup.Note where
{
    import Shapes;
    import qualified Data.Aeson as JSON;
    import qualified Data.Aeson.Types as JSON;
    import Truth.Core;
    import Truth.World.JSON;


    data NoteSel t where
    {
        NoteTitle :: NoteSel (WholeEdit String);
        NotePast :: NoteSel (WholeEdit Bool);
        NoteText :: NoteSel (StringEdit Text);
    };

    instance TestEquality NoteSel where
    {
        testEquality NoteTitle NoteTitle = Just Refl;
        testEquality NotePast NotePast = Just Refl;
        testEquality NoteText NoteText = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance TupleWitness Edit NoteSel where
    {
        tupleWitness _ NoteTitle = Dict;
        tupleWitness _ NotePast = Dict;
        tupleWitness _ NoteText = Dict;
    };

    instance TupleWitness FullEdit NoteSel where
    {
        tupleWitness _ NoteTitle = Dict;
        tupleWitness _ NotePast = Dict;
        tupleWitness _ NoteText = Dict;
    };

    instance TupleReaderWitness SubjectReader NoteSel where
    {
        tupleReaderWitness _ NoteTitle = Dict;
        tupleReaderWitness _ NotePast = Dict;
        tupleReaderWitness _ NoteText = Dict;
    };

    instance TupleReaderWitness FullSubjectReader NoteSel where
    {
        tupleReaderWitness _ NoteTitle = Dict;
        tupleReaderWitness _ NotePast = Dict;
        tupleReaderWitness _ NoteText = Dict;
    };

    instance SubjectTupleSelector NoteSel;

    instance FiniteTupleSelector NoteSel where
    {
        tupleConstruct getVal = (\title past text -> MkTuple $ \case
        {
            NoteTitle -> title;
            NotePast -> past;
            NoteText -> text;
        }) <$> (getVal NoteTitle) <*> (getVal NotePast) <*> (getVal NoteText);
    };

    instance HasNewValue (Tuple NoteSel) where
    {
        newValue = MkTuple $ \case
        {
            NoteTitle -> "untitled";
            NotePast -> False;
            NoteText -> mempty;
        };
    };

    type NoteEdit = TupleEdit NoteSel;

    noteEditSpec :: UISpec NoteEdit;
    noteEditSpec = uiVertical $ tupleEditUISpecs $ \case
    {
        NoteTitle -> uiTextEntry;
        NotePast -> uiCheckbox "past";
        NoteText -> uiTextText;
    };

    type Note = Tuple NoteSel;

    instance JSON.ToJSON Note where
    {
        toJSON (MkTuple sel) = JSON.Object $ mapFromList
        [
            (fromString "title",JSON.toJSON $ sel NoteTitle),
            (fromString "past",JSON.toJSON $ sel NotePast),
            (fromString "",JSON.toJSON $ sel NoteText)
        ];
    };

    parseMaybe :: Maybe a -> JSON.Parser a;
    parseMaybe (Just a) = return a;
    parseMaybe Nothing = empty;

    parseField :: JSON.FromJSON a => String -> JSON.Object -> JSON.Parser a;
    parseField key obj = do
    {
        val <- parseMaybe $ lookup (fromString key) obj;
        JSON.parseJSON val;
    };

    instance JSON.FromJSON Note where
    {
        parseJSON value = do
        {
            obj :: JSON.Object <- JSON.parseJSON value;
            title :: String <- parseField "title" obj;
            past :: Bool <- parseField "past" obj;
            text :: Text <- parseField "" obj;
            return $ MkTuple $ \sel -> case sel of
            {
                NoteTitle -> title;
                NotePast -> past;
                NoteText -> text;
            };
        }
    };

    noteCodec :: ReasonCodec ByteString (EditSubject NoteEdit);
    noteCodec = jsonValueCodec . jsonCodec;
}
