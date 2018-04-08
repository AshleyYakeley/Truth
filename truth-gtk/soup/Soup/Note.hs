module Soup.Note where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Shapes
import Truth.Core
import Truth.World.JSON

data NoteSel t where
    NoteTitle :: NoteSel (WholeEdit Text)
    NotePast :: NoteSel (WholeEdit Bool)
    NoteText :: NoteSel (StringEdit Text)

instance (c (WholeEdit Text), c (WholeEdit Bool), c (StringEdit Text)) => WitnessConstraint c NoteSel where
    witnessConstraint NoteTitle = Dict
    witnessConstraint NotePast = Dict
    witnessConstraint NoteText = Dict

instance Show (NoteSel t) where
    show NoteTitle = "title"
    show NotePast = "past"
    show NoteText = "text"

instance AllWitnessConstraint Show NoteSel where
    allWitnessConstraint = Dict

instance (c (WholeReader Text), c (WholeReader Bool), c (StringRead Text)) => TupleReaderWitness c NoteSel where
    tupleReaderWitness NoteTitle = Dict
    tupleReaderWitness NotePast = Dict
    tupleReaderWitness NoteText = Dict

instance (c Text, c Bool, c Text) => TupleSubjectWitness c NoteSel where
    tupleSubjectWitness NoteTitle = Dict
    tupleSubjectWitness NotePast = Dict
    tupleSubjectWitness NoteText = Dict

instance FiniteWitness NoteSel where
    assembleWitnessF getw =
        (\a b c ->
             MkAllF $ \case
                 NoteTitle -> a
                 NotePast -> b
                 NoteText -> c) <$>
        getw NoteTitle <*>
        getw NotePast <*>
        getw NoteText

instance TestEquality NoteSel where
    testEquality NoteTitle NoteTitle = Just Refl
    testEquality NotePast NotePast = Just Refl
    testEquality NoteText NoteText = Just Refl
    testEquality _ _ = Nothing

instance TupleWitness ApplicableEdit NoteSel where
    tupleWitness NoteTitle = Dict
    tupleWitness NotePast = Dict
    tupleWitness NoteText = Dict

instance TupleWitness FullEdit NoteSel where
    tupleWitness NoteTitle = Dict
    tupleWitness NotePast = Dict
    tupleWitness NoteText = Dict

instance SubjectTupleSelector NoteSel

instance FiniteTupleSelector NoteSel where
    tupleConstruct getVal =
        (\title past text ->
             MkTuple $ \case
                 NoteTitle -> title
                 NotePast -> past
                 NoteText -> text) <$>
        (getVal NoteTitle) <*>
        (getVal NotePast) <*>
        (getVal NoteText)

instance HasNewValue (Tuple NoteSel) where
    newValue =
        MkTuple $ \case
            NoteTitle -> "untitled"
            NotePast -> False
            NoteText -> mempty

type NoteEdit = TupleEdit NoteSel

noteEditSpec :: UISpec NoteEdit
noteEditSpec =
    uiVertical $
    tupleEditUISpecs $ \case
        NoteTitle -> (uiTextEntry, False)
        NotePast -> (uiCheckbox (constEditFunction "past") id, False)
        NoteText -> (uiText, True)

type Note = Tuple NoteSel

instance JSON.ToJSON Note where
    toJSON (MkTuple sel) =
        JSON.Object $
        mapFromList
            [ (fromString "title", JSON.toJSON $ sel NoteTitle)
            , (fromString "past", JSON.toJSON $ sel NotePast)
            , (fromString "", JSON.toJSON $ sel NoteText)
            ]

parseMaybe :: Maybe a -> JSON.Parser a
parseMaybe (Just a) = return a
parseMaybe Nothing = empty

parseField :: JSON.FromJSON a => Text -> JSON.Object -> JSON.Parser a
parseField key obj = do
    val <- parseMaybe $ lookup key obj
    JSON.parseJSON val

instance JSON.FromJSON Note where
    parseJSON value = do
        obj :: JSON.Object <- JSON.parseJSON value
        title :: Text <- parseField "title" obj
        past :: Bool <- parseField "past" obj
        text :: Text <- parseField "" obj
        return $
            MkTuple $ \sel ->
                case sel of
                    NoteTitle -> title
                    NotePast -> past
                    NoteText -> text

noteCodec :: ReasonCodec ByteString (EditSubject NoteEdit)
noteCodec = jsonValueCodec . jsonCodec
