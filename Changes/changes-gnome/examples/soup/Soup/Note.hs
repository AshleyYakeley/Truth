module Soup.Note where

import Changes.Core
import Changes.World.GNOME.GTK
import Changes.World.JSON
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.Aeson.Types qualified as JSON
import Shapes

data NoteSel t where
    NoteTitle :: NoteSel (WholeUpdate Text)
    NotePast :: NoteSel (WholeUpdate Bool)
    NoteText :: NoteSel (StringUpdate Text)

instance Show (NoteSel t) where
    show NoteTitle = "title"
    show NotePast = "past"
    show NoteText = "text"

instance AllConstraint Show NoteSel where
    allConstraint = Dict

instance (c Text, c Bool, c Text) => TupleSubjectWitness c NoteSel where
    tupleSubjectWitness NoteTitle = Dict
    tupleSubjectWitness NotePast = Dict
    tupleSubjectWitness NoteText = Dict

instance (c (WholeReader Text), c (WholeReader Bool), c (StringRead Text)) => TupleReaderWitness c NoteSel where
    tupleReaderWitness NoteTitle = Dict
    tupleReaderWitness NotePast = Dict
    tupleReaderWitness NoteText = Dict

instance (c (WholeEdit Text), c (WholeEdit Bool), c (StringEdit Text)) => TupleEditWitness c NoteSel where
    tupleEditWitness NoteTitle = Dict
    tupleEditWitness NotePast = Dict
    tupleEditWitness NoteText = Dict

instance (c (WholeUpdate Text), c (WholeUpdate Bool), c (StringUpdate Text)) => TupleUpdateWitness c NoteSel where
    tupleUpdateWitness NoteTitle = Dict
    tupleUpdateWitness NotePast = Dict
    tupleUpdateWitness NoteText = Dict

instance FiniteWitness NoteSel where
    assembleAllFor getw =
        (\a b c ->
             MkAllFor $ \case
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

instance SubjectTupleSelectorRead NoteSel

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

type NoteUpdate = TupleUpdate NoteSel

noteEditSpec :: Model NoteUpdate -> SelectNotify TextSelection -> GView 'Unlocked Widget
noteEditSpec sub sel = do
    titleUI <- createTextEntry $ mapModel (tupleChangeLens NoteTitle) sub
    pastUI <- createCheckButton (constantModel "past") $ mapModel (tupleChangeLens NotePast) sub
    textUI <- createTextView (mapModel (tupleChangeLens NoteText) sub) sel
    createLayout
        OrientationVertical
        [ (defaultLayoutOptions, titleUI)
        , (defaultLayoutOptions, pastUI)
        , (defaultLayoutOptions {loGrow = True}, textUI)
        ]

type Note = Tuple NoteSel

instance JSON.ToJSON Note where
    toJSON (MkTuple sel) =
        JSON.Object $
        JSON.fromList
            [ (fromString "title", JSON.toJSON $ sel NoteTitle)
            , (fromString "past", JSON.toJSON $ sel NotePast)
            , (fromString "", JSON.toJSON $ sel NoteText)
            ]

parseField :: JSON.FromJSON a => Text -> JSON.Object -> JSON.Parser a
parseField key obj = do
    val <- mpure $ JSON.lookup (JSON.fromText key) obj
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

noteCodec :: ReasonCodec LazyByteString (UpdateSubject NoteUpdate)
noteCodec = jsonValueCodec . jsonCodec
