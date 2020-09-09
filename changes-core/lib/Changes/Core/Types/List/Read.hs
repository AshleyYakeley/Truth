module Truth.Core.Types.List.Read where

import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence

data ListReader seq reader t where
    ListReadLength :: ListReader seq reader (SequencePoint seq)
    ListReadItem :: SequencePoint seq -> reader t -> ListReader seq reader (Maybe t)

itemReadFunction :: SequencePoint seq -> ReadFunctionF Maybe (ListReader seq reader) reader
itemReadFunction i mr rt = MkComposeM $ mr $ ListReadItem i rt

knownItemReadFunction :: Integral (Index seq) => SequencePoint seq -> ReadFunction (ListReader seq reader) reader
knownItemReadFunction i mr rt = do
    mt <- getComposeM $ itemReadFunction i mr rt
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item " ++ show i ++ " in list"

instance (IsSequence seq, SubjectReader reader, ReaderSubject reader ~ Element seq) =>
             SubjectReader (ListReader seq reader) where
    type ReaderSubject (ListReader seq reader) = seq
    subjectToRead sq ListReadLength = seqLength sq
    subjectToRead sq (ListReadItem i reader) = fmap (\e -> subjectToRead e reader) $ seqIndex sq i

instance (IsSequence seq, FullSubjectReader reader, ReaderSubject reader ~ Element seq) =>
             FullSubjectReader (ListReader seq reader) where
    readableToSubject mr = do
        len <- mr ListReadLength
        list <- for [0 .. pred len] $ \i -> readableToSubject $ knownItemReadFunction i mr
        return $ fromList list
