module Changes.Core.Types.List.Read where

import Changes.Core.Import
import Changes.Core.Read
import Changes.Core.Sequence

data ListReader reader t where
    ListReadLength :: ListReader reader SequencePoint
    ListReadItem :: SequencePoint -> reader t -> ListReader reader (Maybe t)

itemReadFunction :: SequencePoint -> ReadFunctionF Maybe (ListReader reader) reader
itemReadFunction i mr rt = MkComposeM $ mr $ ListReadItem i rt

knownItemReadFunction :: SequencePoint -> ReadFunction (ListReader reader) reader
knownItemReadFunction i mr rt = do
    mt <- getComposeM $ itemReadFunction i mr rt
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item " ++ show i ++ " in list"

instance SubjectReader reader => SubjectReader (ListReader reader) where
    type ReaderSubject (ListReader reader) = Vector (ReaderSubject reader)
    subjectToRead sq ListReadLength = seqLength sq
    subjectToRead sq (ListReadItem i reader) = fmap (\e -> subjectToRead e reader) $ seqIndex sq i

instance FullSubjectReader reader => FullSubjectReader (ListReader reader) where
    readableToSubject mr = do
        len <- mr ListReadLength
        list <- for [0 .. pred len] $ \i -> readableToSubject $ knownItemReadFunction i mr
        return $ fromList list
