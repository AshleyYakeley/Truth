module Changes.Core.Types.List.Read where

import Changes.Core.Import
import Changes.Core.Read
import Changes.Core.Sequence

data ListReader reader t where
    ListReadLength :: forall reader. ListReader reader SequencePoint
    ListReadItem :: forall reader t. SequencePoint -> reader t -> ListReader reader (Maybe t)

itemReadFunction :: forall reader. SequencePoint -> ReadFunctionF Maybe (ListReader reader) reader
itemReadFunction i mr rt = MkComposeInner $ mr $ ListReadItem i rt

knownItemReadFunction :: forall reader. SequencePoint -> ReadFunction (ListReader reader) reader
knownItemReadFunction i mr rt = do
    mt <- unComposeInner $ itemReadFunction i mr rt
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item " ++ show i ++ " in list"

instance forall reader. SubjectReader reader => SubjectReader (ListReader reader) where
    type ReaderSubject (ListReader reader) = Vector (ReaderSubject reader)
    subjectToRead sq ListReadLength = seqLength sq
    subjectToRead sq (ListReadItem i rd) = fmap (\e -> subjectToRead e rd) $ seqIndex sq i

instance forall reader. FullSubjectReader reader => FullSubjectReader (ListReader reader) where
    readableToSubject mr = do
        len <- mr ListReadLength
        list <- for [0 .. pred len] $ \i -> readableToSubject $ knownItemReadFunction i mr
        return $ fromList list
