module Truth.Core.Read.FullSubjectReader where

import Truth.Core.Import
import Truth.Core.Read.Readable
import Truth.Core.Read.SubjectReader

class (SubjectReader reader) => FullSubjectReader (reader :: Type -> Type) where
    readableToSubject ::
           forall m. MonadIO m
        => Readable m reader
        -> m (ReaderSubject reader)
    -- ^ Construct the subject by making Readable calls

ioFuncReadFunction ::
       (FullSubjectReader ra, SubjectReader rb) => (ReaderSubject ra -> IO (ReaderSubject rb)) -> ReadFunction ra rb
ioFuncReadFunction f mra =
    mSubjectToReadable $ do
        a <- readableToSubject mra
        liftIO $ f a

funcReadFunction ::
       (FullSubjectReader ra, SubjectReader rb) => (ReaderSubject ra -> ReaderSubject rb) -> ReadFunction ra rb
funcReadFunction f = ioFuncReadFunction $ return . f
