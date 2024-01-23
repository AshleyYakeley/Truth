module Changes.Core.Read.FullSubjectReader where

import Changes.Core.Import
import Changes.Core.Read.Readable
import Changes.Core.Read.SubjectReader

class SubjectReader reader => FullSubjectReader (reader :: Type -> Type) where
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
