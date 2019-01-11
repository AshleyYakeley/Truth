module Truth.Core.Read.FullSubjectReader where

import Truth.Core.Import
import Truth.Core.Read.MutableRead
import Truth.Core.Read.SubjectReader

class (SubjectReader reader) => FullSubjectReader (reader :: Type -> Type) where
    mutableReadToSubject ::
           forall m. MonadIO m
        => MutableRead m reader
        -> m (ReaderSubject reader)
    -- ^ Construct the subject by making MutableRead calls

ioFuncReadFunction ::
       (FullSubjectReader ra, SubjectReader rb) => (ReaderSubject ra -> IO (ReaderSubject rb)) -> ReadFunction ra rb
ioFuncReadFunction f mra =
    mSubjectToMutableRead $ do
        a <- mutableReadToSubject mra
        liftIO $ f a

funcReadFunction ::
       (FullSubjectReader ra, SubjectReader rb) => (ReaderSubject ra -> ReaderSubject rb) -> ReadFunction ra rb
funcReadFunction f = ioFuncReadFunction $ return . f
