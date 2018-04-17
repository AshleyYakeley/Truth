module Truth.Core.Read.FullSubjectReader where

import Truth.Core.Import
import Truth.Core.Read.MutableRead
import Truth.Core.Read.SubjectReader

class (SubjectReader reader) => FullSubjectReader (reader :: * -> *) where
    mutableReadToSubject ::
           forall m. MonadIO m
        => MutableRead m reader
        -> m (ReaderSubject reader)
    -- ^ Construct the subject by making MutableRead calls
