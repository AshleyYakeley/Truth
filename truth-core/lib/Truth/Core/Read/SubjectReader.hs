module Truth.Core.Read.SubjectReader where

import Truth.Core.Import
import Truth.Core.Read.Readable

-- | The values of the reader type are MutableEdit calls that read parts of something of type (ReaderSubject reader).
class SubjectReader (reader :: Type -> Type) where
    type ReaderSubject reader :: Type
    -- | Make MutableEdit calls when you've actually got the subject
    mSubjectToReadable ::
           forall m. Monad m
        => m (ReaderSubject reader)
        -> Readable m reader
    mSubjectToReadable msubj reader = fmap (\subj -> subjectToRead subj reader) msubj
    subjectToRead :: ReaderSubject reader -> (forall t. reader t -> t)
    subjectToRead subj reader = runIdentity (mSubjectToReadable (Identity subj) reader)

subjectToReadable ::
       forall m reader. (SubjectReader reader, Monad m)
    => ReaderSubject reader
    -> Readable m reader
subjectToReadable subj = mSubjectToReadable $ return subj
