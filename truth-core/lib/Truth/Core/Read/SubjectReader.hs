module Truth.Core.Read.SubjectReader where

import Truth.Core.Import
import Truth.Core.Read.MutableRead

-- | The values of the reader type are MutableEdit calls that read parts of something of type (ReaderSubject reader).
class SubjectReader (reader :: * -> *) where
    type ReaderSubject reader :: *
    -- | Make MutableEdit calls when you've actually got the subject
    mSubjectToMutableRead ::
           forall m. Monad m
        => m (ReaderSubject reader)
        -> MutableRead m reader
    mSubjectToMutableRead msubj reader = fmap (\subj -> subjectToRead subj reader) msubj
    subjectToRead :: ReaderSubject reader -> (forall t. reader t -> t)
    subjectToRead subj reader = runIdentity (mSubjectToMutableRead (Identity subj) reader)

subjectToMutableRead :: (SubjectReader reader, Monad m) => ReaderSubject reader -> MutableRead m reader
subjectToMutableRead subj = mSubjectToMutableRead $ return subj
