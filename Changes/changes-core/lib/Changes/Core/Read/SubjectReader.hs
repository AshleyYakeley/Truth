module Changes.Core.Read.SubjectReader where

import Changes.Core.Import
import Changes.Core.Read.Readable

-- | The values of the reader type are MutableEdit calls that read parts of something of type (ReaderSubject reader).
class SubjectReader (reader :: Type -> Type) where
    type ReaderSubject reader :: Type

    -- | Make MutableEdit calls when you've actually got the subject
    mSubjectToReadable ::
        forall m.
        Monad m =>
        m (ReaderSubject reader) ->
        Readable m reader
    mSubjectToReadable msubj rd = fmap (\subj -> subjectToRead subj rd) msubj

    subjectToRead :: ReaderSubject reader -> (forall t. reader t -> t)
    subjectToRead subj rd = runIdentity (mSubjectToReadable (Identity subj) rd)

subjectToReadable ::
    forall m reader.
    (SubjectReader reader, Monad m) =>
    ReaderSubject reader ->
    Readable m reader
subjectToReadable subj = mSubjectToReadable $ return subj
