module Truth.Core.Read.FullSubjectReader where

import Truth.Core.Import
import Truth.Core.Read.Readable
import Truth.Core.Read.SubjectReader

class (SubjectReader reader) =>
      FullSubjectReader (reader :: * -> *) where
    subjectFromReader :: Readable reader (ReaderSubject reader)
    -- ^ Construct the subject by making MutableEdit calls
