module Truth.Core.Read
    ( module Truth.Core.Read.Readable
    , module Truth.Core.Read.SubjectReader
    , module Truth.Core.Read.FullSubjectReader
    , module Truth.Core.Read
    ) where

import Truth.Core.Import
import Truth.Core.Read.FullSubjectReader
import Truth.Core.Read.Readable
import Truth.Core.Read.SubjectReader

data ConstReader a t where
    MkConstReader :: a -> ConstReader a a
    -- not terribly useful

instance SubjectReader (ConstReader a) where
    type ReaderSubject (ConstReader a) = a
    mSubjectToReadable _ (MkConstReader a) = return a
    -- only reason to specify mSubjectToReadable instead of subjectToRead?
