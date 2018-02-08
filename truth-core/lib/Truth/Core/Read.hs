module Truth.Core.Read
    ( module Truth.Core.Read.MutableRead
    , module Truth.Core.Read.SubjectReader
    , module Truth.Core.Read.FullSubjectReader
    , module Truth.Core.Read
    ) where

import Truth.Core.Import
import Truth.Core.Read.FullSubjectReader
import Truth.Core.Read.MutableRead
import Truth.Core.Read.SubjectReader

data ConstReader a t where
    MkConstReader :: a -> ConstReader a a
    -- not terribly useful

instance SubjectReader (ConstReader a) where
    type ReaderSubject (ConstReader a) = a
    mSubjectToMutableRead _ (MkConstReader a) = return a
    -- only reason to specify mSubjectToMutableRead instead of subjectToRead?
