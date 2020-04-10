module Truth.Core.Read
    ( module I
    , module Truth.Core.Read
    ) where

import Truth.Core.Import
import Truth.Core.Read.FullSubjectReader as I
import Truth.Core.Read.ReadM as I
import Truth.Core.Read.Readable as I
import Truth.Core.Read.SubjectReader as I

data ConstReader a t where
    MkConstReader :: a -> ConstReader a a
    -- not terribly useful

instance SubjectReader (ConstReader a) where
    type ReaderSubject (ConstReader a) = a
    mSubjectToReadable _ (MkConstReader a) = return a
    -- only reason to specify mSubjectToReadable instead of subjectToRead?
