module Changes.Core.Read
    ( module I
    , module Changes.Core.Read
    )
where

import Changes.Core.Import
import Changes.Core.Read.FullSubjectReader as I
import Changes.Core.Read.ReadM as I
import Changes.Core.Read.Readable as I
import Changes.Core.Read.SubjectReader as I

data ConstReader a t where
    MkConstReader :: a -> ConstReader a a

-- not terribly useful

instance SubjectReader (ConstReader a) where
    type ReaderSubject (ConstReader a) = a
    mSubjectToReadable _ (MkConstReader a) = return a

-- only reason to specify mSubjectToReadable instead of subjectToRead?
