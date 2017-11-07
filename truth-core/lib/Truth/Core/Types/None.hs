module Truth.Core.Types.None where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

newtype NoReader (a :: *) (t :: *) =
    MkNoReader None
    deriving (Eq, Countable, Searchable)

instance Finite (NoReader a t) where
    allValues = []

deriving instance Empty (NoReader a t)

instance SubjectReader (NoReader a) where
    type ReaderSubject (NoReader a) = a
    readFromSubjectM _ = never
    readFromSubject _ = never

instance FullSubjectReader (NoReader ()) where
    subjectFromReader = return ()

-- | Can't touch this.
newtype NoEdit (reader :: * -> *) =
    MkNoEdit None
    deriving (Eq, Countable, Searchable)

instance Finite (NoEdit reader) where
    allValues = []

deriving instance Empty (NoEdit reader)

instance Floating (NoEdit reader) (NoEdit reader)

instance Edit (NoEdit reader) where
    type EditReader (NoEdit reader) = reader
    applyEdit = never

instance InvertableEdit (NoEdit reader) where
    invertEdit = never

instance (FullSubjectReader reader, ReaderSubject reader ~ ()) => FullEdit (NoEdit reader) where
    replaceEdit = return ()

noEditFunction :: PureEditFunction (NoEdit (EditReader edit)) edit
noEditFunction = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet _ = readable
    editUpdate = never
    in MkEditFunction {..}

noEditLens :: PureEditLens (NoEdit (EditReader edit)) edit
noEditLens = let
    editLensFunction = noEditFunction
    editLensPutEdit () _ = return Nothing
    in MkEditLens {..}
