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
    mSubjectToMutableRead _ = never
    subjectToRead _ = never

instance FullSubjectReader (NoReader ()) where
    mutableReadToSubject _ = return ()

-- | Can't touch this.
newtype NoEdit (reader :: * -> *) =
    MkNoEdit None
    deriving (Eq, Countable, Searchable)

instance Finite (NoEdit reader) where
    allValues = []

deriving instance Empty (NoEdit reader)

instance Show (NoEdit reader) where
    show edit = never edit

instance Floating (NoEdit reader) (NoEdit reader)

type instance EditReader (NoEdit reader) = reader

instance Edit (NoEdit reader) where
    applyEdit edit _ = never edit

instance InvertibleEdit (NoEdit reader) where
    invertEdit edit _ = never edit

instance (FullSubjectReader reader, ReaderSubject reader ~ ()) => FullEdit (NoEdit reader) where
    replaceEdit _ _ = return ()

noEditLens :: forall edit. EditLens (NoEdit (EditReader edit)) edit
noEditLens = let
    efGet :: ReadFunctionT IdentityT (EditReader edit) (EditReader edit)
    efGet mr = remonadMutableRead IdentityT mr
    efUpdate edit _ = never edit
    elFunction = MkAnEditFunction {..}
    elPutEdits _ _ = return Nothing
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}
