module Truth.Core.Types.None where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

newtype NoReader (a :: Type) (t :: Type) =
    MkNoReader None
    deriving (Eq, Countable, Searchable)

instance TestEquality (NoReader a) where
    testEquality = never

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
newtype NoEdit (reader :: Type -> Type) =
    MkNoEdit None
    deriving (Eq, Countable, Searchable)

instance Finite (NoEdit reader) where
    allValues = []

deriving instance Empty (NoEdit reader)

instance Show (NoEdit reader) where
    show edit = never edit

instance Floating (NoEdit reader) (NoEdit reader)

type instance EditReader (NoEdit reader) = reader

instance ApplicableEdit (NoEdit reader) where
    applyEdit edit _ = never edit

instance InvertibleEdit (NoEdit reader) where
    invertEdit edit _ = never edit

instance FullSubjectReader reader => SubjectMapEdit (NoEdit reader)

instance (FullSubjectReader reader, ReaderSubject reader ~ ()) => FullEdit (NoEdit reader) where
    replaceEdit _ _ = return ()

instance TestEquality reader => CacheableEdit (NoEdit reader)

type NoUpdate reader = EditUpdate (NoEdit reader)

elPutEditsNone ::
       forall edita readerb m m'. (Monad m', MonadIO m)
    => [NoEdit readerb]
    -> MutableRead m (EditReader edita)
    -> m' (Maybe [edita])
elPutEditsNone [] _ = return $ Just []
elPutEditsNone (e:_) _ = never e
