module Changes.Core.Types.None where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read

newtype NoReader (a :: Type) (t :: Type) =
    MkNoReader Void
    deriving newtype (Eq, Countable, Searchable)

instance TestEquality (NoReader a) where
    testEquality = never

instance Finite (NoReader a t) where
    allValues = []

deriving newtype instance Empty (NoReader a t)

instance SubjectReader (NoReader a) where
    type ReaderSubject (NoReader a) = a
    mSubjectToReadable _ = never
    subjectToRead _ = never

instance FullSubjectReader (NoReader ()) where
    readableToSubject _ = return ()

-- | Can't touch this.
newtype ConstEdit (reader :: Type -> Type) =
    MkConstEdit Void
    deriving newtype (Eq, Countable, Searchable)

instance forall reader. Finite (ConstEdit reader) where
    allValues = []

deriving newtype instance forall reader . Empty (ConstEdit reader)

instance forall reader. Show (ConstEdit reader) where
    show edit = never edit

instance forall reader. FloatingOn (ConstEdit reader) (ConstEdit reader)

type instance forall reader. EditReader (ConstEdit reader) = reader

instance forall reader. ApplicableEdit (ConstEdit reader) where
    applyEdit edit _ = never edit

instance forall reader. InvertibleEdit (ConstEdit reader) where
    invertEdit edit _ = never edit

instance forall reader. FullSubjectReader reader => SubjectMapEdit (ConstEdit reader)

instance forall reader. (FullSubjectReader reader, ReaderSubject reader ~ ()) => FullEdit (ConstEdit reader) where
    replaceEdit _ _ = return ()

instance forall reader. TestEquality reader => CacheableEdit (ConstEdit reader) where
    trimEdits _ = []

type ConstUpdate reader = EditUpdate (ConstEdit reader)

clPutEditsNone ::
       forall edita editb m m' rd. (Monad m', Empty editb)
    => [editb]
    -> Readable m rd
    -> m' (Maybe [edita])
clPutEditsNone [] _ = return $ Just []
clPutEditsNone (e:_) _ = never e
