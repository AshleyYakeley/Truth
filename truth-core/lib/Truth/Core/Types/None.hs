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

readFunctionNoUpdateFunction :: forall ra editb. ReadFunction ra (EditReader editb) -> UpdateFunction (NoEdit ra) editb
readFunctionNoUpdateFunction rf = let
    ufGet :: forall . ReadFunctionT IdentityT ra (EditReader editb)
    ufGet mra rb = lift $ rf mra rb
    ufUpdate edit _ = never edit
    in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

readFunctionNoEditLens :: forall ra editb. ReadFunction ra (EditReader editb) -> EditLens (NoEdit ra) editb
readFunctionNoEditLens rf = readOnlyEditLens $ readFunctionNoUpdateFunction rf

noEditLens :: forall edit. EditLens (NoEdit (EditReader edit)) edit
noEditLens = readFunctionNoEditLens $ \rt -> rt

ioFuncNoUpdateFunction ::
       (FullSubjectReader ra, SubjectReader (EditReader editb))
    => (ReaderSubject ra -> IO (EditSubject editb))
    -> UpdateFunction (NoEdit ra) editb
ioFuncNoUpdateFunction f = readFunctionNoUpdateFunction $ ioFuncReadFunction f

funcNoUpdateFunction ::
       (FullSubjectReader ra, SubjectReader (EditReader editb))
    => (ReaderSubject ra -> EditSubject editb)
    -> UpdateFunction (NoEdit ra) editb
funcNoUpdateFunction f = ioFuncNoUpdateFunction $ return . f

ioFuncNoEditLens ::
       (FullSubjectReader ra, SubjectReader (EditReader editb))
    => (ReaderSubject ra -> IO (EditSubject editb))
    -> EditLens (NoEdit ra) editb
ioFuncNoEditLens f = readOnlyEditLens $ ioFuncNoUpdateFunction f

funcNoEditLens ::
       (FullSubjectReader ra, SubjectReader (EditReader editb))
    => (ReaderSubject ra -> EditSubject editb)
    -> EditLens (NoEdit ra) editb
funcNoEditLens f = ioFuncNoEditLens $ return . f
