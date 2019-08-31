module Truth.Core.Edit.Edit where

import Truth.Core.Import
import Truth.Core.Read

class Floating edit (t :: Type) where
    floatingUpdate :: edit -> t -> t
    floatingUpdate _ = id

instance Floating edit t => Floating [edit] t where
    floatingUpdate [] = id
    floatingUpdate (e:ee) = floatingUpdate ee . floatingUpdate e

type family EditReader (edit :: Type) :: Type -> Type

class IsUpdate (update :: Type) where
    type UpdateEdit update :: Type
    editUpdate :: UpdateEdit update -> update

type UpdateReader update = EditReader (UpdateEdit update)

class IsUpdate update => IsEditUpdate update where
    updateEdit :: update -> UpdateEdit update

newtype EditUpdate edit =
    MkEditUpdate edit
    deriving (Eq, Countable, Searchable)

instance Finite edit => Finite (EditUpdate edit) where
    allValues = fmap MkEditUpdate allValues

deriving instance Empty edit => Empty (EditUpdate edit)

instance Show edit => Show (EditUpdate edit) where
    show (MkEditUpdate edit) = show edit

instance IsUpdate (EditUpdate edit) where
    type UpdateEdit (EditUpdate edit) = edit
    editUpdate = MkEditUpdate

instance IsEditUpdate (EditUpdate edit) where
    updateEdit (MkEditUpdate edit) = edit

class Floating edit edit => ApplicableEdit (edit :: Type) where
    applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit)

type EditSubject edit = ReaderSubject (EditReader edit)

type UpdateSubject update = ReaderSubject (UpdateReader update)

applyEdits :: ApplicableEdit edit => [edit] -> ReadFunction (EditReader edit) (EditReader edit)
applyEdits [] mr = mr
applyEdits (e:es) mr = applyEdits es $ applyEdit e mr

class SubjectReader (EditReader edit) => SubjectMapEdit (edit :: Type) where
    mapSubjectEdits ::
           forall m. MonadIO m
        => [edit]
        -> EditSubject edit
        -> m (EditSubject edit)
    default mapSubjectEdits ::
        forall m.
            (MonadIO m, ApplicableEdit edit, FullSubjectReader (EditReader edit)) =>
                    [edit] -> EditSubject edit -> m (EditSubject edit)
    mapSubjectEdits edits subj = mutableReadToSubject $ applyEdits edits $ subjectToMutableRead subj

mapEditToMapEdits ::
       Monad m
    => (edit -> EditSubject edit -> m (EditSubject edit))
    -> [edit]
    -> EditSubject edit
    -> m (EditSubject edit)
mapEditToMapEdits _ [] subj = return subj
mapEditToMapEdits f (e:ee) oldsubj = do
    newsubj <- f e oldsubj
    mapEditToMapEdits f ee newsubj

class InvertibleEdit (edit :: Type) where
    invertEdit ::
           forall m. MonadIO m
        => edit
        -> MutableRead m (EditReader edit)
        -> m [edit]
    invertEdit edit = invertEdits [edit]
    invertEdits ::
           forall m. MonadIO m
        => [edit]
        -> MutableRead m (EditReader edit)
        -> m [edit]
    default invertEdits ::
        (MonadIO m, ApplicableEdit edit, InvertibleEdit edit) => [edit] -> MutableRead m (EditReader edit) -> m [edit]
    invertEdits [] _mr = return []
    invertEdits (e:ee) mr = do
        u <- invertEdit e mr
        uu <- invertEdits ee (applyEdit e mr)
        return $ u ++ uu
    -- edits always applied in the given order, so list returned will be reversed relative to list given.
    {-# MINIMAL invertEdit | invertEdits #-}
