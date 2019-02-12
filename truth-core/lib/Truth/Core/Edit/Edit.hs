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

class (Floating edit edit) => ApplicableEdit (edit :: Type) where
    applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit)

type EditSubject edit = ReaderSubject (EditReader edit)

applyEdits :: (ApplicableEdit edit) => [edit] -> ReadFunction (EditReader edit) (EditReader edit)
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
