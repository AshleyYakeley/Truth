module Truth.Core.Edit.Edit where

import Truth.Core.Import
import Truth.Core.Read

class Floating edit (t :: *) where
    floatingUpdate :: edit -> t -> t
    floatingUpdate _ = id

instance Floating edit t => Floating [edit] t where
    floatingUpdate [] = id
    floatingUpdate (e:ee) = floatingUpdate ee . floatingUpdate e

class (Floating edit edit) =>
      Edit (edit :: *) where
    type EditReader edit :: * -> *
    applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit)

type EditSubject edit = ReaderSubject (EditReader edit)

applyEdits :: (Edit edit) => [edit] -> ReadFunction (EditReader edit) (EditReader edit)
applyEdits [] mr = mr
applyEdits (e:es) mr = applyEdits es $ applyEdit e mr

class Edit edit =>
      InvertibleEdit (edit :: *) where
    invertEdit ::
           forall m. MonadIO m
        => edit
        -> MutableRead m (EditReader edit)
        -> m [edit]
    -- edits always applied in the given order, so list returned will be reversed relative to list given.

invertEdits :: (MonadIO m, InvertibleEdit edit) => [edit] -> MutableRead m (EditReader edit) -> m [edit]
invertEdits [] _mr = return []
invertEdits (e:ee) mr = do
    u <- invertEdit e mr
    uu <- invertEdits ee (applyEdit e mr)
    return $ u ++ uu