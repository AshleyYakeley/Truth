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
applyEdits [] = readable
applyEdits (e:es) = composeReadFunction (applyEdits es) (applyEdit e)

class Edit edit =>
      InvertableEdit (edit :: *) where
    invertEdit :: edit -> Readable (EditReader edit) [edit]
    -- edits always applied in the given order, so list returned will be reversed relative to list given.

invertEdits :: InvertableEdit edit => [edit] -> Readable (EditReader edit) [edit]
invertEdits [] = return []
invertEdits (e:ee) = do
    uu <- mapReadable (applyEdit e) $ invertEdits ee
    u <- invertEdit e
    return $ u ++ uu
