module Truth.Core.Edit.Opaque where

import Truth.Core.Edit.Update
import Truth.Core.Import

data OpaqueUpdate (edit :: Type) =
    MkOpaqueUpdate

instance IsUpdate (OpaqueUpdate edit) where
    type UpdateEdit (OpaqueUpdate edit) = edit
    editUpdate _edit = MkOpaqueUpdate
