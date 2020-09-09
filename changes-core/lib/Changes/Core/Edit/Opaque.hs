module Changes.Core.Edit.Opaque where

import Changes.Core.Edit.Update
import Changes.Core.Import

data OpaqueUpdate (edit :: Type) =
    MkOpaqueUpdate

type instance UpdateEdit (OpaqueUpdate edit) = edit

instance IsUpdate (OpaqueUpdate edit) where
    editUpdate _edit = MkOpaqueUpdate
