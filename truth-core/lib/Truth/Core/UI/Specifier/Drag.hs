module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIDragSource sel edit where
    MkUIDragSource :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> UIDragSource sel edit

instance Show (UIDragSource sel edit) where
    show (MkUIDragSource typename _ spec) = "drag-source " ++ typename ++ " " ++ show spec

instance UIType UIDragSource where
    uiWitness = $(iowitness [t|UIDragSource|])

uiDragSource :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> UISpec sel edit
uiDragSource datatype lens spec = MkUISpec $ MkUIDragSource datatype lens spec

data UIDragDestination sel edit where
    MkUIDragDestination
        :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> UIDragDestination sel edit

instance Show (UIDragDestination sel edit) where
    show (MkUIDragDestination typename _ spec) = "drag-destination " ++ typename ++ " " ++ show spec

instance UIType UIDragDestination where
    uiWitness = $(iowitness [t|UIDragDestination|])

uiDragDestination :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> UISpec sel edit
uiDragDestination datatype lens spec = MkUISpec $ MkUIDragDestination datatype lens spec
