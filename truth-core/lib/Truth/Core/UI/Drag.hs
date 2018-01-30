module Truth.Core.UI.Drag where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UIDragSource edit where
    MkUIDragSource :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec edit -> UIDragSource edit

instance Show (UIDragSource edit) where
    show (MkUIDragSource typename _ spec) = "drag-source " ++ typename ++ " " ++ show spec

instance UIType UIDragSource where
    uiWitness = $(iowitness [t|UIDragSource|])

uiDragSource :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec edit -> UISpec edit
uiDragSource datatype lens spec = MkUISpec $ MkUIDragSource datatype lens spec

data UIDragDestination edit where
    MkUIDragDestination
        :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec edit -> UIDragDestination edit

instance Show (UIDragDestination edit) where
    show (MkUIDragDestination typename _ spec) = "drag-destination " ++ typename ++ " " ++ show spec

instance UIType UIDragDestination where
    uiWitness = $(iowitness [t|UIDragDestination|])

uiDragDestination :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec edit -> UISpec edit
uiDragDestination datatype lens spec = MkUISpec $ MkUIDragDestination datatype lens spec
