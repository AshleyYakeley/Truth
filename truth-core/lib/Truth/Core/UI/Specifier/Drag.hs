module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIDragSource seledit edit where
    MkUIDragSource
        :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec seledit edit -> UIDragSource seledit edit

instance Show (UIDragSource seledit edit) where
    show (MkUIDragSource typename _ spec) = "drag-source " ++ typename ++ " " ++ show spec

instance UIType UIDragSource where
    uiWitness = $(iowitness [t|UIDragSource|])

uiDragSource :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec seledit edit -> UISpec seledit edit
uiDragSource datatype lens spec = MkUISpec $ MkUIDragSource datatype lens spec

data UIDragDestination seledit edit where
    MkUIDragDestination
        :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec seledit edit -> UIDragDestination seledit edit

instance Show (UIDragDestination seledit edit) where
    show (MkUIDragDestination typename _ spec) = "drag-destination " ++ typename ++ " " ++ show spec

instance UIType UIDragDestination where
    uiWitness = $(iowitness [t|UIDragDestination|])

uiDragDestination :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec seledit edit -> UISpec seledit edit
uiDragDestination datatype lens spec = MkUISpec $ MkUIDragDestination datatype lens spec
