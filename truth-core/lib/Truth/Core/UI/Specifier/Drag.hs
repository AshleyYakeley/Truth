module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data DragSourceUISpec sel edit where
    MkDragSourceUISpec
        :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> DragSourceUISpec sel edit

instance Show (DragSourceUISpec sel edit) where
    show (MkDragSourceUISpec typename _ spec) = "drag-source " ++ typename ++ " " ++ show spec

instance UIType DragSourceUISpec where
    uiWitness = $(iowitness [t|DragSourceUISpec|])

dragSourceUISpec :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> UISpec sel edit
dragSourceUISpec datatype lens spec = MkUISpec $ MkDragSourceUISpec datatype lens spec

data DragDestinationUISpec sel edit where
    MkDragDestinationUISpec
        :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> DragDestinationUISpec sel edit

instance Show (DragDestinationUISpec sel edit) where
    show (MkDragDestinationUISpec typename _ spec) = "drag-destination " ++ typename ++ " " ++ show spec

instance UIType DragDestinationUISpec where
    uiWitness = $(iowitness [t|DragDestinationUISpec|])

dragDestinationUISpec :: Serialize t => String -> EditLens edit (WholeEdit t) -> UISpec sel edit -> UISpec sel edit
dragDestinationUISpec datatype lens spec = MkUISpec $ MkDragDestinationUISpec datatype lens spec
