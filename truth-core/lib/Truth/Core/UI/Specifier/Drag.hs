module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data DragSourceUISpec sel update where
    MkDragSourceUISpec
        :: Serialize t => String -> EditLens update (WholeUpdate t) -> UISpec sel update -> DragSourceUISpec sel update

instance Show (DragSourceUISpec sel update) where
    show (MkDragSourceUISpec typename _ spec) = "drag-source " ++ typename ++ " " ++ show spec

instance UIType DragSourceUISpec where
    uiWitness = $(iowitness [t|DragSourceUISpec|])

dragSourceUISpec :: Serialize t => String -> EditLens update (WholeUpdate t) -> UISpec sel update -> UISpec sel update
dragSourceUISpec datatype lens spec = MkUISpec $ MkDragSourceUISpec datatype lens spec

data DragDestinationUISpec sel update where
    MkDragDestinationUISpec
        :: Serialize t
        => String
        -> EditLens update (WholeUpdate t)
        -> UISpec sel update
        -> DragDestinationUISpec sel update

instance Show (DragDestinationUISpec sel update) where
    show (MkDragDestinationUISpec typename _ spec) = "drag-destination " ++ typename ++ " " ++ show spec

instance UIType DragDestinationUISpec where
    uiWitness = $(iowitness [t|DragDestinationUISpec|])

dragDestinationUISpec ::
       Serialize t => String -> EditLens update (WholeUpdate t) -> UISpec sel update -> UISpec sel update
dragDestinationUISpec datatype lens spec = MkUISpec $ MkDragDestinationUISpec datatype lens spec
