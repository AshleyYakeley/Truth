module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data DragSourceUISpec sel where
    MkDragSourceUISpec :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> UISpec sel -> DragSourceUISpec sel

instance Show (DragSourceUISpec sel) where
    show (MkDragSourceUISpec typename _ spec) = "drag-source " ++ typename ++ " " ++ show spec

instance UIType DragSourceUISpec where
    uiWitness = $(iowitness [t|DragSourceUISpec|])

dragSourceUISpec :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> UISpec sel -> UISpec sel
dragSourceUISpec datatype lens spec = MkUISpec $ MkDragSourceUISpec datatype lens spec

data DragDestinationUISpec sel where
    MkDragDestinationUISpec
        :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> UISpec sel -> DragDestinationUISpec sel

instance Show (DragDestinationUISpec sel) where
    show (MkDragDestinationUISpec typename _ spec) = "drag-destination " ++ typename ++ " " ++ show spec

instance UIType DragDestinationUISpec where
    uiWitness = $(iowitness [t|DragDestinationUISpec|])

dragDestinationUISpec :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> UISpec sel -> UISpec sel
dragDestinationUISpec datatype lens spec = MkUISpec $ MkDragDestinationUISpec datatype lens spec
