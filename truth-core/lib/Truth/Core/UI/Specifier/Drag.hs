module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data DragSourceUISpec sel where
    MkDragSourceUISpec :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> LUISpec sel -> DragSourceUISpec sel

instance Show (DragSourceUISpec sel) where
    show (MkDragSourceUISpec typename _ _) = "drag-source " ++ typename

instance UIType DragSourceUISpec where
    uiWitness = $(iowitness [t|DragSourceUISpec|])

dragSourceUISpec :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> LUISpec sel -> LUISpec sel
dragSourceUISpec datatype lens spec = mkLUISpec $ MkDragSourceUISpec datatype lens spec

data DragDestinationUISpec sel where
    MkDragDestinationUISpec
        :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> LUISpec sel -> DragDestinationUISpec sel

instance Show (DragDestinationUISpec sel) where
    show (MkDragDestinationUISpec typename _ _) = "drag-destination " ++ typename

instance UIType DragDestinationUISpec where
    uiWitness = $(iowitness [t|DragDestinationUISpec|])

dragDestinationUISpec :: Serialize t => String -> OpenSubscriber (WholeUpdate t) -> LUISpec sel -> LUISpec sel
dragDestinationUISpec datatype lens spec = mkLUISpec $ MkDragDestinationUISpec datatype lens spec
