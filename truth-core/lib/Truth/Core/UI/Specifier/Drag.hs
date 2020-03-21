module Truth.Core.UI.Specifier.Drag where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data DragSourceUISpec where
    MkDragSourceUISpec :: Serialize t => String -> Subscriber (WholeUpdate t) -> CVUISpec -> DragSourceUISpec

instance Show DragSourceUISpec where
    show (MkDragSourceUISpec typename _ _) = "drag-source " ++ typename

instance UIType DragSourceUISpec where
    uiWitness = $(iowitness [t|DragSourceUISpec|])

dragSourceUISpec :: Serialize t => String -> Subscriber (WholeUpdate t) -> CVUISpec -> CVUISpec
dragSourceUISpec datatype lens spec = mkCVUISpec $ MkDragSourceUISpec datatype lens spec

data DragDestinationUISpec where
    MkDragDestinationUISpec :: Serialize t => String -> Subscriber (WholeUpdate t) -> CVUISpec -> DragDestinationUISpec

instance Show DragDestinationUISpec where
    show (MkDragDestinationUISpec typename _ _) = "drag-destination " ++ typename

instance UIType DragDestinationUISpec where
    uiWitness = $(iowitness [t|DragDestinationUISpec|])

dragDestinationUISpec :: Serialize t => String -> Subscriber (WholeUpdate t) -> CVUISpec -> CVUISpec
dragDestinationUISpec datatype lens spec = mkCVUISpec $ MkDragDestinationUISpec datatype lens spec
