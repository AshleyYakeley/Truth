module Pinafore.Storage.File where

import Pinafore.Storage.Table
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ObjectStore

type PinaforeFileEdit = ObjectStoreEdit Point ByteStringEdit

class HasPinaforeFileEdit baseedit where
    pinaforeFileLens :: EditLens baseedit PinaforeFileEdit

instance HasPinaforeFileEdit PinaforeFileEdit where
    pinaforeFileLens = id

pinaforeFileItemLens :: HasPinaforeFileEdit baseedit => Point -> EditLens baseedit (SingleObjectEdit ByteStringEdit)
pinaforeFileItemLens point = tupleEditLens (MkFunctionSelector point) . pinaforeFileLens

directoryPinaforeFileObject :: FilePath -> Object PinaforeFileEdit
directoryPinaforeFileObject path =
    directoryObjectStore (subdirectoryObject True path fileSystemObject) (\(MkPoint (MkAnchor uuid)) -> show uuid)
