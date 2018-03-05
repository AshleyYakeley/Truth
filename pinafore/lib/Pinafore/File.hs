module Pinafore.File where

import Pinafore.Table
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ObjectStore

type PinaforeFileEdit = ObjectStoreEdit Point ByteStringEdit

class HasPinaforeFileEdit baseedit where
    pinaforeFileLens :: EditLens baseedit PinaforeFileEdit

instance HasPinaforeFileEdit PinaforeFileEdit where
    pinaforeFileLens = id

directoryPinaforeFileObject :: FilePath -> Object PinaforeFileEdit
directoryPinaforeFileObject path = directoryObjectStore (subdirectoryObject True path fileSystemObject) show
