{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where
{-
import Pinafore.Base
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ObjectStore

type PinaforeFileUpdate = ObjectStoreUpdate FileEntity ByteStringEdit

pinaforeFileItemLens ::
       FileEntity -> EditLens baseupdate (SingleObjectUpdate ByteStringEdit)
pinaforeFileItemLens entity = tupleEditLens (MkFunctionSelector entity) . baseEditLens

directoryPinaforeFileObject :: FilePath -> Object (UpdateEdit PinaforeFileUpdate)
directoryPinaforeFileObject path =
    directoryObjectStore
        (subdirectoryObject True path fileSystemObject)
        (\(MkFileEntity (MkEntity (MkAnchor uuid))) -> show uuid)
-}
