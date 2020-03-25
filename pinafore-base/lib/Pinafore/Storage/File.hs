{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Pinafore.Base
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ObjectStore

type PinaforeFileUpdate = ObjectStoreUpdate FileEntity ByteStringEdit

type HasPinaforeFileUpdate = BaseChangeLens PinaforeFileUpdate

instance BaseChangeLens PinaforeFileUpdate PinaforeFileUpdate where
    baseChangeLens = id

pinaforeFileItemLens ::
       HasPinaforeFileUpdate baseupdate => FileEntity -> ChangeLens baseupdate (SingleObjectUpdate ByteStringEdit)
pinaforeFileItemLens entity = tupleChangeLens (MkFunctionSelector entity) . baseChangeLens

directoryPinaforeFileObject :: FilePath -> Object (UpdateEdit PinaforeFileUpdate)
directoryPinaforeFileObject path =
    directoryObjectStore
        (subdirectoryObject True path fileSystemObject)
        (\(MkFileEntity (MkEntity (MkAnchor uuid))) -> show uuid)
