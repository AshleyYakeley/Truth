{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Pinafore.Base
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ObjectStore

type PinaforeFileEdit = ObjectStoreEdit FileEntity ByteStringEdit

type HasPinaforeFileEdit = BaseEditLens PinaforeFileEdit

instance BaseEditLens PinaforeFileEdit PinaforeFileEdit where
    baseEditLens = id

pinaforeFileItemLens ::
       HasPinaforeFileEdit baseedit => FileEntity -> EditLens baseedit (SingleObjectEdit ByteStringEdit)
pinaforeFileItemLens entity = tupleEditLens (MkFunctionSelector entity) . baseEditLens

directoryPinaforeFileObject :: FilePath -> Object PinaforeFileEdit
directoryPinaforeFileObject path =
    directoryObjectStore
        (subdirectoryObject True path fileSystemObject)
        (\(MkFileEntity (MkEntity (MkAnchor uuid))) -> show uuid)
