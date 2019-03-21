{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Pinafore.Base
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ObjectStore

type PinaforeFileEdit = ObjectStoreEdit Entity ByteStringEdit

type HasPinaforeFileEdit = BaseEditLens PinaforeFileEdit

instance BaseEditLens PinaforeFileEdit PinaforeFileEdit where
    baseEditLens = id

pinaforeFileItemLens :: HasPinaforeFileEdit baseedit => Entity -> EditLens baseedit (SingleObjectEdit ByteStringEdit)
pinaforeFileItemLens entity = tupleEditLens (MkFunctionSelector entity) . baseEditLens

directoryPinaforeFileObject :: FilePath -> Object PinaforeFileEdit
directoryPinaforeFileObject path =
    directoryObjectStore (subdirectoryObject True path fileSystemObject) (\(MkEntity (MkAnchor uuid)) -> show uuid)
