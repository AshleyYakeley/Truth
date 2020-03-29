{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Pinafore.Base
import Shapes
import Truth.Core
import Truth.World.FileSystem
import Truth.World.ReferenceStore

type PinaforeFileUpdate = ReferenceStoreUpdate FileEntity ByteStringEdit

type HasPinaforeFileUpdate = BaseChangeLens PinaforeFileUpdate

instance BaseChangeLens PinaforeFileUpdate PinaforeFileUpdate where
    baseChangeLens = id

pinaforeFileItemLens ::
       HasPinaforeFileUpdate baseupdate => FileEntity -> ChangeLens baseupdate (SingleReferenceUpdate ByteStringEdit)
pinaforeFileItemLens entity = tupleChangeLens (MkFunctionSelector entity) . baseChangeLens

directoryPinaforeFileReference :: FilePath -> Reference (UpdateEdit PinaforeFileUpdate)
directoryPinaforeFileReference path =
    directoryReferenceStore
        (subdirectoryReference True path fileSystemReference)
        (\(MkFileEntity (MkEntity anchor)) -> show anchor)
