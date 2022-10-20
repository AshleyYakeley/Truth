{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Changes.Core
import Changes.World.FileSystem
import Changes.World.ReferenceStore
import Pinafore.Base
import Shapes

type QFileUpdate = ReferenceStoreUpdate FileEntity ByteStringEdit

instance BaseChangeLens QFileUpdate QFileUpdate where
    baseChangeLens = id

storageFileItemLens ::
       BaseChangeLens QFileUpdate baseupdate
    => FileEntity
    -> ChangeLens baseupdate (SingleReferenceUpdate ByteStringEdit)
storageFileItemLens entity = tupleChangeLens (MkFunctionSelector entity) . baseChangeLens

directoryStorageFileReference :: FilePath -> Reference (UpdateEdit QFileUpdate)
directoryStorageFileReference path =
    directoryReferenceStore
        (subdirectoryReference True path fileSystemReference)
        (\(MkFileEntity (MkEntity anchor)) -> show anchor)
