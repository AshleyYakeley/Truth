{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Changes.Core
import Changes.World.FileSystem
import Changes.World.ReferenceStore
import Pinafore.Base
import Shapes

type QFileUpdate = ReferenceStoreUpdate FileEntity ByteStringEdit

storageFileItemLens :: FileEntity -> ChangeLens QFileUpdate (SingleReferenceUpdate ByteStringEdit)
storageFileItemLens entity = tupleChangeLens (MkFunctionSelector entity)

directoryStorageFileReference :: FilePath -> Reference (UpdateEdit QFileUpdate)
directoryStorageFileReference path =
    directoryReferenceStore
        (subdirectoryReference True path fileSystemReference)
        (\(MkFileEntity (MkEntity anchor)) -> show anchor)
