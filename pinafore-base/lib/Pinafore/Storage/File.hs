{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Changes.Core
import Changes.World.FileSystem
import Changes.World.ReferenceStore
import Pinafore.Base
import Shapes

type PinaforeFileUpdate = ReferenceStoreUpdate FileEntity ByteStringEdit

instance BaseChangeLens PinaforeFileUpdate PinaforeFileUpdate where
    baseChangeLens = id

pinaforeFileItemLens ::
       BaseChangeLens PinaforeFileUpdate baseupdate
    => FileEntity
    -> ChangeLens baseupdate (SingleReferenceUpdate ByteStringEdit)
pinaforeFileItemLens entity = tupleChangeLens (MkFunctionSelector entity) . baseChangeLens

directoryPinaforeFileReference :: FilePath -> Reference (UpdateEdit PinaforeFileUpdate)
directoryPinaforeFileReference path =
    directoryReferenceStore
        (subdirectoryReference True path fileSystemReference)
        (\(MkFileEntity (MkEntity anchor)) -> show anchor)
