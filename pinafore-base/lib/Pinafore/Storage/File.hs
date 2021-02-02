{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.File where

import Changes.Core
import Changes.World.FileSystem
import Changes.World.ReferenceStore
import Pinafore.Base
import Shapes

type PinaforeFileUpdate = ReferenceStoreUpdate FileEntity ByteStringEdit

pinaforeFileItemLens :: FileEntity -> ChangeLens PinaforeFileUpdate (SingleReferenceUpdate ByteStringEdit)
pinaforeFileItemLens entity = tupleChangeLens $ MkFunctionSelector entity

directoryPinaforeFileReference :: FilePath -> Reference (UpdateEdit PinaforeFileUpdate)
directoryPinaforeFileReference path =
    directoryReferenceStore
        (subdirectoryReference True path fileSystemReference)
        (\(MkFileEntity (MkEntity anchor)) -> show anchor)

nullFileModel :: Model PinaforeFileUpdate
nullFileModel = rejectingModel $ constFunctionReadFunction nullSingleReferenceReadable
