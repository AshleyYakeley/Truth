module Pinafore.Test
    ( parseType
    , runScoped
    , runSourceScoped
    , runSourcePos
    , PinaforeTypeSystem
    , Name
    , UVar
    , PinaforeType
    , PinaforeScoped
    , PinaforeSourceScoped
    , pinaforeSimplifyTypes
    , toTypeF
    , module Pinafore.Test
    ) where

import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Read
import Pinafore.Language.Type
import Pinafore.Language.Type.Simplify
import Pinafore.Main
import Pinafore.Pinafore
import Pinafore.Storage
import Shapes
import Truth.Core
import Truth.World.ObjectStore

makeTestPinaforeContext :: IO (PinaforeContext, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <- freeIOObject ([], []) $ \_ -> True
    let
        pinaforeObject :: Object PinaforeEdit
        pinaforeObject =
            tupleObject $ \case
                PinaforeSelectPoint -> pinaforeTableEntityObject $ convertObject tableStateObject
                PinaforeSelectFile -> readConstantObject $ constFunctionReadFunction nullSingleObjectMutableRead
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getObjectSubject tableStateObject
    pc <- makePinaforeContext pinaforeObject $ \_ -> return ()
    return (pc, getTableState)
