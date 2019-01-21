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

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Read
import Pinafore.Language.Type
import Pinafore.Language.Type.Simplify
import Pinafore.Pinafore
import Pinafore.Storage
import Shapes
import Truth.Core
import Truth.World.ObjectStore

makeTestPinaforeContext :: LifeCycle (PinaforeContext PinaforeEdit, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <-
        liftIO $ freeIOObject ([], []) $ \_ -> True
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

withTestPinaforeContext ::
       ((?pinafore :: PinaforeContext PinaforeEdit) => IO (EditSubject PinaforeTableEdit) -> IO r) -> IO r
withTestPinaforeContext f =
    withLifeCycle makeTestPinaforeContext $ \(pc, getTableState) -> let
        ?pinafore = pc
        in f getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext baseedit) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f
