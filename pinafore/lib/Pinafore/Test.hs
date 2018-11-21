module Pinafore.Test
    ( parseType
    , runPinaforeTypeCheck
    , PinaforeTypeSystem
    , Name
    , UVar
    , PinaforeType
    , PinaforeTypeCheck
    , toTypeF
    , module Pinafore.Test
    ) where

import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Read
import Pinafore.Language.Type
import Pinafore.Main
import Pinafore.Pinafore
import Pinafore.Storage
import Shapes
import Truth.Core
import Truth.World.ObjectStore

makeTestObject :: IO (Object PinaforeEdit)
makeTestObject = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <- freeIOObject ([], []) $ \_ -> True
    return $
        tupleObject $ \case
            PinaforeSelectPoint -> pinaforeTablePointObject $ convertObject tableStateObject
            PinaforeSelectFile -> readConstantObject $ constFunctionReadFunction nullSingleObjectMutableRead

makeTestPinaforeContext :: IO PinaforeContext
makeTestPinaforeContext = do
    pinaforeObject <- makeTestObject
    makePinaforeContext pinaforeObject $ \_ -> return ()
