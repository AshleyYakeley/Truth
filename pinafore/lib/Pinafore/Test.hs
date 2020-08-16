module Pinafore.Test
    ( parseType
    , runScoped
    , runSourcePos
    , PinaforeTypeSystem
    , Name
    , UVar
    , Var(..)
    , PinaforeGroundType(..)
    , EntityGroundType(..)
    , PinaforeType
    , PinaforePolyShim
    , PinaforeShimWit
    , PinaforeSingularType
    , PinaforeSingularShimWit
    , PinaforeScoped
    , PinaforeSourceScoped
    , toJMShimWit
    , PinaforeTableSubject(..)
    , module Pinafore.Test
    ) where

import Pinafore.Base
import Pinafore.Language
import Pinafore.Language.Name
import Pinafore.Language.Read
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Storage
import Shapes
import Truth.Core

makeTestPinaforeContext :: UIToolkit -> LifeCycleIO (PinaforeContext, IO PinaforeTableSubject)
makeTestPinaforeContext uitoolkit = do
    let rc = emptyResourceContext
    tableStateReference :: Reference (WholeEdit PinaforeTableSubject) <-
        liftIO $ makeMemoryReference (MkPinaforeTableSubject [] [] [] []) $ \_ -> True
    let
        tableReference :: Reference PinaforeTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: IO PinaforeTableSubject
        getTableState = getReferenceSubject rc tableStateReference
    (model, ()) <- makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference tableReference
    pc <- makePinaforeContext model uitoolkit
    return (pc, getTableState)

withTestPinaforeContext ::
       ((?pinafore :: PinaforeContext) => UIToolkit -> MFunction LifeCycleIO IO -> IO PinaforeTableSubject -> IO r)
    -> IO r
withTestPinaforeContext call =
    runLifeCycle $
    liftWithUnlift $ \unlift -> do
        let uitoolkit = nullUIToolkit unlift
        (pc, getTableState) <- unlift $ makeTestPinaforeContext uitoolkit
        let
            ?pinafore = pc
            in call uitoolkit unlift getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

runTestPinaforeSourceScoped :: PinaforeSourceScoped a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeSourceScoped "<input>" sa

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> IO ()
    -> Editor (WholeUpdate a) ()
checkUpdateEditor val push = let
    editorInit :: Reference (WholeEdit a) -> LifeCycleIO (MVar (NonEmpty (WholeUpdate a)))
    editorInit _ = liftIO newEmptyMVar
    editorUpdate ::
           MVar (NonEmpty (WholeUpdate a))
        -> Reference (WholeEdit a)
        -> ResourceContext
        -> NonEmpty (WholeUpdate a)
        -> EditContext
        -> IO ()
    editorUpdate var _ _ edits _ = putMVar var edits
    editorDo :: MVar (NonEmpty (WholeUpdate a)) -> Reference (WholeEdit a) -> Task () -> LifeCycleIO ()
    editorDo var _ _ =
        liftIO $ do
            push
            edits <- takeMVar var
            case edits of
                MkWholeReaderUpdate v :| []
                    | v == val -> return ()
                _ -> fail "unexpected push"
    editorTask = mempty
    in MkEditor {..}
