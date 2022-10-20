module Pinafore.Language.Library.Task
    ( taskLibraryModule
    , LangTask(..)
    , liftTask
    , awaitTask
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

-- Task
newtype LangTask a = MkLangTask
    { unLangTask :: Task Action a
    } deriving (Functor, Applicative)

instance MaybeRepresentational LangTask where
    maybeRepresentational = Nothing

instance HasVariance LangTask where
    type VarianceOf LangTask = 'Covariance

taskGroundType :: QGroundType '[ CoCCRVariance] LangTask
taskGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangTask)|]) "Task"

instance HasQGroundType '[ CoCCRVariance] LangTask where
    qGroundType = taskGroundType

liftTask :: Task IO a -> LangTask a
liftTask task = MkLangTask $ hoistTask liftIO task

asyncTask :: Action a -> Action (LangTask a)
asyncTask pa = do
    task <- forkTask pa
    actionOnClose $ do
        _ <- taskWait task
        return ()
    return $ MkLangTask task

awaitTask :: forall a. LangTask a -> Action a
awaitTask task = taskWait $ unLangTask task

isDone :: forall a. LangTask a -> Action Bool
isDone task = taskIsDone $ unLangTask task

langCheckTask :: forall a. LangTask a -> Action (Maybe a)
langCheckTask task = checkTask $ unLangTask task

pairTask :: forall a b. LangTask a -> LangTask b -> LangTask (a, b)
pairTask = liftA2 (,)

langTimeTask :: UTCTime -> LangTask ()
langTimeTask t = liftTask $ timeTask t

langDurationTask :: NominalDiffTime -> IO (LangTask ())
langDurationTask d = fmap liftTask $ durationTask d

langRaceTasks :: forall a. [LangTask a] -> Action (LangTask a)
langRaceTasks tasks = fmap MkLangTask $ raceTasks $ fmap unLangTask tasks

taskLibraryModule :: LibraryModule
taskLibraryModule =
    MkDocTree
        "Task"
        ""
        [ mkTypeEntry "Task" "A task is something that can be waited for to give a result." $
          MkSomeGroundType taskGroundType
        , mkValEntry "mapTask" "" $ fmap @LangTask @A @B
        , mkValEntry "pureTask" "A task that's already completed with this value." $ pure @LangTask @A
        , mkValEntry "pairTask" "Combine two tasks." $ pairTask @A @B
        , mkValEntry "async" "Run an action in another thread. It will complete in the current life cycle." $
          asyncTask @A
        , mkValEntry "await" "Wait for a task to complete. This action is idempotent." $ awaitTask @A
        , mkValEntry "checkTask" "Check to see if a task is done without waiting." $ langCheckTask @A
        , mkValEntry "isDone" "Check whether a task is done." $ isDone @TopType
        , mkValEntry "timeTask" "A task that is done at this time." langTimeTask
        , mkValEntry "durationTask" "A task that will be done after this duration." langDurationTask
        , mkValEntry "race" "Whichever task is done first." $ langRaceTasks @A
        ]
