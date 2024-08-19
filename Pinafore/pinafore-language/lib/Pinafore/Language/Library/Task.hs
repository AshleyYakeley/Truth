module Pinafore.Language.Library.Task
    ( taskLibSection
    , LangTask(..)
    , liftTask
    , langWait
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

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

langAsync :: Action A -> Action (LangTask A)
langAsync pa = do
    task <- forkTask pa
    actionOnClose $ do
        _ <- taskWait task
        return ()
    return $ MkLangTask task

langWait :: LangTask A -> Action A
langWait task = taskWait $ unLangTask task

langIsDone :: LangTask TopType -> Action Bool
langIsDone task = taskIsDone $ unLangTask task

langCheck :: LangTask A -> Action (Maybe A)
langCheck task = taskCheck $ unLangTask task

langTimeTask :: UTCTime -> LangTask ()
langTimeTask t = liftTask $ timeTask t

langDuration :: NominalDiffTime -> IO (LangTask ())
langDuration d = fmap liftTask $ durationTask d

langFirst :: [LangTask A] -> Action (LangTask A)
langFirst tasks = fmap MkLangTask $ firstTask $ fmap unLangTask tasks

-- StoppableTask
newtype LangStoppableTask a = MkLangStoppableTask
    { unLangStoppableTask :: StoppableTask Action a
    } deriving (Functor, Applicative)

instance MaybeRepresentational LangStoppableTask where
    maybeRepresentational = Nothing

instance HasVariance LangStoppableTask where
    type VarianceOf LangStoppableTask = 'Covariance

stoppableTaskGroundType :: QGroundType '[ CoCCRVariance] LangStoppableTask
stoppableTaskGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangStoppableTask)|]) "StoppableTask"

instance HasQGroundType '[ CoCCRVariance] LangStoppableTask where
    qGroundType = stoppableTaskGroundType

langStoppableTaskTask :: LangStoppableTask A -> LangTask (Maybe A)
langStoppableTaskTask lstask = MkLangTask $ stoppableTaskTask $ unLangStoppableTask lstask

langStopTask :: LangStoppableTask TopType -> Action ()
langStopTask (MkLangStoppableTask task) = stoppableTaskStop task

langAsyncStoppable :: (Action BottomType -> Action A) -> Action (LangStoppableTask A)
langAsyncStoppable call = do
    stask <- forkStoppableTask $ \stop -> call stop
    actionOnClose $ do
        _ <- taskWait $ stoppableTaskTask stask
        return ()
    return $ MkLangStoppableTask stask

langForever :: Action (LangStoppableTask BottomType)
langForever = fmap MkLangStoppableTask $ liftIO foreverStoppableTask

langFollow :: LangTask A -> Action (LangStoppableTask A)
langFollow (MkLangTask task) = fmap MkLangStoppableTask $ followStoppableTask task

langFirstStoppable :: [LangStoppableTask A] -> Action (LangStoppableTask A)
langFirstStoppable stasks = fmap MkLangStoppableTask $ firstStoppableTask $ fmap unLangStoppableTask stasks

langRace :: [LangStoppableTask A] -> Action (LangStoppableTask A)
langRace stasks = fmap MkLangStoppableTask $ raceStoppableTasks $ fmap unLangStoppableTask stasks

taskLibSection :: LibraryStuff context
taskLibSection =
    headingBDS "Tasks" "" $
    [ headingBDS "Task" "" $
      [ typeBDS
            "Task"
            "A task is something that can be waited for to give a result."
            (MkSomeGroundType taskGroundType)
            []
      , namespaceBDS "Task" $
        applicativeEntries @_ @LangTask <>
        [ valBDS "async" "Run an action in another thread. It will complete in the current life cycle." langAsync
        , valBDS "wait" "Wait for a task to complete. This action is idempotent." langWait
        , valBDS "check" "Check to see if a task is done without waiting." langCheck
        , valBDS "isDone" "Check whether a task is done." langIsDone
        , valBDS "time" "A task that is done at this time." langTimeTask
        , valBDS "duration" "Create a task that will be done after this duration." langDuration
        , valBDS "first" "Create a task for the first task to finish." langFirst
        ]
      ]
    , headingBDS "StoppableTask" "" $
      [ typeBDS "StoppableTask" "A `Task` that can be stopped." (MkSomeGroundType stoppableTaskGroundType) []
      , hasSubtypeRelationBDS Verify "" $ functionToShim "langStoppableTaskTask" langStoppableTaskTask
      , namespaceBDS "StoppableTask" $
        applicativeEntries @_ @LangStoppableTask <>
        [ valBDS "stop" "Stop a stoppable task." langStopTask
        , valBDS
              "async"
              "Run an action in another thread, passing in a stop function. It will complete in the current life cycle."
              langAsyncStoppable
        , valBDS "forever" "Create a stoppable task that will run forever (until stopped)." langForever
        , valBDS
              "follow"
              "Create a stoppable task that follows a task. Stopping it will not stop the original task."
              langFollow
        , valBDS
              "first"
              "Create a stoppable task for the first stoppable task to finish. Does not stop the other tasks when done."
              langFirstStoppable
        , valBDS
              "race"
              "Create a stoppable task for the first stoppable task to finish. Also forks a task to stop them all when that happens."
              langRace
        ]
      ]
    ]
