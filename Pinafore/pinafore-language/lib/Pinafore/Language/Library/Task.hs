{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Task
    ( taskLibSection
    , LangTask (..)
    , liftTask
    , langWait
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var

taskGroundType :: QGroundType '[CoCCRVariance] LangTask
taskGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangTask)|]) "Task"

instance HasQGroundType '[CoCCRVariance] LangTask where
    qGroundType = taskGroundType

stoppableTaskGroundType :: QGroundType '[CoCCRVariance] LangStoppableTask
stoppableTaskGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangStoppableTask)|]) "StoppableTask"

instance HasQGroundType '[CoCCRVariance] LangStoppableTask where
    qGroundType = stoppableTaskGroundType

taskLibSection :: LibraryStuff
taskLibSection =
    headingBDS "Tasks" ""
        $ [ headingBDS "Task" ""
                $ [ typeBDS
                        "Task"
                        "A task is something that can be waited for to give a result. Some tasks are _one-shot_, once they are done they remain done. Other tasks may become pending again after being done."
                        (MkSomeGroundType taskGroundType)
                        []
                  , namespaceBDS "Task"
                        $ applicativeEntries @LangTask
                        <> [ valBDS
                                "async"
                                "Run an action in another thread, as a one-shot task. It will complete in the current life cycle."
                                $ langAsync @A
                           , valBDS "wait" "Wait for a task to complete. For one-shot tasks, this action is idempotent." $ langWait @A
                           , valBDS "check" "Check to see if a task is done without waiting." $ langCheck @A
                           , valBDS "isDone" "Check whether a task is done." langIsDone
                           , valBDS "time" "A one-shot task that is done at this time." langTimeTask
                           , valBDS "duration" "Create a one-shot task that will be done after this duration." langDuration
                           , valBDS "first" "Create a one-shot task for the first task to finish." $ langFirst @A
                           ]
                  ]
          , headingBDS "StoppableTask" ""
                $ [ typeBDS "StoppableTask" "A `Task` that can be stopped." (MkSomeGroundType stoppableTaskGroundType) []
                  , hasSubtypeRelationBDS Verify "" $ functionToShim "langStoppableTaskTask" $ langStoppableTaskTask @A
                  , namespaceBDS "StoppableTask"
                        $ applicativeEntries @LangStoppableTask
                        <> [ valBDS "stop" "Stop a stoppable task." langStopTask
                           , valBDS
                                "async"
                                "Run an action in another thread, passing in a stop function. It will complete in the current life cycle."
                                $ langAsyncStoppable @A
                           , valBDS "forever" "Create a stoppable task that will run forever (until stopped)." langForever
                           , valBDS "follow" "Create a stoppable task that follows a task. Stopping it will not stop the original task."
                                $ langFollow @A
                           , valBDS
                                "first"
                                "Create a stoppable task for the first stoppable task to finish. Does not stop the other tasks when done."
                                $ langFirstStoppable @A
                           , valBDS
                                "race"
                                "Create a stoppable task for the first stoppable task to finish. Also forks a task to stop them all when that happens."
                                $ langRace @A
                           ]
                  ]
          ]
