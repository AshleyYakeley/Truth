module Pinafore.Language.Library.Task
    ( taskLibSection
    , LangTask(..)
    , liftTask
    , awaitTask
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
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

langTimeTask :: UTCTime -> LangTask ()
langTimeTask t = liftTask $ timeTask t

langDurationTask :: NominalDiffTime -> IO (LangTask ())
langDurationTask d = fmap liftTask $ durationTask d

langRaceTasks :: forall a. [LangTask a] -> Action (LangTask a)
langRaceTasks tasks = fmap MkLangTask $ raceTasks $ fmap unLangTask tasks

taskLibSection :: BindDocStuff context
taskLibSection =
    headingBDS "Task" "" $
    [ typeBDS "Task" "A task is something that can be waited for to give a result." (MkSomeGroundType taskGroundType) []
    , namespaceBDS "Task" $
      applicativeEntries @_ @LangTask <>
      [ valBDS "async" "Run an action in another thread. It will complete in the current life cycle." $ asyncTask @A
      , valBDS "await" "Wait for a task to complete. This action is idempotent." $ awaitTask @A
      , valBDS "check" "Check to see if a task is done without waiting." $ langCheckTask @A
      , valBDS "isDone" "Check whether a task is done." $ isDone @TopType
      , valBDS "time" "A task that is done at this time." langTimeTask
      , valBDS "duration" "A task that will be done after this duration." langDurationTask
      , valBDS "race" "Whichever task is done first." $ langRaceTasks @A
      ]
    ]
