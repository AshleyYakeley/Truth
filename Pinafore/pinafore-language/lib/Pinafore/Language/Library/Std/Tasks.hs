module Pinafore.Language.Library.Std.Tasks
    ( tasksLibEntries
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

-- Task
newtype LangTask a =
    MkLangTask (Compose Task Know a)
    deriving (Functor, Applicative)

instance RepresentationalRole LangTask where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangTask where
    maybeRepresentational = Just Dict

instance HasVariance LangTask where
    type VarianceOf LangTask = 'Covariance

taskGroundType :: PinaforeGroundType '[ CoCCRVariance] LangTask
taskGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangTask)|]) "Task"

instance HasPinaforeGroundType '[ CoCCRVariance] LangTask where
    pinaforeGroundType = taskGroundType

async :: PinaforeAction a -> PinaforeAction (LangTask a)
async pa =
    actionLiftViewKnowWithUnlift $ \unlift -> do
        task <- liftIOWithUnlift $ \unliftIO -> forkSingleTask $ unliftIO $ unlift pa
        viewOnCloseIO $ do
            _ <- taskWait task
            return ()
        return $ Known $ MkLangTask $ Compose task

await :: LangTask a -> PinaforeAction a
await (MkLangTask (Compose task)) = actionLiftViewKnow $ liftIO $ taskWait task

isDone :: LangTask a -> IO Bool
isDone (MkLangTask (Compose task)) = taskIsDone task

pairTask :: forall a b. LangTask a -> LangTask b -> LangTask (a, b)
pairTask = liftA2 (,)

tasksLibEntries :: [DocTreeEntry BindDoc]
tasksLibEntries =
    [ docTreeEntry
          "Tasks"
          ""
          [ mkTypeEntry "Task" "A task is an action running in another thread." $ MkSomeGroundType taskGroundType
          , mkValEntry "mapTask" "" $ fmap @LangTask @A @B
          , mkValEntry "pureTask" "A task that's already completed with this value." $ pure @LangTask @A
          , mkValEntry "pairTask" "Combine two tasks." $ pairTask @A @B
          , mkValEntry
                "async"
                "Run an action as a task in another thread. It will complete in the current life cycle (ignoring exceptions)." $
            async @A
          , mkValEntry "await" "Wait for a task to complete. This action is idempotent." $ await @A
          , mkValEntry "isDone" "Check whether a task is done." $ isDone @TopType
          ]
    ]
