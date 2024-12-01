module Pinafore.Language.Value.Task where

import Import

-- Task
newtype LangTask a = MkLangTask
    { unLangTask :: Task Action a
    } deriving newtype (Functor, Applicative)

instance MaybeRepresentational LangTask where
    maybeRepresentational = Nothing

instance HasVariance LangTask where
    type VarianceOf LangTask = 'Covariance

liftTask :: Task IO a -> LangTask a
liftTask task = MkLangTask $ hoistTask liftIO task

langAsync :: Action a -> Action (LangTask a)
langAsync pa = do
    task <- forkTask pa
    actionOnClose $ do
        _ <- taskWait task
        return ()
    return $ MkLangTask task

langWait :: LangTask a -> Action a
langWait task = taskWait $ unLangTask task

langIsDone :: LangTask TopType -> Action Bool
langIsDone task = taskIsDone $ unLangTask task

langCheck :: LangTask a -> Action (Maybe a)
langCheck task = taskCheck $ unLangTask task

langTimeTask :: UTCTime -> LangTask ()
langTimeTask t = liftTask $ timeTask t

langDuration :: NominalDiffTime -> IO (LangTask ())
langDuration d = fmap liftTask $ durationTask d

langFirst :: [LangTask a] -> Action (LangTask a)
langFirst tasks = fmap MkLangTask $ firstTask $ fmap unLangTask tasks

-- StoppableTask
newtype LangStoppableTask a = MkLangStoppableTask
    { unLangStoppableTask :: StoppableTask Action a
    } deriving newtype (Functor, Applicative)

instance MaybeRepresentational LangStoppableTask where
    maybeRepresentational = Nothing

instance HasVariance LangStoppableTask where
    type VarianceOf LangStoppableTask = 'Covariance

langStoppableTaskTask :: LangStoppableTask a -> LangTask (Maybe a)
langStoppableTaskTask lstask = MkLangTask $ stoppableTaskTask $ unLangStoppableTask lstask

langStopTask :: LangStoppableTask TopType -> Action ()
langStopTask (MkLangStoppableTask task) = stoppableTaskStop task

langAsyncStoppable :: (Action BottomType -> Action a) -> Action (LangStoppableTask a)
langAsyncStoppable call = do
    stask <- forkStoppableTask $ \stop -> call stop
    actionOnClose $ do
        _ <- taskWait $ stoppableTaskTask stask
        return ()
    return $ MkLangStoppableTask stask

langForever :: Action (LangStoppableTask BottomType)
langForever = fmap MkLangStoppableTask $ liftIO foreverStoppableTask

langFollow :: LangTask a -> Action (LangStoppableTask a)
langFollow (MkLangTask task) = fmap MkLangStoppableTask $ followStoppableTask task

langFirstStoppable :: [LangStoppableTask a] -> Action (LangStoppableTask a)
langFirstStoppable stasks = fmap MkLangStoppableTask $ firstStoppableTask $ fmap unLangStoppableTask stasks

langRace :: [LangStoppableTask a] -> Action (LangStoppableTask a)
langRace stasks = fmap MkLangStoppableTask $ raceStoppableTasks $ fmap unLangStoppableTask stasks
