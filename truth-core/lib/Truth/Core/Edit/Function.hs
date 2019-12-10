module Truth.Core.Edit.Function where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read
import Truth.Debug

data UpdateFunction updateA updateB = MkUpdateFunction
    { ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    , ufUpdate :: forall m. MonadIO m => updateA -> MutableRead m (UpdateReader updateA) -> m [updateB]
    -- ^ the MutableRead argument will reflect the new value
    }

instance Category UpdateFunction where
    id :: forall update. UpdateFunction update update
    id = let
        ufGet :: ReadFunction (UpdateReader update) (UpdateReader update)
        ufGet mr = mr
        ufUpdate update _ = return [update]
        in MkUpdateFunction {..}
    (.) :: forall updateA updateB updateC.
           UpdateFunction updateB updateC
        -> UpdateFunction updateA updateB
        -> UpdateFunction updateA updateC
    MkUpdateFunction gBC uBC . MkUpdateFunction gAB uAB = let
        gAC :: forall m. MonadIO m
            => MutableRead m (UpdateReader updateA)
            -> MutableRead m (UpdateReader updateC)
        gAC mra = gBC $ gAB mra
        uAC :: forall m. MonadIO m
            => updateA
            -> MutableRead m (UpdateReader updateA)
            -> m [updateC]
        uAC updA mrA = traceBracket "AnEditFunction.ucCompose.efUpdate" $ do
            updBs <- uAB updA mrA
            updCss <- for updBs $ \updB -> uBC updB $ \rt -> id $ gAB mrA rt
            return $ mconcat updCss
        in MkUpdateFunction gAC uAC

ufUpdates ::
       MonadIO m => UpdateFunction updateA updateB -> [updateA] -> MutableRead m (UpdateReader updateA) -> m [updateB]
ufUpdates _ [] _ = return []
ufUpdates sef (ea:eas) mr = do
    eb <- ufUpdate sef ea mr
    ebs <- ufUpdates sef eas mr
    return $ eb ++ ebs

ioFuncUpdateFunction ::
       forall updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => (UpdateSubject updateA -> IO (UpdateSubject updateB))
    -> UpdateFunction updateA updateB
ioFuncUpdateFunction amb = let
    ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    ufGet mra rt = (mSubjectToMutableRead $ mutableReadToSubject mra >>= \a -> liftIO (amb a)) rt
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [updateB]
    ufUpdate updateA mra =
        fmap (fmap editUpdate) $
        getReplaceEdits $
        mSubjectToMutableRead $ do
            a <- mutableReadToSubject $ applyEdit (updateEdit updateA) mra
            liftIO $ amb a
    in MkUpdateFunction {..}

funcUpdateFunction ::
       forall updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => (UpdateSubject updateA -> UpdateSubject updateB)
    -> UpdateFunction updateA updateB
funcUpdateFunction ab = ioFuncUpdateFunction $ \a -> return $ ab a

immutableUpdateFunction ::
       (forall m. MonadIO m => MutableRead m (UpdateReader updateB)) -> UpdateFunction updateA updateB
immutableUpdateFunction mr = MkUpdateFunction {ufGet = \_ -> mr, ufUpdate = \_ _ -> return []}

ioConstUpdateFunction ::
       SubjectReader (UpdateReader updateB) => IO (UpdateSubject updateB) -> UpdateFunction updateA updateB
ioConstUpdateFunction iob = immutableUpdateFunction $ mSubjectToMutableRead $ liftIO iob

constUpdateFunction :: SubjectReader (UpdateReader updateB) => UpdateSubject updateB -> UpdateFunction updateA updateB
constUpdateFunction b = ioConstUpdateFunction $ return b
