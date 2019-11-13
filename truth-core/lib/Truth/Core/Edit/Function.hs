module Truth.Core.Edit.Function where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Resource

data AnUpdateFunction (tt :: [TransKind]) updateA updateB = MkAnUpdateFunction
    { ufGet :: ReadFunctionTT tt (UpdateReader updateA) (UpdateReader updateB)
    , ufUpdate :: forall m. MonadIO m => updateA -> MutableRead m (UpdateReader updateA) -> ApplyStack tt m [updateB]
    -- ^ the MutableRead argument will reflect the new value
    }

type UpdateFunction = Runnable2 AnUpdateFunction

instance RunnableMap AnUpdateFunction where
    mapRunnable t1t2 =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkAnUpdateFunction g u) ->
            MkAnUpdateFunction
                (\(mr :: MutableRead m _) rt -> tlfFunction t1t2 (Proxy @m) $ g mr rt)
                (\ea (mr :: MutableRead m _) -> tlfFunction t1t2 (Proxy @m) $ u ea mr)

instance RunnableCategory AnUpdateFunction where
    ucId :: forall update. AnUpdateFunction '[] update update
    ucId = let
        ufGet :: ReadFunctionTT '[] (UpdateReader update) (UpdateReader update)
        ufGet mr = mr
        ufUpdate update _ = return [update]
        in MkAnUpdateFunction {..}
    ucCompose ::
           forall ttab ttbc updateA updateB updateC. (MonadTransStackUnliftAll ttab, MonadTransStackUnliftAll ttbc)
        => AnUpdateFunction ttbc updateB updateC
        -> AnUpdateFunction ttab updateA updateB
        -> AnUpdateFunction (Concat ttbc ttab) updateA updateC
    ucCompose (MkAnUpdateFunction gBC uBC) (MkAnUpdateFunction gAB uAB) = let
        gAC :: forall m. MonadIO m
            => MutableRead m (UpdateReader updateA)
            -> MutableRead (ApplyStack (Concat ttbc ttab) m) (UpdateReader updateC)
        gAC mra =
            case transStackDict @MonadIO @ttab @m of
                Dict ->
                    case transStackConcatRefl @ttbc @ttab @m of
                        Refl -> gBC $ gAB mra
        uAC :: forall m. MonadIO m
            => updateA
            -> MutableRead m (UpdateReader updateA)
            -> ApplyStack (Concat ttbc ttab) m [updateC]
        uAC updA mrA =
            case transStackDict @MonadIO @ttab @m of
                Dict ->
                    case transStackDict @MonadIO @ttbc @(ApplyStack ttab m) of
                        Dict ->
                            case transStackConcatRefl @ttbc @ttab @m of
                                Refl -> do
                                    updBs <- stackLift @ttbc @(ApplyStack ttab m) $ uAB updA mrA
                                    updCss <- for updBs $ \updB -> uBC updB $ \rt -> id $ gAB mrA rt
                                    return $ mconcat updCss
        in MkAnUpdateFunction gAC uAC

ufUpdates ::
       (MonadIO m, Monad (ApplyStack tm m))
    => AnUpdateFunction tm updateA updateB
    -> [updateA]
    -> MutableRead m (UpdateReader updateA)
    -> ApplyStack tm m [updateB]
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
    in MkRunnable2 cmEmpty MkAnUpdateFunction {..}

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
immutableUpdateFunction mr = MkRunnable2 cmEmpty $ MkAnUpdateFunction {ufGet = \_ -> mr, ufUpdate = \_ _ -> return []}

ioConstUpdateFunction ::
       SubjectReader (UpdateReader updateB) => IO (UpdateSubject updateB) -> UpdateFunction updateA updateB
ioConstUpdateFunction iob = immutableUpdateFunction $ mSubjectToMutableRead $ liftIO iob

constUpdateFunction :: SubjectReader (UpdateReader updateB) => UpdateSubject updateB -> UpdateFunction updateA updateB
constUpdateFunction b = ioConstUpdateFunction $ return b

updateFunctionRead ::
       forall m updateA updateB. MonadUnliftIO m
    => UpdateFunction updateA updateB
    -> MutableRead m (UpdateReader updateA)
    -> MutableRead m (UpdateReader updateB)
updateFunctionRead (MkRunnable2 trun (MkAnUpdateFunction g _)) mr rt =
    runMonoTransStackRunner trun $ \run -> run $ g mr rt
