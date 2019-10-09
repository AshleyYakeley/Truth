module Truth.Core.Edit.Function where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Run
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data AnUpdateFunction t updateA updateB = MkAnUpdateFunction
    { ufGet :: ReadFunctionT t (UpdateReader updateA) (UpdateReader updateB)
    , ufUpdate :: forall m. MonadIO m => updateA -> MutableRead m (UpdateReader updateA) -> t m [updateB]
    -- ^ the MutableRead argument will reflect the new value
    }

type UpdateFunction = RunnableT2 AnUpdateFunction

instance Unliftable AnUpdateFunction where
    fmapUnliftable t1t2 (MkAnUpdateFunction g u) =
        MkAnUpdateFunction (\mr rt -> t1t2 $ g mr rt) (\ea mr -> t1t2 $ u ea mr)

instance RunnableCategory AnUpdateFunction where
    ucId = let
        ufGet = remonadMutableRead IdentityT
        ufUpdate update _ = IdentityT $ return [update]
        in MkAnUpdateFunction {..}
    ucCompose ::
           forall tab tbc updateA updateB editc. (MonadTransConstraint MonadIO tab, MonadTransConstraint MonadIO tbc)
        => AnUpdateFunction tbc updateB editc
        -> AnUpdateFunction tab updateA updateB
        -> AnUpdateFunction (ComposeT tbc tab) updateA editc
    ucCompose (MkAnUpdateFunction gBC uBC) (MkAnUpdateFunction gAB uAB) = let
        gAC :: forall m. MonadIO m
            => MutableRead m (UpdateReader updateA)
            -> MutableRead (ComposeT tbc tab m) (UpdateReader editc)
        gAC mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict -> remonadMutableRead MkComposeT $ gBC @(tab m) $ gAB @m mra
        uAC :: forall m. MonadIO m
            => updateA
            -> MutableRead m (UpdateReader updateA)
            -> ComposeT tbc tab m [editc]
        uAC editA mrA =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            MkComposeT $ do
                                editbs <- lift $ uAB editA mrA
                                editcss <- for editbs $ \updateB -> uBC updateB $ gAB mrA
                                return $ mconcat editcss
        in MkAnUpdateFunction gAC uAC

ufUpdates ::
       (MonadIO m, Monad (t m))
    => AnUpdateFunction t updateA updateB
    -> [updateA]
    -> MutableRead m (UpdateReader updateA)
    -> t m [updateB]
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
    ufGet :: ReadFunctionT IdentityT (UpdateReader updateA) (UpdateReader updateB)
    ufGet mra rt = lift $ (mSubjectToMutableRead $ mutableReadToSubject mra >>= \a -> liftIO (amb a)) rt
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m [updateB]
    ufUpdate updateA mra =
        lift $
        fmap (fmap editUpdate) $
        getReplaceEdits $
        mSubjectToMutableRead $ do
            a <- mutableReadToSubject $ applyEdit (updateEdit updateA) mra
            liftIO $ amb a
    in MkRunnableT2 identityUntrans MkAnUpdateFunction {..}

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
immutableUpdateFunction mr =
    MkRunnableT2 identityUntrans $ MkAnUpdateFunction {ufGet = \_ -> mr, ufUpdate = \_ _ -> return []}

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
updateFunctionRead (MkRunnableT2 unlift (MkAnUpdateFunction g _)) mr rt = unlift $ g mr rt
