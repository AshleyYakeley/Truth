module Truth.Core.Edit.Function where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Unlift
import Truth.Core.Import
import Truth.Core.Read

data AnUpdateFunction t edita editb = MkAnUpdateFunction
    { ufGet :: ReadFunctionT t (EditReader edita) (EditReader editb)
    , ufUpdate :: forall m. MonadIO m => edita -> MutableRead m (EditReader edita) -> t m [editb]
    -- ^ the MutableRead argument will reflect the new value
    }

type UpdateFunction = CloseUnlift AnUpdateFunction

instance Unliftable AnUpdateFunction where
    fmapUnliftable t1t2 (MkAnUpdateFunction g u) =
        MkAnUpdateFunction (\mr rt -> t1t2 $ g mr rt) (\ea mr -> t1t2 $ u ea mr)

instance UnliftCategory AnUpdateFunction where
    ucId = let
        ufGet = remonadMutableRead IdentityT
        ufUpdate edit _ = IdentityT $ return [edit]
        in MkAnUpdateFunction {..}
    ucCompose ::
           forall tab tbc edita editb editc. (MonadTransConstraint MonadIO tab, MonadTransConstraint MonadIO tbc)
        => AnUpdateFunction tbc editb editc
        -> AnUpdateFunction tab edita editb
        -> AnUpdateFunction (ComposeT tbc tab) edita editc
    ucCompose (MkAnUpdateFunction gBC uBC) (MkAnUpdateFunction gAB uAB) = let
        gAC :: forall m. MonadIO m
            => MutableRead m (EditReader edita)
            -> MutableRead (ComposeT tbc tab m) (EditReader editc)
        gAC mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict -> remonadMutableRead MkComposeT $ gBC @(tab m) $ gAB @m mra
        uAC :: forall m. MonadIO m
            => edita
            -> MutableRead m (EditReader edita)
            -> ComposeT tbc tab m [editc]
        uAC editA mrA =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            MkComposeT $ do
                                editbs <- lift $ uAB editA mrA
                                editcss <- for editbs $ \editb -> uBC editb $ gAB mrA
                                return $ mconcat editcss
        in MkAnUpdateFunction gAC uAC

ufUpdates ::
       (MonadIO m, Monad (t m))
    => AnUpdateFunction t edita editb
    -> [edita]
    -> MutableRead m (EditReader edita)
    -> t m [editb]
ufUpdates _ [] _ = return []
ufUpdates sef (ea:eas) mr = do
    eb <- ufUpdate sef ea mr
    ebs <- ufUpdates sef eas mr
    return $ eb ++ ebs

ioFuncUpdateFunction ::
       forall edita editb. (FullSubjectReader (EditReader edita), ApplicableEdit edita, FullEdit editb)
    => (EditSubject edita -> IO (EditSubject editb))
    -> UpdateFunction edita editb
ioFuncUpdateFunction amb = let
    ufGet :: ReadFunctionT IdentityT (EditReader edita) (EditReader editb)
    ufGet mra rt = lift $ (mSubjectToMutableRead $ mutableReadToSubject mra >>= \a -> liftIO (amb a)) rt
    ufUpdate ::
           forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> IdentityT m [editb]
    ufUpdate edita mra =
        lift $
        getReplaceEdits $ mSubjectToMutableRead $ (mutableReadToSubject $ applyEdit edita mra) >>= \a -> liftIO (amb a)
    in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

funcUpdateFunction ::
       forall edita editb. (FullSubjectReader (EditReader edita), ApplicableEdit edita, FullEdit editb)
    => (EditSubject edita -> EditSubject editb)
    -> UpdateFunction edita editb
funcUpdateFunction ab = ioFuncUpdateFunction $ \a -> return $ ab a

immutableUpdateFunction :: (forall m. MonadIO m => MutableRead m (EditReader editb)) -> UpdateFunction edita editb
immutableUpdateFunction mr =
    MkCloseUnlift identityUnlift $ MkAnUpdateFunction {ufGet = \_ -> mr, ufUpdate = \_ _ -> return []}

ioConstUpdateFunction :: SubjectReader (EditReader editb) => IO (EditSubject editb) -> UpdateFunction edita editb
ioConstUpdateFunction iob = immutableUpdateFunction $ mSubjectToMutableRead $ liftIO iob

constUpdateFunction :: SubjectReader (EditReader editb) => EditSubject editb -> UpdateFunction edita editb
constUpdateFunction b = ioConstUpdateFunction $ return b

updateFunctionRead ::
       forall m edita editb. MonadUnliftIO m
    => UpdateFunction edita editb
    -> MutableRead m (EditReader edita)
    -> MutableRead m (EditReader editb)
updateFunctionRead (MkCloseUnlift unlift (MkAnUpdateFunction g _)) mr rt = runUnlift unlift $ g mr rt
