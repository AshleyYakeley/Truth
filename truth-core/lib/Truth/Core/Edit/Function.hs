module Truth.Core.Edit.Function where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Unlift
import Truth.Core.Import
import Truth.Core.Read

data AnEditFunction t edita editb = MkAnEditFunction
    { efGet :: ReadFunctionT t (EditReader edita) (EditReader editb)
    , efUpdate :: forall m. MonadIO m =>
                                edita -> MutableRead m (EditReader edita) -> t m [editb]
    }

type EditFunction = CloseUnlift AnEditFunction

instance Unliftable AnEditFunction where
    fmapUnliftable t1t2 (MkAnEditFunction g u) = MkAnEditFunction (\mr rt -> t1t2 $ g mr rt) (\ea mr -> t1t2 $ u ea mr)

instance UnliftCategory AnEditFunction where
    ucId = let
        efGet = remonadMutableRead IdentityT
        efUpdate edit _ = IdentityT $ return [edit]
        in MkAnEditFunction {..}
    ucCompose ::
           forall tab tbc edita editb editc. (MonadTransConstraint MonadIO tab, MonadTransConstraint MonadIO tbc)
        => AnEditFunction tbc editb editc
        -> AnEditFunction tab edita editb
        -> AnEditFunction (ComposeT tbc tab) edita editc
    ucCompose (MkAnEditFunction gBC uBC) (MkAnEditFunction gAB uAB) = let
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
        in MkAnEditFunction gAC uAC

efUpdates ::
       (MonadIO m, Monad (t m))
    => AnEditFunction t edita editb
    -> [edita]
    -> MutableRead m (EditReader edita)
    -> t m [editb]
efUpdates _ [] _ = return []
efUpdates sef (ea:eas) mr = do
    eb <- efUpdate sef ea mr
    ebs <- efUpdates sef eas mr
    return $ eb ++ ebs

funcEditFunction ::
       forall edita editb. (FullSubjectReader (EditReader edita), Edit edita, FullEdit editb)
    => (EditSubject edita -> EditSubject editb)
    -> EditFunction edita editb
funcEditFunction ab = let
    efGet :: ReadFunctionT IdentityT (EditReader edita) (EditReader editb)
    efGet mra rt = lift $ (mSubjectToMutableRead $ fmap ab $ mutableReadToSubject mra) rt
    efUpdate ::
           forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> IdentityT m [editb]
    efUpdate edita mra =
        lift $ getReplaceEdits $ mSubjectToMutableRead $ fmap ab $ mutableReadToSubject $ applyEdit edita mra
    in MkCloseUnlift identityUnlift MkAnEditFunction {..}

constEditFunction :: SubjectReader (EditReader editb) => EditSubject editb -> EditFunction edita editb
constEditFunction b =
    MkCloseUnlift identityUnlift $
    MkAnEditFunction {efGet = \_ rt -> mSubjectToMutableRead (return b) rt, efUpdate = \_ _ -> return []}

editFunctionRead ::
       forall m edita editb. MonadUnliftIO m
    => EditFunction edita editb
    -> MutableRead m (EditReader edita)
    -> MutableRead m (EditReader editb)
editFunctionRead (MkCloseUnlift unlift (MkAnEditFunction g _)) mr rt = runUnlift unlift $ g mr rt
