{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core.Edit.Function where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.Unlift
import Truth.Core.Import
import Truth.Core.Read

data AnEditFunction t edita editb = MkAnEditFunction
    { efGet :: forall m. MonadIO m =>
                             MutableRead m (EditReader edita) -> MutableRead (t m) (EditReader editb)
    , efUpdate :: forall m. MonadIO m =>
                                edita -> MutableRead m (EditReader edita) -> t m [editb]
    }

type EditFunction' = CloseUnlift AnEditFunction

instance UnliftCategory AnEditFunction where
    type UnliftCategoryConstraint AnEditFunction edit = ()
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

instance Category EditFunction' where
    id = cid
    (.) = (<.>)

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
