{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core.Edit.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Unlift
import Truth.Core.Import
import Truth.Core.Read

data AnEditLens t edita editb = MkAnEditLens
    { elFunction :: AnEditFunction t edita editb
    , elPutEdit :: forall m. MonadIO m =>
                                 editb -> MutableRead m (EditReader edita) -> t m (Maybe [edita])
    }

type EditLens' = CloseUnlift AnEditLens

instance UnliftCategory AnEditLens where
    type UnliftCategoryConstraint AnEditLens edit = Edit edit
    ucId = let
        pe :: forall m edit. MonadIO m
           => edit
           -> MutableRead m (EditReader edit)
           -> IdentityT m (Maybe [edit])
        pe edit _ = return $ Just [edit]
        in MkAnEditLens ucId pe
    ucCompose ::
           forall tab tbc edita editb editc.
           (MonadTransConstraint MonadIO tab, MonadTransConstraint MonadIO tbc, Edit edita, Edit editb, Edit editc)
        => AnEditLens tbc editb editc
        -> AnEditLens tab edita editb
        -> AnEditLens (ComposeT tbc tab) edita editc
    ucCompose (MkAnEditLens efBC peBC) lensAB@(MkAnEditLens efAB _) = let
        peAC ::
               forall m. MonadIO m
            => editc
            -> MutableRead m (EditReader edita)
            -> ComposeT tbc tab m (Maybe [edita])
        peAC ec mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            getCompose $ do
                                ebs <- Compose $ MkComposeT $ peBC ec $ efGet efAB mra
                                Compose $ lift2ComposeT $ elPutEdits lensAB ebs mra
        efAC = ucCompose efBC efAB
        in MkAnEditLens efAC peAC

elPutEdits ::
       (MonadIO m, Monad (t m), Edit edita)
    => AnEditLens t edita editb
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> t m (Maybe [edita])
elPutEdits _ [] _ = getCompose $ return []
elPutEdits lens (e:ee) mr =
    getCompose $ do
        ea <- Compose $ elPutEdit lens e mr
        eea <- Compose $ elPutEdits lens ee $ mapMutableRead (applyEdits ea) $ mapMutableRead (applyEdits ea) mr
        return $ ea ++ eea
