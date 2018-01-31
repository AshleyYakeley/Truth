module Truth.Core.Object.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read

mapSubscriber ::
       forall edita editb action. (Edit edita)
    => EditLens edita editb
    -> Subscriber edita action
    -> Subscriber editb action
mapSubscriber lens@(MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens lensFunc _)) sub =
    MkSubscriber $ \(initialB :: Object editb -> IO editor) updateB -> let
        initialA :: Object edita -> IO editor
        initialA objectA = initialB $ mapObject lens objectA
        updateA ::
               forall m. MonadUnliftIO m
            => editor
            -> MutableRead m (EditReader edita)
            -> [edita]
            -> m ()
        updateA editor mrA editAs =
            unlift $
            withTransConstraintTM @MonadUnliftIO $ do
                editBs <- efUpdates lensFunc editAs mrA
                updateB editor (efGet lensFunc mrA) editBs
        in subscribe sub initialA updateA

convertSubscriber ::
       forall edita editb actions. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => Subscriber edita actions
    -> Subscriber editb actions
convertSubscriber = mapSubscriber convertEditLens
