module Truth.Core.Object.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read

mapSubscriber ::
       forall edita editb action. (Edit edita)
    => GeneralLens edita editb
    -> Subscriber edita action
    -> Subscriber editb action
mapSubscriber lens@(MkCloseState (MkEditLens {..} :: EditLens lensstate edita editb)) sub =
    MkSubscriber $ \(initialB :: Object editb -> IO editor) updateB ->
        let MkEditFunction {..} = editLensFunction
            initialA :: Object edita -> IO (Object edita, editor)
            initialA objectA = do
                ed <- initialB $ mapObject lens objectA
                return (objectA, ed)
            updateA ::
                   forall m. IsStateIO m
                => (Object edita, editor)
                -> MutableRead m (EditReader edita)
                -> [edita]
                -> m ()
            updateA (_objectA, editor) mr editAs =
                editAccess $
                StateT $ \oldls -> do
                    (newls, editBs) <- unReadable (editUpdates editLensFunction editAs oldls) mr
                    updateB editor (mapMutableRead (editGet newls) mr) editBs
                    return ((), newls)
        in do ((_, editor), closer, action) <- subscribe sub initialA updateA
              return (editor, closer, action)

convertSubscriber ::
       forall edita editb actions. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => Subscriber edita actions
    -> Subscriber editb actions
convertSubscriber = mapSubscriber $ toGeneralLens convertEditLens
