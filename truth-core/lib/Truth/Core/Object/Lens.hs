module Truth.Core.Object.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Object.Update

mapSubscriber ::
       forall edita editb action. (ApplicableEdit edita)
    => EditLens edita editb
    -> Subscriber edita action
    -> Subscriber editb action
mapSubscriber lens sub =
    MkSubscriber $ \(initialB :: Object editb -> IO editor) updateB -> let
        getObjectB :: Object edita -> Object editb
        getObjectB = lensObject True lens
        initialA :: Object edita -> IO editor
        initialA objectA = do
            editor <- initialB $ getObjectB objectA
            return editor
        updateA :: editor -> Object edita -> [edita] -> IO ()
        updateA editor objectA editAs = do
            editBs <- objectMapUpdates (editLensFunction lens) objectA editAs
            updateB editor (getObjectB objectA) editBs
        in do
               (editor, objectA, actions) <- subscribe sub initialA updateA
               return (editor, getObjectB objectA, actions)

convertSubscriber ::
       forall edita editb actions. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => Subscriber edita actions
    -> Subscriber editb actions
convertSubscriber = mapSubscriber convertEditLens
