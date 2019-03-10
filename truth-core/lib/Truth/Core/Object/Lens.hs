module Truth.Core.Object.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Object.Update

mapSubscriber ::
       forall edita editb. (ApplicableEdit edita)
    => EditLens edita editb
    -> Subscriber edita
    -> Subscriber editb
mapSubscriber lens (MkSubscriber objectA subA) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    in MkSubscriber objectB $ \updateB -> let
           updateA :: [edita] -> IO ()
           updateA editAs = do
               editBs <- objectMapUpdates (editLensFunction lens) objectA editAs
               updateB editBs
           in subA updateA

convertSubscriber ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => Subscriber edita
    -> Subscriber editb
convertSubscriber = mapSubscriber convertEditLens
