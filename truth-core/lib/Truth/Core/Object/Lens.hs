module Truth.Core.Object.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Object.UnliftIO
import Truth.Core.Object.Update

mapASubscriber ::
       forall t m edita editb. (MonadTransUnlift t, MonadUnliftIO m)
    => UnliftIO (t m)
    -> AnEditLens t edita editb
    -> ASubscriber m edita
    -> ASubscriber (t m) editb
mapASubscriber unlift alens (MkASubscriber objectA subA)
    | Dict <- hasTransConstraint @MonadUnliftIO @t @m = let
        objectB :: AnObject (t m) editb
        objectB = lensAnObject alens objectA
        in MkASubscriber objectB $ \updateB -> let
               updateA :: [edita] -> EditContext -> IO ()
               updateA [] _ectxt = return ()
               updateA editAs ectxt = do
                   editBs <- runTransform unlift $ anObjectMapUpdates (elFunction alens) objectA editAs
                   case editBs of
                       [] -> return ()
                       _ -> updateB editBs ectxt
               in remonad lift $ subA updateA

mapSubscriber :: forall edita editb. EditLens edita editb -> Subscriber edita -> Subscriber editb
mapSubscriber (MkCloseUnlift (lensUnlift :: Unlift t) alens) (MkCloseUnliftIO (subUnlift :: UnliftIO m) asub)
    | Dict <- hasTransConstraint @MonadUnliftIO @t @m = let
        unliftDiscard = lensObjectUnliftDiscard lensUnlift subUnlift
        unliftFull = lensObjectUnliftFull lensUnlift subUnlift
        in MkCloseUnliftIO unliftDiscard (mapASubscriber unliftFull alens asub)

convertSubscriber ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => Subscriber edita
    -> Subscriber editb
convertSubscriber = mapSubscriber convertEditLens
