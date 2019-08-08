{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Subscriber
    ( module Truth.Debug
    , module Truth.Debug.Edit
    , module Truth.Debug.Object
    ) where

import Truth.Core.Import
import Truth.Core.Object.UnliftIO
import Truth.Core.Object.Subscriber
import Truth.Debug
import Truth.Debug.Edit
import Truth.Debug.Object

instance TraceThing (Subscriber edit) where
    traceThing prefix (MkCloseUnliftIO run (MkASubscriber anobj sub)) =
        MkCloseUnliftIO (traceThing (contextStr prefix "run") run) $ MkASubscriber (traceAnObject prefix blankEditShower anobj) $ traceThing (contextStr prefix "update") sub
