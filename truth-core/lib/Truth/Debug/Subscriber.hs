{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Subscriber
    ( module Truth.Debug
    , module Truth.Debug.Edit
    , module Truth.Debug.Object
    ) where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.Object.Subscriber
import Truth.Debug
import Truth.Debug.Edit
import Truth.Debug.Object

instance TraceThing (Subscriber edit) where
    traceThing prefix (MkResource rr (MkASubscriber anobj sub utask)) = case resourceRunnerStackUnliftDict @IO rr of
        Dict -> case resourceRunnerStackUnliftDict @LifeCycleIO rr of
            Dict -> MkResource rr $ MkASubscriber (traceAnObject prefix blankEditShower anobj) (\task call -> traceBracket (contextStr prefix "update") $ sub task call) utask
