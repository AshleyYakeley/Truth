{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Subscriber
    ( module Truth.Debug
    , module Truth.Debug.Edit
    , module Truth.Debug.Object
    ) where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.Reference.Model
import Truth.Debug
import Truth.Debug.Edit
import Truth.Debug.Object

instance TraceThing (Model edit) where
    traceThing prefix (MkResource rr (MkAModel anobj sub utask)) = case resourceRunnerStackUnliftDict @IO rr of
        Dict -> case resourceRunnerStackUnliftDict @LifeCycleIO rr of
            Dict -> MkResource rr $ MkAModel (traceAnObject prefix blankEditShower anobj) (\task call -> traceBracket (contextStr prefix "update") $ sub task call) utask
