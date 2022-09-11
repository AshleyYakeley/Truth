{-# OPTIONS -fno-warn-orphans #-}

module Changes.Debug.Subscriber
    ( module Changes.Debug
    , module Changes.Debug.Edit
    , module Changes.Debug.Reference
    ) where

import Changes.Core.Import
import Changes.Core.Model.Model
import Changes.Core.Resource
import Changes.Debug
import Changes.Debug.Edit
import Changes.Debug.Reference

instance TraceThing (Model edit) where
    traceThing prefix (MkResource rr (MkAModel anobj sub utask)) =
        case resourceRunnerStackUnliftDict @IO rr of
            Dict ->
                case resourceRunnerStackUnliftDict @Lifecycle rr of
                    Dict ->
                        MkResource rr $
                        MkAModel
                            (traceAReference prefix blankEditShower anobj)
                            (\task call -> traceBracket_ (contextStr prefix "update") $ sub task call)
                            utask
