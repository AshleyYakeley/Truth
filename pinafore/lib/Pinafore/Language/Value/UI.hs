{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Value.UI where

import Data.Shim
import Shapes
import Truth.Core

type LangUI = CVUISpec

type LangNotifier = SelectNotify

instance HasVariance 'Contravariance SelectNotify where
    varianceRepresentational = Nothing
