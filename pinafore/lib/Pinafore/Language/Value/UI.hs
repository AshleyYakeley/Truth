module Pinafore.Language.Value.UI where

import Data.Shim
import Shapes
import Truth.Core

newtype PinaforeUI a = MkPinaforeUI
    { unPinaforeUI :: LUISpec a
    }

instance Functor PinaforeUI where
    fmap ab (MkPinaforeUI spec) = MkPinaforeUI $ mapSelectionUISpec ab spec

instance HasVariance 'Covariance PinaforeUI where
    varianceRepresentational = Nothing
