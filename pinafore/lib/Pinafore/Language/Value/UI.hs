module Pinafore.Language.Value.UI where

import Data.Shim
import Shapes
import Truth.Core

newtype PinaforeUI baseupdate a = MkPinaforeUI
    { unPinaforeUI :: UISpec a baseupdate
    }

instance Functor (PinaforeUI baseupdate) where
    fmap ab (MkPinaforeUI spec) = MkPinaforeUI $ mapSelectionUISpec ab spec

instance HasVariance 'Covariance (PinaforeUI baseupdate) where
    varianceRepresentational = Nothing
