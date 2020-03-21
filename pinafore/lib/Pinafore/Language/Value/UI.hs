module Pinafore.Language.Value.UI where

import Data.Shim
import Shapes
import Truth.Core

newtype PinaforeUI a = MkPinaforeUI
    { unPinaforeUI :: SelectNotify a -> CVUISpec
    }

instance Functor PinaforeUI where
    fmap ab (MkPinaforeUI spec) = MkPinaforeUI $ \sn -> spec $ contramap ab sn

instance HasVariance 'Covariance PinaforeUI where
    varianceRepresentational = Nothing
