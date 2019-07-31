module Pinafore.Language.UI where

import Data.Shim
import Shapes
import Truth.Core

newtype PinaforeUI baseedit a = MkPinaforeUI
    { unPinaforeUI :: UISpec a baseedit
    }

instance Functor (PinaforeUI baseedit) where
    fmap ab (MkPinaforeUI spec) = MkPinaforeUI $ mapSelectionUISpec ab spec

instance HasVariance 'Covariance (PinaforeUI baseedit) where
    varianceRepresentational = Nothing
