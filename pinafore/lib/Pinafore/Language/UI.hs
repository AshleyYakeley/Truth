module Pinafore.Language.UI where

import Language.Expression.Dolan
import Shapes
import Truth.Core

newtype PinaforeUI baseedit a = MkPinaforeUI
    { unPinaforeUI :: UISpec a baseedit
    }

instance Functor (PinaforeUI baseedit) where
    fmap ab (MkPinaforeUI spec) = MkPinaforeUI $ mapSelectionUISpec ab spec

instance HasDolanVary '[ 'Covariance] (PinaforeUI baseedit) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap