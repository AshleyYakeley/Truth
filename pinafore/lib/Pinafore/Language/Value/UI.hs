module Pinafore.Language.Value.UI where

import Data.Shim
import Shapes
import Truth.Core

newtype LangUI a = MkLangUI
    { unLangUI :: SelectNotify a -> CVUISpec
    }

instance Functor LangUI where
    fmap ab (MkLangUI spec) = MkLangUI $ \sn -> spec $ contramap ab sn

instance HasVariance 'Covariance LangUI where
    varianceRepresentational = Nothing
