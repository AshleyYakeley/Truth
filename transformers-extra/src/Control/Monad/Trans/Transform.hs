module Control.Monad.Trans.Transform where

import Control.Category
import Data.Kind
import Prelude hiding ((.), id)

newtype Transform (p :: k -> Type) (q :: k -> Type) = MkTransform
    { runTransform :: forall (a :: k). p a -> q a
    }

instance Category Transform where
    id = MkTransform id
    (MkTransform bc) . (MkTransform ab) = MkTransform $ bc . ab
