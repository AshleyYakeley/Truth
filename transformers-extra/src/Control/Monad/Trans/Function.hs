module Control.Monad.Trans.Function where

import Control.Category
import Data.Kind
import Prelude hiding ((.), id)

type MFunction (p :: k -> Type) (q :: k -> Type) = forall (a :: k). p a -> q a

newtype WMFunction (p :: k -> Type) (q :: k -> Type) = MkWMFunction
    { runWMFunction :: MFunction p q
    }

instance Category WMFunction where
    id = MkWMFunction id
    (MkWMFunction bc) . (MkWMFunction ab) = MkWMFunction $ bc . ab

type MBackFunction (ma :: k -> Type) (mb :: k -> Type) = forall (r :: k). (MFunction mb ma -> ma r) -> mb r

newtype WMBackFunction (p :: k -> Type) (q :: k -> Type) = MkWMBackFunction
    { runWMBackFunction :: MBackFunction p q
    }

instance Category WMBackFunction where
    id = MkWMBackFunction $ \f -> f id
    (MkWMBackFunction bc) . (MkWMBackFunction ab) = MkWMBackFunction $ \f -> bc $ \mcmb -> ab $ \mbma -> f $ mbma . mcmb
