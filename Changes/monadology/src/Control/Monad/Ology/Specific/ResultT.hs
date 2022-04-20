module Control.Monad.Ology.Specific.ResultT where

import Control.Monad.Ology.Specific.ComposeInner
import Control.Monad.Ology.Specific.Result

type ResultT e = ComposeInner (Result e)

runResultT :: forall m e a. ResultT e m a -> m (Result e a)
runResultT = getComposeInner
