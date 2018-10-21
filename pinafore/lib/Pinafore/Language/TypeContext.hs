module Pinafore.Language.TypeContext where

import Shapes

data TypeContext =
    MkTypeContext

newtype PinaforeTypeCheck a =
    MkPinaforeTypeCheck (ReaderT TypeContext (Result Text) a)
    deriving (Functor, Applicative, Monad, MonadFail)

typeCheckResult :: Result Text a -> PinaforeTypeCheck a
typeCheckResult ra = MkPinaforeTypeCheck $ lift ra

runPinaforeTypeCheck :: PinaforeTypeCheck a -> Result Text a
runPinaforeTypeCheck (MkPinaforeTypeCheck qa) = runReaderT qa MkTypeContext
