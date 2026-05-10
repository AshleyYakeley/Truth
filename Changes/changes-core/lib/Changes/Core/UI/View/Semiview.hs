module Changes.Core.UI.View.Semiview
    ( Semiview
    , runSemiview
    , semiviewGetContext
    , semiviewWithContext
    )
where

import Changes.Core.Import
import Changes.Core.UI.View.Context

newtype Semiview a = MkSemiview (ReaderT ViewContext IO a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadFix
        , MonadFail
        , MonadIO
        , MonadException
        , MonadThrow e
        , MonadCatch e
        , MonadHoistIO
        , MonadTunnelIO
        , MonadUnliftIO
        , MonadAskUnliftIO
        , RepresentationalRole
        )

runSemiview :: ViewContext -> Semiview --> IO
runSemiview vc (MkSemiview sva) = runReaderT sva vc

semiviewGetContext :: Semiview ViewContext
semiviewGetContext = MkSemiview ask

semiviewWithContext :: (ViewContext -> ViewContext) -> Semiview --> Semiview
semiviewWithContext f (MkSemiview ma) = MkSemiview $ withReaderT f ma
