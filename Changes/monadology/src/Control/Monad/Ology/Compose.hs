module Control.Monad.Ology.Compose where

import Control.Monad.Ology.Functor.MonadOne
import Control.Monad.Ology.Result
import Control.Monad.Ology.Trans.Constraint
import Control.Monad.Ology.Trans.Stack
import Control.Monad.Ology.Trans.Tunnel
import Import

type ComposeM :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype ComposeM inner outer a = MkComposeM
    { getComposeM :: outer (inner a)
    }

instance (Functor inner, Functor outer) => Functor (ComposeM inner outer) where
    fmap ab (MkComposeM oia) = MkComposeM $ fmap (fmap ab) oia

instance (MonadOne inner, Monad outer) => Applicative (ComposeM inner outer) where
    pure a = MkComposeM $ pure $ pure a
    mab <*> ma = do
        ab <- mab
        a <- ma
        return $ ab a

instance (MonadOne inner, Monad outer, Alternative inner) => Alternative (ComposeM inner outer) where
    empty = MkComposeM $ pure empty
    (MkComposeM oia) <|> cb = do
        ma <-
            MkComposeM $ do
                ia <- oia
                return $ fmap Just ia <|> return Nothing
        case ma of
            Just a -> return a
            Nothing -> cb

instance (MonadOne inner, Monad outer) => Monad (ComposeM inner outer) where
    return = pure
    (MkComposeM oia) >>= p =
        MkComposeM $ do
            ia <- oia
            case retrieveOne ia of
                SuccessResult a -> do
                    ib <- getComposeM $ p a
                    return $ ia >> ib
                FailureResult ix -> return $ fmap never ix

instance (MonadOne inner, MonadFix outer) => MonadFix (ComposeM inner outer) where
    mfix ama =
        MkComposeM $
        mfix $ \ia ->
            getComposeM $
            ama $
            case retrieveOne ia of
                SuccessResult a -> a
                FailureResult _ -> error "bad ComposeM mfix"

instance (MonadOne inner, Monad outer, Alternative inner) => MonadPlus (ComposeM inner outer)

liftOuter :: (Functor outer, Applicative inner) => outer a -> ComposeM inner outer a
liftOuter ma = MkComposeM $ fmap pure ma

liftInner :: Applicative outer => inner a -> ComposeM inner outer a
liftInner na = MkComposeM $ pure na

instance (MonadOne inner, MonadIO outer) => MonadIO (ComposeM inner outer) where
    liftIO ioa = liftOuter $ liftIO ioa

instance MonadOne inner => MonadTrans (ComposeM inner) where
    lift = liftOuter

instance MonadOne inner => TransConstraint Functor (ComposeM inner) where
    hasTransConstraint = Dict

instance MonadOne inner => TransConstraint Monad (ComposeM inner) where
    hasTransConstraint = Dict

instance MonadOne inner => TransConstraint MonadIO (ComposeM inner) where
    hasTransConstraint = Dict

instance MonadOne inner => TransConstraint MonadFix (ComposeM inner) where
    hasTransConstraint = Dict

instance MonadOne inner => TransTunnel (ComposeM inner) where
    type Tunnel (ComposeM inner) = inner
    tunnel call = MkComposeM $ call getComposeM

transComposeOne :: (TransTunnel t, Monad m, MonadOne f) => t (ComposeM f m) a -> t m (f a)
transComposeOne tca =
    withTransConstraintTM @Monad $
    fmap (restoreOne . eitherToResult) $
    transExcept $ hoist (ExceptT . fmap (resultToEither . retrieveOne) . getComposeM) tca

transStackComposeOne ::
       forall tt f m a. (MonadTransStackTunnel tt, Monad m, MonadOne f)
    => ApplyStack tt (ComposeM f m) a
    -> ApplyStack tt m (f a)
transStackComposeOne tca =
    case transStackDict @Monad @tt @m of
        Dict ->
            fmap (restoreOne . eitherToResult) $
            transStackExcept @tt @m @(f None) $
            stackHoist
                @tt
                @(ComposeM f m)
                @(ExceptT (f None) _)
                (ExceptT . fmap (resultToEither . retrieveOne) . getComposeM)
                tca
