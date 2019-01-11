module Control.Monad.Trans.Compose where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.AskUnlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Except
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Constraint
import Data.Kind
import Prelude

newtype ComposeT (t1 :: (Type -> Type) -> (Type -> Type)) (t2 :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) (a :: Type) = MkComposeT
    { unComposeT :: t1 (t2 m) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO, MonadFix, MonadPlus)

lift1ComposeT :: (MonadTransTunnel t1, MonadTrans t2, Monad m) => t1 m a -> ComposeT t1 t2 m a
lift1ComposeT t1ma = MkComposeT $ remonad lift t1ma

lift2ComposeT ::
       forall t1 t2 m a. (MonadTrans t1, Monad (t2 m))
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT t2ma = MkComposeT $ lift t2ma

lift2ComposeT' ::
       forall t1 t2 m a. (MonadTrans t1, MonadTransConstraint Monad t2, Monad m)
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT' =
    case hasTransConstraint @Monad @t2 @m of
        Dict -> lift2ComposeT

lift2ComposeT'' ::
       forall t1 t2 m a. (MonadTrans t1, MonadTransConstraint MonadIO t2, MonadIO m)
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT'' =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> lift2ComposeT

lift1ComposeTWithUnlift ::
       (MonadTransTunnel t1, MonadTransUnlift t2, MonadUnliftIO m)
    => ((forall a. ComposeT t1 t2 m a -> t1 m a) -> t1 m r)
    -> ComposeT t1 t2 m r
lift1ComposeTWithUnlift call =
    MkComposeT $
    tunnel $ \tun -> liftWithUnlift $ \(MkUnlift unlift) -> tun $ call $ \(MkComposeT ttma) -> remonad unlift ttma

lift2ComposeTWithUnlift ::
       forall t1 t2 m r. (MonadTransUnlift t1, MonadTransUnlift t2, MonadUnliftIO m)
    => ((forall a. ComposeT t1 t2 m a -> t2 m a) -> t2 m r)
    -> ComposeT t1 t2 m r
lift2ComposeTWithUnlift call =
    case hasTransConstraint @MonadUnliftIO @t2 @m of
        Dict -> MkComposeT $ liftWithUnlift $ \(MkUnlift unlift) -> call $ \(MkComposeT ttma) -> unlift ttma

composeUnlift :: MonadTransUnlift tb => Unlift ta -> Unlift tb -> Unlift (ComposeT ta tb)
composeUnlift (MkUnlift ua) (MkUnlift ub) =
    MkUnlift $ \(MkComposeT tatbma) -> ub $ withTransConstraintTM @MonadUnliftIO $ ua tatbma

instance (MonadTrans t1, MonadTransConstraint Monad t2) => MonadTrans (ComposeT t1 t2) where
    lift (ma :: m a) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ lift $ lift ma

instance (MonadTransConstraint Monad t1, MonadTransConstraint Monad t2) => MonadTransConstraint Monad (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. Monad m
        => Dict (Monad (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                case hasTransConstraint @Monad @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadIO t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadIO t2) =>
             MonadTransConstraint MonadIO (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadIO m
        => Dict (MonadIO (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t2 @m of
            Dict ->
                case hasTransConstraint @MonadIO @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadFail t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadFail t2) =>
             MonadTransConstraint MonadFail (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadFail m
        => Dict (MonadFail (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadFail @t2 @m of
            Dict ->
                case hasTransConstraint @MonadFail @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadFix t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadFix t2) =>
             MonadTransConstraint MonadFix (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadFix m
        => Dict (MonadFix (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadFix @t2 @m of
            Dict ->
                case hasTransConstraint @MonadFix @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadPlus t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadPlus t2) =>
             MonadTransConstraint MonadPlus (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadPlus m
        => Dict (MonadPlus (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadPlus @t2 @m of
            Dict ->
                case hasTransConstraint @MonadPlus @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransTunnel t1, MonadTransTunnel t2) => MonadTransTunnel (ComposeT t1 t2) where
    tunnel :: forall m2 r. (forall a. (forall m1. ComposeT t1 t2 m1 r -> m1 a) -> m2 a) -> ComposeT t1 t2 m2 r
    tunnel call =
        MkComposeT $
        tunnel $ \t1m1rm1a -> tunnel $ \t2m1am1b -> call $ \(MkComposeT t1t2m1r) -> t2m1am1b $ t1m1rm1a $ t1t2m1r
    transExcept ::
           forall m e a. Monad m
        => ComposeT t1 t2 (ExceptT e m) a
        -> ComposeT t1 t2 m (Either e a)
    transExcept (MkComposeT ma) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ transExcept $ remonad (\t2ea -> ExceptT $ transExcept t2ea) ma

instance (MonadTransUnlift t1, MonadTransUnlift t2) => MonadTransUnlift (ComposeT t1 t2) where
    liftWithUnlift ::
           forall m r. MonadUnliftIO m
        => (Unlift (ComposeT t1 t2) -> m r)
        -> ComposeT t1 t2 m r
    liftWithUnlift call =
        case hasTransConstraint @MonadUnliftIO @t2 @m of
            Dict ->
                MkComposeT $
                liftWithUnlift $ \(MkUnlift unlift1) ->
                    liftWithUnlift $ \(MkUnlift unlift2) ->
                        call $
                        MkUnlift $ \(MkComposeT t1t2ma) ->
                            unlift2 $ withTransConstraintTM @MonadUnliftIO $ unlift1 t1t2ma
    getDiscardingUnlift ::
           forall m. Monad m
        => ComposeT t1 t2 m (Unlift (ComposeT t1 t2))
    getDiscardingUnlift =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                withTransConstraintTM @Monad $ do
                    unlift1 <- getDiscardingUnlift
                    unlift2 <- lift getDiscardingUnlift
                    return $ composeUnlift unlift1 unlift2

instance (MonadTransAskUnlift t1, MonadTransAskUnlift t2) => MonadTransAskUnlift (ComposeT t1 t2) where
    askUnlift ::
           forall m. Monad m
        => ComposeT t1 t2 m (Unlift (ComposeT t1 t2))
    askUnlift =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                withTransConstraintTM @Monad $ do
                    unlift1 <- askUnlift
                    unlift2 <- lift askUnlift
                    return $ composeUnlift unlift1 unlift2
