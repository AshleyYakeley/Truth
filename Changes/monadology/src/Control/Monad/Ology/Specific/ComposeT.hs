module Control.Monad.Ology.Specific.ComposeT
    ( ComposeT(..)
    , composeUnlift
    , composeWUnlift
    , lift1ComposeT
    , lift2ComposeT
    , lift2ComposeT'
    , lift2ComposeT''
    , lift1ComposeTWithUnlift
    , lift2ComposeTWithUnlift
    ) where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ComposeInner
import Import

newtype ComposeT (t1 :: (Type -> Type) -> (Type -> Type)) (t2 :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) (a :: Type) = MkComposeT
    { unComposeT :: t1 (t2 m) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO, MonadFix, MonadPlus)

lift1ComposeT ::
       forall t1 t2 m a. (MonadTransTunnel t1, MonadTrans t2, TransConstraint Monad t2, Monad m)
    => t1 m a
    -> ComposeT t1 t2 m a
lift1ComposeT t1ma =
    case hasTransConstraint @Monad @t2 @m of
        Dict -> MkComposeT $ hoist lift t1ma

lift2ComposeT ::
       forall t1 t2 m a. (MonadTrans t1, Monad (t2 m))
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT t2ma = MkComposeT $ lift t2ma

lift2ComposeT' ::
       forall t1 t2 m a. (MonadTrans t1, TransConstraint Monad t2, Monad m)
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT' =
    case hasTransConstraint @Monad @t2 @m of
        Dict -> lift2ComposeT

lift2ComposeT'' ::
       forall t1 t2 m a. (MonadTrans t1, TransConstraint MonadIO t2, MonadIO m)
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT'' =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> lift2ComposeT

lift1ComposeTWithUnlift ::
       forall t1 t2 m r. (MonadTransTunnel t1, MonadTransUnlift t2, MonadTunnelIOInner m)
    => ((forall a. ComposeT t1 t2 m a -> t1 m a) -> t1 m r)
    -> ComposeT t1 t2 m r
lift1ComposeTWithUnlift call =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict ->
            MkComposeT $
            tunnel $ \tun -> liftWithUnlift $ \unlift -> tun $ call $ \(MkComposeT ttma) -> hoist unlift ttma

lift2ComposeTWithUnlift ::
       forall t1 t2 m r. (MonadTransUnlift t1, MonadTransUnlift t2, MonadTunnelIOInner m)
    => ((forall a. ComposeT t1 t2 m a -> t2 m a) -> t2 m r)
    -> ComposeT t1 t2 m r
lift2ComposeTWithUnlift call =
    case hasTransConstraint @MonadTunnelIOInner @t2 @m of
        Dict -> MkComposeT $ liftWithUnlift $ \unlift -> call $ \(MkComposeT ttma) -> unlift ttma

composeUnlift ::
       forall c ta tb. (MonadTransUnlift tb, TransConstraint c tb)
    => Unlift c ta
    -> Unlift c tb
    -> Unlift c (ComposeT ta tb)
composeUnlift ua ub (MkComposeT tatbma) = ub $ withTransConstraintTM @c $ ua tatbma

composeWUnlift ::
       forall c ta tb. (MonadTransUnlift tb, TransConstraint c tb)
    => WUnlift c ta
    -> WUnlift c tb
    -> WUnlift c (ComposeT ta tb)
composeWUnlift (MkWUnlift ua) (MkWUnlift ub) = MkWUnlift $ composeUnlift @c ua ub

instance (MonadTrans t1, MonadTrans t2, TransConstraint Monad t2) => MonadTrans (ComposeT t1 t2) where
    lift (ma :: m a) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ lift $ lift ma

instance (TransConstraint Functor t1, TransConstraint Functor t2) => TransConstraint Functor (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. Functor m
        => Dict (Functor (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @Functor @t2 @m of
            Dict ->
                case hasTransConstraint @Functor @t1 @(t2 m) of
                    Dict -> Dict

instance (TransConstraint Applicative t1, TransConstraint Applicative t2) =>
             TransConstraint Applicative (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. Applicative m
        => Dict (Applicative (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @Applicative @t2 @m of
            Dict ->
                case hasTransConstraint @Applicative @t1 @(t2 m) of
                    Dict -> Dict

instance (TransConstraint Monad t1, TransConstraint Monad t2) => TransConstraint Monad (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. Monad m
        => Dict (Monad (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                case hasTransConstraint @Monad @t1 @(t2 m) of
                    Dict -> Dict

instance (TransConstraint MonadIO t1, TransConstraint Monad t2, TransConstraint MonadIO t2) =>
             TransConstraint MonadIO (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadIO m
        => Dict (MonadIO (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t2 @m of
            Dict ->
                case hasTransConstraint @MonadIO @t1 @(t2 m) of
                    Dict -> Dict

instance (TransConstraint MonadFail t1, TransConstraint Monad t2, TransConstraint MonadFail t2) =>
             TransConstraint MonadFail (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadFail m
        => Dict (MonadFail (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadFail @t2 @m of
            Dict ->
                case hasTransConstraint @MonadFail @t1 @(t2 m) of
                    Dict -> Dict

instance (TransConstraint MonadFix t1, TransConstraint Monad t2, TransConstraint MonadFix t2) =>
             TransConstraint MonadFix (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadFix m
        => Dict (MonadFix (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadFix @t2 @m of
            Dict ->
                case hasTransConstraint @MonadFix @t1 @(t2 m) of
                    Dict -> Dict

instance (TransConstraint MonadPlus t1, TransConstraint Monad t2, TransConstraint MonadPlus t2) =>
             TransConstraint MonadPlus (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadPlus m
        => Dict (MonadPlus (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadPlus @t2 @m of
            Dict ->
                case hasTransConstraint @MonadPlus @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransHoist t1, MonadTransHoist t2) => MonadTransHoist (ComposeT t1 t2) where
    hoist ::
           forall m1 m2. (Monad m1, Monad m2)
        => (m1 --> m2)
        -> ComposeT t1 t2 m1 --> ComposeT t1 t2 m2
    hoist f (MkComposeT ma) =
        case hasTransConstraint @Monad @t2 @m1 of
            Dict ->
                case hasTransConstraint @Monad @t2 @m2 of
                    Dict -> MkComposeT $ hoist (hoist f) ma

instance (MonadTransTunnel t1, MonadTransTunnel t2) => MonadTransTunnel (ComposeT t1 t2) where
    type Tunnel (ComposeT t1 t2) = ComposeInner (Tunnel t1) (Tunnel t2)
    tunnel ::
           forall m2 r. Monad m2
        => ((forall m1 a. Monad m1 => ComposeT t1 t2 m1 a -> m1 (ComposeInner (Tunnel t1) (Tunnel t2) a)) -> m2 (ComposeInner (Tunnel t1) (Tunnel t2) r))
        -> ComposeT t1 t2 m2 r
    tunnel call =
        case hasTransConstraint @Monad @t2 @m2 of
            Dict ->
                MkComposeT $
                tunnel $ \unlift1 ->
                    tunnel $ \unlift2 ->
                        fmap getComposeInner $
                        call $ \(MkComposeT ff :: _ m1 _) ->
                            case hasTransConstraint @Monad @t2 @m1 of
                                Dict -> fmap MkComposeInner $ unlift2 $ unlift1 $ ff

instance (MonadTransCoerce t1, MonadTransCoerce t2, TransConstraint Monad t2) => MonadTransCoerce (ComposeT t1 t2) where
    transCoerce ::
           forall m1 m2. Coercible m1 m2
        => Dict (Coercible (ComposeT t1 t2 m1) (ComposeT t1 t2 m2))
    transCoerce =
        case transCoerce @t2 @m1 @m2 of
            Dict ->
                case transCoerce @t1 @(t2 m1) @(t2 m2) of
                    Dict -> Dict

instance (MonadTransUnlift t1, MonadTransUnlift t2) => MonadTransUnlift (ComposeT t1 t2) where
    liftWithUnlift ::
           forall m r. MonadIO m
        => (Unlift MonadTunnelIOInner (ComposeT t1 t2) -> m r)
        -> ComposeT t1 t2 m r
    liftWithUnlift call =
        case hasTransConstraint @MonadIO @t2 @m of
            Dict ->
                MkComposeT $
                liftWithUnlift $ \unlift1 ->
                    liftWithUnlift $ \unlift2 ->
                        call $ \(MkComposeT t1t2ma) ->
                            unlift2 $ withTransConstraintTM @MonadTunnelIOInner $ unlift1 t1t2ma
    getDiscardingUnlift ::
           forall m. Monad m
        => ComposeT t1 t2 m (WUnlift MonadTunnelIOInner (ComposeT t1 t2))
    getDiscardingUnlift =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                withTransConstraintTM @Monad $ do
                    unlift1 <- getDiscardingUnlift
                    unlift2 <- lift getDiscardingUnlift
                    return $ composeWUnlift unlift1 unlift2

instance (MonadTransAskUnlift t1, MonadTransAskUnlift t2) => MonadTransAskUnlift (ComposeT t1 t2) where
    askUnlift ::
           forall m. Monad m
        => ComposeT t1 t2 m (WUnlift Monad (ComposeT t1 t2))
    askUnlift =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                withTransConstraintTM @Monad $ do
                    unlift1 <- askUnlift
                    unlift2 <- lift askUnlift
                    return $ composeWUnlift unlift1 unlift2
