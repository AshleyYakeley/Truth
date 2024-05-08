module Language.Expression.Dolan.Solver.Crumble.Crumbler where

import Shapes

type Crumbler :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype Crumbler w m f a = MkCrumbler
    { unCrumbler :: forall (rlist :: [Type]). ReaderT (ListType w rlist) m (f (ListProduct rlist -> a))
    }

instance forall w m f. (Functor f, Functor m) => Functor (Crumbler w m f) where
    fmap ab (MkCrumbler ruha) = MkCrumbler $ (fmap $ fmap $ fmap ab) ruha

instance forall w m f. (Applicative f, Applicative m) => Applicative (Crumbler w m f) where
    pure a = MkCrumbler $ pure $ pure $ pure a
    liftA2 f (MkCrumbler rma) (MkCrumbler rmb) = MkCrumbler $ liftA2 (liftA2 $ liftA2 f) rma rmb

instance forall w m f. (Applicative f, Monad m) => WrappedApplicative (Crumbler w m f) where
    type WAInnerM (Crumbler w m f) = m
    wexec msa =
        MkCrumbler $ do
            sa <- lift msa
            unCrumbler sa
    whoist f (MkCrumbler rmfa) = MkCrumbler $ hoist f rmfa

crumblerLift ::
       forall w m f a. (Applicative f, Monad m)
    => f a
    -> Crumbler w m f a
crumblerLift fa = MkCrumbler $ return $ fmap pure fa

runCrumbler ::
       forall w m f a. (Applicative f, Monad m)
    => Crumbler w m f a
    -> m (f a)
runCrumbler (MkCrumbler rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

addMemo ::
       forall w m f t a. (Applicative f, Monad m)
    => w t
    -> Crumbler w m f a
    -> Crumbler w m f (t -> a)
addMemo wt pc =
    MkCrumbler $ withReaderT (\seen' -> ConsListType wt seen') $ fmap (fmap $ \tla l t -> tla (t, l)) $ unCrumbler pc

memoiseBranch ::
       forall w m f t a. (TestEquality w, Applicative f, Monad m)
    => (t -> t)
    -> w t
    -> Crumbler w m f (t -> a)
    -> Crumbler w m f (t, a)
    -> Crumbler w m f a
memoiseBranch lazify wt call1 call2 =
    MkCrumbler $ do
        seen <- ask
        case lookUpListElement wt seen of
            Just lelem -> fmap (fmap $ \lta -> lta <*> listProductGetElement lelem) $ unCrumbler call1
            Nothing -> let
                fixconv :: (t -> (t, a)) -> a
                fixconv f = let
                    ~(t, a) = f $ lazify t
                    in a
                in unCrumbler $ fmap fixconv $ addMemo wt call2
