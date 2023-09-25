module Language.Expression.Dolan.Solver.Crumble.Crumbler where

import Data.Shim
import Shapes

type Crumbler :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype Crumbler w m f a = MkCrumbler
    { unCrumbler :: forall (rlist :: [Type]). ReaderT (ListType w rlist) m (f (ListProduct rlist -> a))
    }

instance forall w m f. (Functor f, Monad m) => Functor (Crumbler w m f) where
    fmap ab (MkCrumbler ruha) = MkCrumbler $ (fmap $ fmap $ fmap ab) ruha

crumblerPure ::
       forall w m f a. (Applicative f, Monad m)
    => a
    -> Crumbler w m f a
crumblerPure a = MkCrumbler $ pure $ pure $ pure a

runCrumbler ::
       forall w m f a. (Applicative f, Monad m)
    => Crumbler w m f a
    -> m (f a)
runCrumbler (MkCrumbler rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

dropMemos ::
       forall w m f a. (Applicative f, Monad m)
    => Crumbler w m f a
    -> Crumbler w m f a
dropMemos pc = MkCrumbler $ withReaderT (\_ -> NilListType) $ fmap (fmap $ \ua _ -> ua ()) $ unCrumbler pc

addMemo ::
       forall w m f t a. (Applicative f, Monad m)
    => w t
    -> Crumbler w m f a
    -> Crumbler w m f (t -> a)
addMemo wt pc =
    MkCrumbler $ withReaderT (\seen' -> ConsListType wt seen') $ fmap (fmap $ \tla l t -> tla (t, l)) $ unCrumbler pc

memoise ::
       forall w m f t a. (TestEquality w, Applicative f, MonadIO m)
    => (t -> t)
    -> w t
    -> Crumbler w m f (t -> a)
    -> Crumbler w m f (t, a)
    -> Crumbler w m f a
memoise lazify wt call1 call2 =
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

mapEachMemo ::
       forall w m f a. (Applicative f, Monad m)
    => (forall t. w t -> m (ShimWit (->) w t))
    -> Crumbler w m f a
    -> Crumbler w m f a
mapEachMemo ff = let
    mapInside ::
           forall b.
           (forall rlist. ListType w rlist -> m (f (ListProduct rlist -> b)))
        -> (forall rlist. ListType w rlist -> m (f (ListProduct rlist -> b)))
    mapInside sma NilListType = sma NilListType
    mapInside sma (ConsListType w ww) = do
        MkShimWit w' conv <- ff w
        fmap (fmap $ \lta (a, l) -> lta l $ conv a) $
            mapInside (\ww' -> fmap (fmap $ \tla l t -> tla (t, l)) $ sma $ ConsListType w' ww') ww
    in \rma -> MkCrumbler $ ReaderT $ mapInside $ runReaderT $ unCrumbler rma
