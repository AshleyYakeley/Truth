module Control.Monad.Ology.ComposeInner where

import Control.Monad.Ology.MonadExtract
import Control.Monad.Ology.MonadIdentity
import Control.Monad.Ology.MonadInner
import Control.Monad.Ology.MonadOuter
import Control.Monad.Ology.Result
import Control.Monad.Ology.Trans.Constraint
import Import

type ComposeInner :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype ComposeInner inner outer a = MkComposeInner
    { getComposeInner :: outer (inner a)
    }

instance (Foldable inner, Foldable outer, Functor outer) => Foldable (ComposeInner inner outer) where
    foldMap am (MkComposeInner oia) = foldMap id $ fmap (foldMap am) oia

instance (Traversable inner, Traversable outer) => Traversable (ComposeInner inner outer) where
    traverse afb (MkComposeInner oia) = fmap MkComposeInner $ traverse (traverse afb) oia

instance (Functor inner, Functor outer) => Functor (ComposeInner inner outer) where
    fmap ab (MkComposeInner oia) = MkComposeInner $ fmap (fmap ab) oia

instance (MonadInner inner, Monad outer) => Applicative (ComposeInner inner outer) where
    pure a = MkComposeInner $ pure $ pure a
    -- cannot use obvious definition for <*>, because that would incorrectly execute the outer part of ma even if mab fails
    mab <*> ma = do
        ab <- mab
        a <- ma
        return $ ab a

instance (MonadInner inner, Monad outer, Alternative inner) => Alternative (ComposeInner inner outer) where
    empty = MkComposeInner $ pure empty
    -- cannot use obvious definition for <|> for similar reasons as in <*>
    (MkComposeInner oia) <|> cb = do
        ma <-
            MkComposeInner $ do
                ia <- oia
                return $ fmap Just ia <|> return Nothing
        case ma of
            Just a -> return a
            Nothing -> cb

instance (MonadInner inner, Monad outer) => Monad (ComposeInner inner outer) where
    return = pure
    (MkComposeInner oia) >>= p =
        MkComposeInner $ do
            ia <- oia
            case retrieveInner ia of
                SuccessResult a -> do
                    ib <- getComposeInner $ p a
                    return $ ia >> ib
                FailureResult ix -> return $ fmap absurd ix

instance (MonadInner inner, MonadInner outer) => MonadInner (ComposeInner inner outer) where
    retrieveInner (MkComposeInner oia) =
        case retrieveInner oia of
            SuccessResult ia ->
                case retrieveInner ia of
                    SuccessResult a -> SuccessResult a
                    FailureResult iv -> FailureResult $ MkComposeInner $ pure iv
            FailureResult ov -> FailureResult $ MkComposeInner $ fmap pure ov

instance (MonadInner inner, MonadOuter inner, MonadOuter outer) => MonadOuter (ComposeInner inner outer) where
    outerCommute fcioa = MkComposeInner $ fmap outerCommute $ outerCommute $ fmap getComposeInner fcioa

instance (MonadInner inner, MonadFix outer) => MonadFix (ComposeInner inner outer) where
    mfix ama =
        MkComposeInner $
        mfix $ \ia ->
            getComposeInner $
            ama $
            case retrieveInner ia of
                SuccessResult a -> a
                FailureResult _ -> error "bad ComposeInner mfix"

instance (MonadInner inner, Monad outer, Alternative inner) => MonadPlus (ComposeInner inner outer)

instance (MonadExtract inner, MonadExtract outer) => MonadExtract (ComposeInner inner outer) where
    mToValue (MkComposeInner oia) = mToValue $ mToValue oia

instance (MonadIdentity inner, MonadIdentity outer) => MonadIdentity (ComposeInner inner outer)

liftOuter :: (Functor outer, Applicative inner) => outer a -> ComposeInner inner outer a
liftOuter ma = MkComposeInner $ fmap pure ma

liftInner :: Applicative outer => inner a -> ComposeInner inner outer a
liftInner na = MkComposeInner $ pure na

instance (MonadInner inner, MonadIO outer) => MonadIO (ComposeInner inner outer) where
    liftIO ioa = liftOuter $ liftIO ioa

instance MonadInner inner => MonadTrans (ComposeInner inner) where
    lift = liftOuter

instance MonadInner inner => TransConstraint Functor (ComposeInner inner) where
    hasTransConstraint = Dict

instance MonadInner inner => TransConstraint Monad (ComposeInner inner) where
    hasTransConstraint = Dict

instance MonadInner inner => TransConstraint MonadIO (ComposeInner inner) where
    hasTransConstraint = Dict

instance MonadInner inner => TransConstraint MonadFix (ComposeInner inner) where
    hasTransConstraint = Dict
