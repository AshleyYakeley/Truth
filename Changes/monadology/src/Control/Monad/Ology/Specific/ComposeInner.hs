module Control.Monad.Ology.Specific.ComposeInner where

import Control.Monad.Ology.General.Exception.Class
import Control.Monad.Ology.General.Extract
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Identity
import Control.Monad.Ology.General.Inner
import Control.Monad.Ology.General.Outer
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.Specific.Result
import Import

type ComposeInner :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype ComposeInner inner outer a = MkComposeInner
    { getComposeInner :: outer (inner a)
    }

instance (Foldable inner, Foldable outer, Functor outer) => Foldable (ComposeInner inner outer) where
    foldMap am (MkComposeInner oia) = foldMap id $ fmap (foldMap am) oia

instance (Traversable inner, Traversable outer) => Traversable (ComposeInner inner outer) where
    traverse afb (MkComposeInner oia) = fmap MkComposeInner $ traverse (traverse afb) oia

instance Traversable inner => TransConstraint Traversable (ComposeInner inner) where
    hasTransConstraint = Dict

instance (Functor inner, Functor outer) => Functor (ComposeInner inner outer) where
    fmap ab (MkComposeInner oia) = MkComposeInner $ fmap (fmap ab) oia

instance Functor inner => TransConstraint Functor (ComposeInner inner) where
    hasTransConstraint = Dict

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
                FailureResult e -> return $ throwExc e

instance MonadInner inner => TransConstraint Monad (ComposeInner inner) where
    hasTransConstraint = Dict

instance (MonadInner inner, MonadInner outer) => MonadInner (ComposeInner inner outer) where
    retrieveInner (MkComposeInner oia) =
        case retrieveInner oia of
            SuccessResult ia ->
                case retrieveInner ia of
                    SuccessResult a -> SuccessResult a
                    FailureResult e -> FailureResult $ Left e
            FailureResult e -> FailureResult $ Right e

instance (MonadInner inner, MonadOuter inner, MonadOuter outer) => MonadOuter (ComposeInner inner outer) where
    getExtract =
        MkComposeInner $ do
            MkExtract oaa <- getExtract
            return $ do
                MkExtract iaa <- getExtract
                return $ MkExtract $ \(MkComposeInner oia) -> iaa $ oaa oia

instance (MonadInner inner, MonadFix outer) => MonadFix (ComposeInner inner outer) where
    mfix ama =
        MkComposeInner $
        mfix $ \ia ->
            getComposeInner $
            ama $
            case retrieveInner ia of
                SuccessResult a -> a
                FailureResult _ -> error "bad ComposeInner mfix"

instance MonadInner inner => TransConstraint MonadFix (ComposeInner inner) where
    hasTransConstraint = Dict

instance (MonadInner inner, Monad outer, Alternative inner) => MonadPlus (ComposeInner inner outer)

instance (MonadExtract inner, MonadExtract outer) => MonadExtract (ComposeInner inner outer) where
    mToValue (MkComposeInner oia) = mToValue $ mToValue oia

instance MonadExtract inner => TransConstraint MonadExtract (ComposeInner inner) where
    hasTransConstraint = Dict

instance (MonadIdentity inner, MonadIdentity outer) => MonadIdentity (ComposeInner inner outer)

instance MonadIdentity inner => TransConstraint MonadIdentity (ComposeInner inner) where
    hasTransConstraint = Dict

instance (MonadInner inner, MonadIO outer) => MonadIO (ComposeInner inner outer) where
    liftIO ioa = lift $ liftIO ioa

instance MonadInner inner => TransConstraint MonadIO (ComposeInner inner) where
    hasTransConstraint = Dict

liftInner :: Applicative outer => inner a -> ComposeInner inner outer a
liftInner na = MkComposeInner $ pure na

instance MonadInner inner => MonadTrans (ComposeInner inner) where
    lift ma = MkComposeInner $ fmap pure ma

instance (MonadInner inner, MonadException inner, MonadException m) => MonadException (ComposeInner inner m) where
    type Exc (ComposeInner inner m) = Either (Exc inner) (Exc m)
    throwExc (Left e) = liftInner $ throwExc e
    throwExc (Right e) = lift $ throwExc e
    catchExc (MkComposeInner mia) handler =
        MkComposeInner $ do
            ira <- tryExc mia
            case fmap retrieveInner ira of
                FailureResult e -> getComposeInner $ handler $ Right e
                SuccessResult (FailureResult e) -> getComposeInner $ handler $ Left e
                SuccessResult (SuccessResult a) -> return $ return a
