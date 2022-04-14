module Control.Monad.Ology.Result where

import Import

data Result e a
    = SuccessResult a
    | FailureResult e

resultToMaybe :: Result e a -> Maybe a
resultToMaybe (SuccessResult a) = Just a
resultToMaybe _ = Nothing

resultToM :: MonadFail m => Result String a -> m a
resultToM (SuccessResult a) = return a
resultToM (FailureResult e) = fail e

resultToEither :: Result e a -> Either e a
resultToEither (FailureResult e) = Left e
resultToEither (SuccessResult a) = Right a

eitherToResult :: Either e a -> Result e a
eitherToResult (Left e) = FailureResult e
eitherToResult (Right a) = SuccessResult a

resultFromMaybe :: e -> Maybe a -> Result e a
resultFromMaybe _ (Just a) = SuccessResult a
resultFromMaybe e Nothing = FailureResult e

maybeToM :: MonadFail m => String -> Maybe a -> m a
maybeToM e = resultToM . resultFromMaybe e

deriving instance (Eq e, Eq a) => Eq (Result e a)

instance Functor (Result e) where
    fmap ab (SuccessResult a) = SuccessResult (ab a)
    fmap _ (FailureResult e) = FailureResult e

instance Foldable (Result e) where
    foldMap am (SuccessResult a) = am a
    foldMap _ (FailureResult _) = mempty

instance Traversable (Result e) where
    traverse afb (SuccessResult a) = fmap SuccessResult (afb a)
    traverse _ (FailureResult e) = pure (FailureResult e)
    sequenceA (SuccessResult fa) = fmap SuccessResult fa
    sequenceA (FailureResult e) = pure (FailureResult e)

instance Applicative (Result e) where
    pure = SuccessResult
    (SuccessResult ab) <*> (SuccessResult a) = SuccessResult (ab a)
    (SuccessResult _) <*> (FailureResult e) = FailureResult e
    (FailureResult e) <*> _ = FailureResult e

instance Monad (Result e) where
    (FailureResult e) >>= _ = FailureResult e
    (SuccessResult a) >>= amq = amq a

instance IsString e => MonadFail (Result e) where
    fail s = FailureResult $ fromString s

instance Monoid e => Alternative (Result e) where
    empty = FailureResult mempty
    (SuccessResult a) <|> _ = SuccessResult a
    (FailureResult _) <|> (SuccessResult a) = SuccessResult a
    (FailureResult e1) <|> (FailureResult e2) = FailureResult $ mappend e1 e2

instance Monoid e => MonadPlus (Result e)

instance MonadFix (Result e) where
    mfix ama = let
        getSuccess (SuccessResult a) = a
        getSuccess (FailureResult _) = error "mfix FailureResult"
        ma = ama $ getSuccess ma
        in ma

instance (Show e, Show a) => Show (Result e a) where
    show (SuccessResult a) = "success: " ++ show a
    show (FailureResult e) = "failure: " ++ show e

mapResultFailure :: (e1 -> e2) -> Result e1 a -> Result e2 a
mapResultFailure _ (SuccessResult a) = SuccessResult a
mapResultFailure e1e2 (FailureResult e1) = FailureResult (e1e2 e1)
