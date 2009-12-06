module Data.Result where
{
    import Data.Bijection;
    import Control.Monad;
    import Data.Traversable;
    import Control.Applicative;
    import Data.Foldable;
    import Data.Monoid;

    data Result e a = SuccessResult a | FailureResult e;
    
    resultToMaybe :: Result e a -> Maybe a;
    resultToMaybe (SuccessResult a) = Just a;
    resultToMaybe _ = Nothing;
    
    instance Functor (Result e) where
    {
        fmap ab (SuccessResult a) = SuccessResult (ab a);
        fmap _ (FailureResult e) = FailureResult e;
    };

    instance Foldable (Result e) where
    {
        foldMap am (SuccessResult a) = am a; 
        foldMap _ (FailureResult _) = mempty; 
    };

    instance Traversable (Result e) where
    {
        traverse afb (SuccessResult a) = fmap SuccessResult (afb a);
        traverse _ (FailureResult e) = pure (FailureResult e);
        sequenceA (SuccessResult fa) = fmap SuccessResult fa;
        sequenceA (FailureResult e) = pure (FailureResult e);
    };

    instance Applicative (Result e) where
    {
        pure = SuccessResult;
        (SuccessResult ab) <*> (SuccessResult a) = SuccessResult (ab a);
        (SuccessResult _) <*> (FailureResult e) = FailureResult e;
        (FailureResult e) <*> _ = FailureResult e;
    };

    instance Monad (Result e) where
    {
        return = SuccessResult;
        (FailureResult e) >>= _ = FailureResult e;
        (SuccessResult a) >>= amq = amq a;
        fail _ = FailureResult undefined;
    };
    
    mapResult :: Bijection (Result e2 (Result e1 a)) (Result (Either e2 e1) a);
    mapResult = MkBijection forwards backwards where
    {
        forwards (SuccessResult (SuccessResult a)) = SuccessResult a;
        forwards (SuccessResult (FailureResult e1)) = FailureResult (Right e1);
        forwards (FailureResult e2) = FailureResult (Left e2);
        
        backwards (SuccessResult a) = SuccessResult (SuccessResult a);
        backwards (FailureResult (Right e1)) = SuccessResult (FailureResult e1);
        backwards (FailureResult (Left e2)) = FailureResult e2;
    };
}
