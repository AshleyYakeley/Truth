module Data.Reity.ReasonM where
{
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.Reader;
    import Data.Result;


    data FailureReason = MkFailureReason String [FailureReason];

    frShow :: Int -> FailureReason -> String;
    frShow n (MkFailureReason s frs) = replicate (n * 4) ' ' ++ s ++ "\n" ++ frShowList (n + 1) frs;

    frShowList :: Int -> [FailureReason] -> String;
    frShowList n frs = mconcat $ fmap (frShow n) frs;

    instance Show FailureReason where
    {
        show = frShow 0;
        showList frs = showString $ frShowList 0 frs;
    };

    type ReasonM = Result [FailureReason];

    kmNullError :: ReaderT k ReasonM a;
    kmNullError = lift $ FailureResult [];

    kmError :: String -> ReaderT k ReasonM a;
    kmError s = lift $ FailureResult [MkFailureReason s []];

    rmContext :: String -> ReasonM a -> ReasonM a;
    rmContext _ (SuccessResult a) = SuccessResult a;
    rmContext s (FailureResult ee) = FailureResult [MkFailureReason s ee];

    kmContext :: String -> ReaderT k ReasonM a -> ReaderT k ReasonM a;
    kmContext s (ReaderT ra) = ReaderT $ \k -> rmContext s $ ra k;

    kmCatch :: ReaderT k ReasonM a -> ([FailureReason] -> ReaderT k ReasonM a) -> ReaderT k ReasonM a;
    kmCatch (ReaderT kma) c = ReaderT $ \k -> case kma k of
    {
        SuccessResult a -> return a;
        FailureResult frs -> runReaderT (c frs) k;
    };
}
