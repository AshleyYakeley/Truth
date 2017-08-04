module Data.Reity.ReasonM where
{
    import Data.String;
    import Data.Semigroup;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.Reader;
    import Data.Result;


    data FailureReason = MkFailureReason String FailureReasons deriving (Eq);

    newtype FailureReasons = MkFailureReasons [FailureReason] deriving (Eq,Semigroup,Monoid); -- need this wrapper for IsString instance

    instance IsString FailureReason where
    {
        fromString s = MkFailureReason s mempty;
    };

    instance IsString FailureReasons where
    {
        fromString s = MkFailureReasons [fromString s];
    };

    frShow :: Int -> FailureReason -> String;
    frShow n (MkFailureReason s frs) = replicate (n * 4) ' ' ++ s ++ "\n" ++ frShowList (n + 1) frs;

    frShowList :: Int -> FailureReasons -> String;
    frShowList n (MkFailureReasons frs) = mconcat $ fmap (frShow n) frs;

    instance Show FailureReason where
    {
        show = frShow 0;
        showList frs = showString $ show $ MkFailureReasons frs;
    };

    instance Show FailureReasons where
    {
        show frs = frShowList 0 frs;
    };

    type ReasonM = Result FailureReasons;

    kmNullError :: ReaderT k ReasonM a;
    kmNullError = lift $ FailureResult mempty;

    rmContext :: String -> ReasonM a -> ReasonM a;
    rmContext _ (SuccessResult a) = SuccessResult a;
    rmContext s (FailureResult ee) = FailureResult $ MkFailureReasons [MkFailureReason s  ee];

    kmContext :: String -> ReaderT k ReasonM a -> ReaderT k ReasonM a;
    kmContext s (ReaderT ra) = ReaderT $ \k -> rmContext s $ ra k;

    kmCatch :: ReaderT k ReasonM a -> (FailureReasons -> ReaderT k ReasonM a) -> ReaderT k ReasonM a;
    kmCatch (ReaderT kma) c = ReaderT $ \k -> case kma k of
    {
        SuccessResult a -> return a;
        FailureResult frs -> runReaderT (c frs) k;
    };
}
