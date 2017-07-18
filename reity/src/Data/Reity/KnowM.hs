module Data.Reity.KnowM where
{
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

    type KnowM = Result [FailureReason];

    kmError :: String -> KnowM a;
    kmError s = FailureResult $ [MkFailureReason s []];

    kmContext :: String -> KnowM a -> KnowM a;
    kmContext _ (SuccessResult a) = SuccessResult a;
    kmContext s (FailureResult ee) = FailureResult [MkFailureReason s ee];
}
