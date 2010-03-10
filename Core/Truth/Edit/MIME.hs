module Truth.Edit.MIME where
{
    import Truth.Edit.Anything;
    import Truth.Edit.Import;
    import qualified Codec.MIME.Type;

    type MIMEContentType = Codec.MIME.Type.Type;

    instance HasNewValue MIMEContentType where
    {
        newValue = Codec.MIME.Type.Type (Codec.MIME.Type.Application "octet-stream") [];
    };

    instance HasInfoT MIMEContentType where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.Edit.MIME.MIMEContentType"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    data MIMEContent = MkMIMEContent MIMEContentType [Word8];

    instance HasNewValue MIMEContent where
    {
        newValue = MkMIMEContent newValue [];
    };

    instance HasInfoT MIMEContent where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.Edit.MIME.MIMEContent"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance IsTuple MIMEContent where
    {
        type ListTuple MIMEContent = ([Word8],(MIMEContentType,()));
        fromListTuple (content,(t,())) = MkMIMEContent t content;
        toListTuple (MkMIMEContent t content) = (content,(t,()));
    };

    data MIMEKnowledge = MkMIMEKnowledge
    {
        findMIMECodecByMIME :: MIMEContentType -> Maybe (AnyF InfoT (Codec [Word8])),
        findMIMECodecByInfoT :: forall b. InfoT b -> Maybe (MIMEContentType,Codec [Word8] b)
    };

    interpretInjection :: (?mimeKnowledge :: MIMEKnowledge) => Injection MIMEContent (Maybe Anything);
    interpretInjection = MkInjection
    {
        -- injForwards :: MIMEContent -> Maybe Anything
        injForwards = \(MkMIMEContent t content) -> do
        {
            (MkAnyF isubj codec) <- findMIMECodecByMIME ?mimeKnowledge t;
            b <- decode codec content;
            return (MkAnything isubj b);
        },
        -- injBackwards :: Maybe Anything -> Maybe MIMEContent
        injBackwards = \ma -> case ma of
        {
            (Just (MkAnything ia a)) -> do
            {
                (t,codec) <- findMIMECodecByInfoT ?mimeKnowledge ia;
                return (MkMIMEContent t (encode codec a));
            };
            _ -> Nothing;
        }
    };

    type CharsetKnowledge = String -> Maybe (Codec [Word8] String);
}
