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

    instance HasInfo (Type_T MIMEContentType) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T MIMEContentType |])
        [
            mkFacts0 (return HasNewValue_Inst),
            mkFacts0 (return Eq_Inst)
        ];
    };

    data MIMEContent = MkMIMEContent MIMEContentType [Word8];

    instance HasNewValue MIMEContent where
    {
        newValue = MkMIMEContent newValue [];
    };

    instance HasInfo (Type_T MIMEContent) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T MIMEContent |])
        [
            mkFacts0 (return HasNewValue_Inst)
            -- ,mkFacts0 (return Eq_Inst)
        ];
    };

    instance IsTuple MIMEContent where
    {
        type ListTuple MIMEContent = ([Word8],(MIMEContentType,()));
        fromListTuple (content,(t,())) = MkMIMEContent t content;
        toListTuple (MkMIMEContent t content) = (content,(t,()));
    };

{-
    data Anything where
    {
        MkAnything :: forall a. Info (Type_T a) -> a -> Anything;
    };
-}

    data AnyCodec where
    {
        MkAnyCodec :: forall a. Info (Type_T a) -> Codec [Word8] a -> AnyCodec;
    };

    data MIMEKnowledge = MkMIMEKnowledge
    {
        findMIMECodecByMIME :: MIMEContentType -> Maybe AnyCodec,
        findMIMECodecByInfoT :: forall b. Info (Type_T b) -> Maybe (MIMEContentType,Codec [Word8] b)
    };

    interpretInjection :: (?mimeKnowledge :: MIMEKnowledge) => Injection MIMEContent (Maybe Anything);
    interpretInjection = MkInjection
    {
        -- injForwards :: MIMEContent -> Maybe Anything
        injForwards = \(MkMIMEContent t content) -> do
        {
            (MkAnyCodec isubj codec) <- findMIMECodecByMIME ?mimeKnowledge t;
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
