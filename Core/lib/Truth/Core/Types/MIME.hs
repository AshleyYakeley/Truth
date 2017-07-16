{-# OPTIONS -fno-warn-orphans #-}
module Truth.Core.Types.MIME where
{
    import Truth.Core.Import;
    import qualified Codec.MIME.Type;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Anything;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.Context;


    type MIMEContentType = Codec.MIME.Type.Type;

    instance HasNewValue MIMEContentType where
    {
        newValue = Codec.MIME.Type.Type (Codec.MIME.Type.Application $ fromString "octet-stream") [];
    };

    $(return []);
    instance HasInfo MIMEContentType where
    {
        info = mkSimpleInfo $(ionamedwitness[t|MIMEContentType|]) [$(declInfo [d|
            instance Eq MIMEContentType;
            instance HasNewValue MIMEContentType;
        |])];
    };

    type MIMEContent = WithContext MIMEContentType [Word8];
    type MIMETuple edit = WithContextSelector (WholeEdit MIMEContentType) edit;
    type MIMEContentEdit edit = TupleEdit (MIMETuple edit);

    data AnyCodec where
    {
        MkAnyCodec :: forall (edit :: *). Info edit -> Info (EditReader edit) -> Info (EditSubject edit) -> Codec [Word8] (EditSubject edit) -> AnyCodec;
    };

    data MIMEKnowledge = MkMIMEKnowledge
    {
        findMIMECodecByMIME :: MIMEContentType -> Maybe AnyCodec,
        findMIMECodecByInfoT :: forall (edit :: *). Info edit -> Maybe (MIMEContentType,Codec [Word8] (EditSubject edit))
    };

    interpretInjection :: (?mimeKnowledge :: MIMEKnowledge) => Injection MIMEContent (Maybe Anything);
    interpretInjection = MkInjection
    {
        -- injForwards :: MIMEContent -> Maybe Anything
        injForwards = \(MkWithContext t content) -> do
        {
            (MkAnyCodec ie ir isubj codec) <- findMIMECodecByMIME ?mimeKnowledge t;
            b <- decode codec content;
            return (MkAnything ie ir isubj b);
        },
        -- injBackwards :: Maybe Anything -> Maybe MIMEContent
        injBackwards = \ma -> case ma of
        {
            (Just (MkAnything ie _ _ a)) -> do
            {
                (t,codec) <- findMIMECodecByInfoT ?mimeKnowledge ie;
                return (MkWithContext t (encode codec a));
            };
            _ -> Nothing;
        }
    };

    type CharsetKnowledge = String -> Maybe (Codec [Word8] String);
}
