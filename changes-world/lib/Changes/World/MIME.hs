{-# OPTIONS -fno-warn-orphans #-}

module Changes.World.MIME where

import Changes.Core
import Changes.World.Anything
import qualified Codec.MIME.Type
import Shapes

type MIMEContentType = Codec.MIME.Type.Type

instance HasNewValue MIMEContentType where
    newValue = Codec.MIME.Type.Type (Codec.MIME.Type.Application $ fromString "octet-stream") []

type MIMEContent = WithContext MIMEContentType [Word8]

type MIMETuple edit = WithContextSelector (WholeEdit MIMEContentType) edit

type MIMEContentEdit edit = TupleUpdateEdit (MIMETuple edit)

data AnyCodec where
    MkAnyCodec :: forall (edit :: Type). IOWitness edit -> Codec [Word8] (EditSubject edit) -> AnyCodec

data MIMEKnowledge = MkMIMEKnowledge
    { findMIMECodecByMIME :: MIMEContentType -> Maybe AnyCodec
    , findMIMECodecByInfoT :: forall (edit :: Type).
                                      IOWitness edit -> Maybe (MIMEContentType, Codec [Word8] (EditSubject edit))
    }

interpretInjection :: (?mimeKnowledge :: MIMEKnowledge) => Injection MIMEContent (Maybe Anything)
interpretInjection = let
    injForwards :: MIMEContent -> Maybe Anything
    injForwards =
        \(MkWithContext t content) -> do
            MkAnyCodec ie codec <- findMIMECodecByMIME ?mimeKnowledge t
            b <- decode codec content
            return $ MkAnything ie b
    injBackwards :: Maybe Anything -> Maybe MIMEContent
    injBackwards =
        \ma ->
            case ma of
                (Just (MkAnything ie a)) -> do
                    (t, codec) <- findMIMECodecByInfoT ?mimeKnowledge ie
                    return (MkWithContext t (encode codec a))
                _ -> Nothing
    in MkInjection {..}
