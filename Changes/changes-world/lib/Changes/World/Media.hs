module Changes.World.Media
    ( module Changes.World.Media.Type
    , module Changes.World.Media
    )
where

import Changes.Core
import Shapes

import Changes.World.Anything
import Changes.World.Media.Type

type Media = WithContext MediaType StrictByteString

type MediaTuple edit = WithContextSelector (WholeEdit MediaType) edit

type MediaEdit edit = TupleUpdateEdit (MediaTuple edit)

data AnyCodec where
    MkAnyCodec :: forall (edit :: Type). IOWitness edit -> Codec StrictByteString (EditSubject edit) -> AnyCodec

data MediaKnowledge = MkMediaKnowledge
    { findMediaCodecByType :: MediaType -> Maybe AnyCodec
    , findMediaCodecByWitness ::
        forall (edit :: Type).
        IOWitness edit -> Maybe (MediaType, Codec StrictByteString (EditSubject edit))
    }

interpretInjection :: (?mediaKnowledge :: MediaKnowledge) => Injection Media (Maybe Anything)
interpretInjection = let
    injForwards :: Media -> Maybe Anything
    injForwards =
        \(MkWithContext t content) -> do
            MkAnyCodec ie codec <- findMediaCodecByType ?mediaKnowledge t
            b <- decode codec content
            return $ MkAnything ie b
    injBackwards :: Maybe Anything -> Maybe Media
    injBackwards =
        \ma ->
            case ma of
                (Just (MkAnything ie a)) -> do
                    (t, codec) <- findMediaCodecByWitness ?mediaKnowledge ie
                    return (MkWithContext t (encode codec a))
                _ -> Nothing
    in MkInjection{..}
