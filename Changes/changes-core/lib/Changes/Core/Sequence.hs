module Changes.Core.Sequence where

import Changes.Core.Import

newtype SequencePoint = MkSequencePoint
    { unSequencePoint :: Int64
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show SequencePoint where
    show (MkSequencePoint i) = show i

seqLength :: IsSequence seq => seq -> SequencePoint
seqLength = fromIntegral . olength64

seqIndex :: IsSequence seq => seq -> SequencePoint -> Maybe (Element seq)
seqIndex sq i = index sq $ fromIntegral i

seqTake :: IsSequence seq => SequencePoint -> seq -> seq
seqTake p = take $ fromIntegral p

seqDrop :: IsSequence seq => SequencePoint -> seq -> seq
seqDrop p = drop $ fromIntegral p

seqSplitAt :: IsSequence seq => SequencePoint -> seq -> (seq, seq)
seqSplitAt p = splitAt $ fromIntegral p

data SequenceRun = MkSequenceRun
    { runStart :: SequencePoint
    , runLength :: SequencePoint
    } deriving (Eq)

instance Show SequenceRun where
    show (MkSequenceRun start len) = show start ++ "+" ++ show len

runEnd :: SequenceRun -> SequencePoint
runEnd MkSequenceRun {..} = runStart + runLength

startEndRun :: SequencePoint -> SequencePoint -> SequenceRun
startEndRun start end = MkSequenceRun start (end - start)

relativeRun :: SequencePoint -> SequenceRun -> SequenceRun
relativeRun n (MkSequenceRun start len) = MkSequenceRun (start - n) len

goodRun :: SequenceRun -> Bool
goodRun run = runStart run >= 0 && runLength run >= 0

positiveRun :: SequenceRun -> Bool
positiveRun run = runStart run >= 0 && runLength run > 0

clipPoint :: SequencePoint -> SequencePoint -> SequencePoint
clipPoint len p =
    if p < 0
        then 0
        else if p > len
                 then len
                 else p

clipRunBounds :: SequencePoint -> SequenceRun -> SequenceRun
clipRunBounds len (MkSequenceRun rstart rlen) = let
    rend = rstart + rlen
    in startEndRun (clipPoint len rstart) (clipPoint len rend)

clipRunStart :: SequencePoint -> SequenceRun -> SequenceRun
clipRunStart nstart (MkSequenceRun start len) = let
    end = start + len
    in startEndRun (max start nstart) end

clipRunEnd :: SequencePoint -> SequenceRun -> SequenceRun
clipRunEnd nend (MkSequenceRun start len) = let
    end = start + len
    in startEndRun start (min end nend)

clipWithin :: SequenceRun -> SequenceRun -> SequenceRun
clipWithin constraint run = clipRunEnd (runEnd constraint) $ clipRunStart (runStart constraint) run

seqSection :: IsSequence seq => SequenceRun -> seq -> seq
seqSection (MkSequenceRun start len) s = seqTake len $ seqDrop (max start 0) s

seqIntersect :: SequenceRun -> SequenceRun -> Maybe SequenceRun
seqIntersect a b = let
    ab = clipWithin a b
    in if goodRun ab
           then Just ab
           else Nothing

seqIntersectInside :: SequenceRun -> SequenceRun -> Maybe SequenceRun
seqIntersectInside a b = let
    ab = seqIntersect a b
    in if runStart a <= runEnd b && runStart b <= runEnd a
           then ab
           else Nothing
