module Truth.Edit.Sequence where
{
    import Truth.Edit.Import;


    newtype SequencePoint seq = MkSequencePoint (Index seq);
    deriving instance Eq (Index seq) => Eq (SequencePoint seq);
    deriving instance Ord (Index seq) => Ord (SequencePoint seq);
    deriving instance Num (Index seq) => Num (SequencePoint seq);

    seqLength :: IsSequence seq => seq -> SequencePoint seq;
    seqLength = fromIntegral . olength64;

    seqTake :: IsSequence seq => SequencePoint seq -> seq -> seq;
    seqTake (MkSequencePoint p) = take p;

    seqDrop :: IsSequence seq => SequencePoint seq -> seq -> seq;
    seqDrop (MkSequencePoint p) = drop p;

    data SequenceRun seq = MkSequenceRun
    {
        runStart :: SequencePoint seq,
        runLength :: SequencePoint seq
    };
    deriving instance Eq (Index seq) => Eq (SequenceRun seq);
    runEnd :: Integral (Index seq) => SequenceRun seq -> SequencePoint seq;
    runEnd MkSequenceRun{..} = runStart + runLength;

    startEndRun :: Integral (Index seq) => SequencePoint seq -> SequencePoint seq -> SequenceRun seq;
    startEndRun start end = MkSequenceRun start (end - start);

    relativeRun :: Integral (Index seq) => SequencePoint seq -> SequenceRun seq -> SequenceRun seq;
    relativeRun n (MkSequenceRun start len) = MkSequenceRun (start - n) len;

    positiveRun :: Integral (Index seq) => SequenceRun seq -> Bool;
    positiveRun (MkSequenceRun _ len) = len > 0;

    clipPoint :: Integral (Index seq) => SequencePoint seq -> SequencePoint seq -> SequencePoint seq;
    clipPoint len p = if p < 0 then 0 else if p > len then len else p;

    clipRunBounds :: Integral (Index seq) => SequencePoint seq -> SequenceRun seq -> SequenceRun seq;
    clipRunBounds len (MkSequenceRun rstart rlen) = let
    {
        rend = rstart + rlen;
    } in startEndRun (clipPoint len rstart) (clipPoint len rend);

    clipRunStart :: Integral (Index seq) => SequencePoint seq -> SequenceRun seq -> SequenceRun seq;
    clipRunStart nstart (MkSequenceRun start len) = let
    {
        end = start + len;
    } in startEndRun (max start nstart) end;

    clipRunEnd :: Integral (Index seq) => SequencePoint seq -> SequenceRun seq -> SequenceRun seq;
    clipRunEnd nend (MkSequenceRun start len) = let
    {
        end = start + len;
    } in startEndRun start (min end nend);

    clipWithin :: Integral (Index seq) => SequenceRun seq -> SequenceRun seq -> SequenceRun seq;
    clipWithin constraint run = clipRunEnd (runEnd constraint) $ clipRunStart (runStart constraint) run;

    seqSection :: IsSequence seq => SequenceRun seq -> seq -> seq;
    seqSection (MkSequenceRun start len) s = seqTake len $ seqDrop (max start 0) s;

    seqIntersect :: Integral (Index seq) => SequenceRun seq -> SequenceRun seq -> Maybe (SequenceRun seq);
    seqIntersect a b = let
    {
        ab = clipWithin a b;
    } in if positiveRun ab then Just ab else Nothing;
}
