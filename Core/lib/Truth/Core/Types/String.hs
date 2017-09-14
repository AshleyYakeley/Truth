module Truth.Core.Types.String where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Sequence;


    data StringRead seq t where
    {
        StringReadLength :: StringRead seq (SequencePoint seq);
        StringReadSection :: SequenceRun seq -> StringRead seq seq;
    };

    instance IsSequence seq => SubjectReader (StringRead seq) where
    {
        type ReaderSubject (StringRead seq) = seq;

        readFromSubject s StringReadLength = seqLength s;
        readFromSubject s (StringReadSection run) = seqSection run s;
    };

    instance IsSequence seq => FullSubjectReader (StringRead seq) where
    {
        subjectFromReader = do
        {
            len <- readable StringReadLength;
            readable $ StringReadSection $ MkSequenceRun 0 len;
        };
    };


    data StringEdit seq = StringReplaceWhole seq | StringReplaceSection (SequenceRun seq) seq;

    floatingUpdateLeft :: IsSequence seq => StringEdit seq -> SequencePoint seq -> SequencePoint seq;
    floatingUpdateLeft (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    {
        uend = ustart + ulen;
        slen = seqLength u;
    } in if i > uend then i + slen - ulen else if i > ustart + slen then ustart + slen else i;
    floatingUpdateLeft _ i = i;

    floatingUpdateRight :: IsSequence seq => StringEdit seq -> SequencePoint seq -> SequencePoint seq;
    floatingUpdateRight (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    {
        uend = ustart + ulen;
        slen = seqLength u;
    } in if i >= uend then i + slen - ulen else if i > ustart + slen then ustart + slen else i;
    floatingUpdateRight _ i = i;

    instance IsSequence seq => Floating (StringEdit seq) (SequencePoint seq) where
    {
        floatingUpdate = floatingUpdateRight;
    };

    instance IsSequence seq => Floating (StringEdit seq) (SequenceRun seq) where
    {
        floatingUpdate edit (MkSequenceRun ostart olen) = let
        {
            oend = ostart + olen;
        } in startEndRun (floatingUpdateRight edit ostart) (floatingUpdateLeft edit oend);
    };

    instance IsSequence seq => Floating (StringEdit seq) (StringEdit seq) where
    {
        floatingUpdate _ (StringReplaceWhole s) = StringReplaceWhole s;
        floatingUpdate edit (StringReplaceSection run s) = StringReplaceSection (floatingUpdate edit run) s;
    };

    instance IsSequence seq => Edit (StringEdit seq) where
    {
        type EditReader (StringEdit seq) = StringRead seq;

        applyEdit (StringReplaceWhole s) reader = return $ readFromSubject s reader;
        applyEdit (StringReplaceSection erunRaw s) StringReadLength = do
        {
            oldlen <- readable StringReadLength;
            let
            {
                (MkSequenceRun _estart elen) = clipRunBounds oldlen erunRaw;
                slen = seqLength s;
            };
            return $ oldlen + slen - elen;
        };
        applyEdit (StringReplaceSection erunRaw s) (StringReadSection rrunRaw) = do
        {
            oldlen <- readable StringReadLength;
            let
            {
                (MkSequenceRun estart elen) = clipRunBounds oldlen erunRaw;
                slen = seqLength s;
                newlen = oldlen + slen - elen;
                rrun = clipRunBounds newlen rrunRaw;

                beforeRun = clipRunEnd estart rrun;
                middleRelRun = clipRunBounds slen $ relativeRun estart rrun;
                afterRun = relativeRun (slen - elen) $ clipRunStart (estart + slen) rrun;

                middle = if positiveRun middleRelRun then seqSection middleRelRun s else mempty;
            };
            before <- if positiveRun beforeRun then readable $ StringReadSection beforeRun else return mempty;
            after <- if positiveRun afterRun then readable $ StringReadSection afterRun else return mempty;
            return $ mappend before $ mappend middle after;
        };
    };

    instance IsSequence seq => InvertableEdit (StringEdit seq) where
    {
        invertEdit (StringReplaceWhole _) = do
        {
            olds <- subjectFromReader;
            return [StringReplaceWhole olds];
        };
        invertEdit (StringReplaceSection run@(MkSequenceRun start _) s) = do
        {
            olds <- readable $ StringReadSection run;
            return [StringReplaceSection (MkSequenceRun start (seqLength s)) olds];
        };
    };

    instance IsSequence seq => FullEdit (StringEdit seq) where
    {
        replaceEdit = do
        {
            a <- readableToM $ subjectFromReader;
            wrWrite $ StringReplaceWhole a;
        };
    };

    stringSectionLens :: forall seq. IsSequence seq =>
        SequenceRun seq -> EditLens (SequenceRun seq) (StringEdit seq) (StringEdit seq);
    stringSectionLens editInitial = let
    {
        editGet :: SequenceRun seq -> ReadFunction (StringRead seq) (StringRead seq);
        editGet stateRaw reader = do
        {
            len <- readable StringReadLength;
            let
            {
                st = clipRunEnd len stateRaw;
            };
            case reader of
            {
                StringReadLength -> return $ runLength st;
                StringReadSection run -> readable $ StringReadSection $ clipWithin st $ relativeRun (negate $ runStart st) run;
            };
        };

        editUpdate :: StringEdit seq -> SequenceRun seq -> Readable (StringRead seq) (SequenceRun seq,[StringEdit seq]);
        editUpdate edita rawoldstate = do
        {
            len <- readable StringReadLength;
            let
            {
                oldstate = clipRunBounds len rawoldstate;
                newstate = floatingUpdate edita oldstate;
            };
            leditb <- case edita of
            {
                StringReplaceWhole s -> return $ return $ StringReplaceWhole $ seqSection newstate s;
                StringReplaceSection rawruna sa -> do
                {
                    let
                    {
                        runa = {- clipRunBounds len -} rawruna;
                    };
                    return $ maybeToList $ do
                    {
                        runb' <- seqIntersectInside oldstate runa;
                        let
                        {
                            runb = relativeRun (runStart oldstate) runb';
                            sb = seqSection (clipRunBounds (seqLength sa) $ relativeRun (runStart runa) newstate) sa;
                        };
                        return $ StringReplaceSection runb sb;
                    };
                };
            };
            return (newstate,leditb);
        };

        editLensFunction :: EditFunction (SequenceRun seq) (StringEdit seq) (StringEdit seq);
        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: SequenceRun seq -> StringEdit seq -> Readable (StringRead seq) (Maybe (SequenceRun seq,[StringEdit seq]));
        editLensPutEdit stateRaw editb = do
        {
            len <- readable StringReadLength;
            let
            {
                oldstate = clipRunEnd len stateRaw;
            };
            return $ Just $ case editb of
            {
                StringReplaceWhole sb -> (oldstate{runLength=seqLength sb},[StringReplaceSection oldstate sb]);
                StringReplaceSection runb sb -> let
                {
                    newlength = runLength oldstate + seqLength sb - runLength runb;
                    newstate = oldstate{runLength=newlength};
                    runa = relativeRun (negate $ runStart oldstate) runb;
                } in (newstate,[StringReplaceSection runa sb]);
            };
        };

    } in MkEditLens{..}
}
