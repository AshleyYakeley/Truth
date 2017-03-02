module Truth.Edit.String where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.Sequence;
    --import Truth.Edit.FloatingEditFunction;
    --import Truth.Edit.FloatingEditLens;


    data StringRead seq t where
    {
        StringReadLength :: StringRead seq (SequencePoint seq);
        StringReadSection :: SequenceRun seq -> StringRead seq seq;
    };

    instance IsSequence seq => Reader (StringRead seq) where
    {
        type ReaderSubject (StringRead seq) = seq;

        readFrom s StringReadLength = seqLength s;
        readFrom s (StringReadSection run) = seqSection run s;
    };

    instance IsSequence seq => FullReader (StringRead seq) where
    {
        fromReader = do
        {
            len <- readable StringReadLength;
            readable $ StringReadSection $ MkSequenceRun 0 len;
        };
    };


    data StringEdit seq = StringReplaceWhole seq | StringReplaceSection (SequenceRun seq) seq;

    instance HasInfo StringEdit where
    {
        info = mkSimpleInfo $(iowitness[t|StringEdit|])
        [
        ];
    };

    instance IsSequence seq => Floating (StringEdit seq) (SequencePoint seq) where
    {
        floatingUpdate (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
        {
            uend = ustart + ulen;
        } in if i >= uend then i + seqLength u - uend else i;
        floatingUpdate _ i = i;
    };

    instance IsSequence seq => Floating (StringEdit seq) (SequenceRun seq) where
    {
        floatingUpdate edit (MkSequenceRun ostart olen) = let
        {
            oend = ostart + olen;
        } in startEndRun (floatingUpdate edit ostart) (floatingUpdate edit oend);
    };

    instance IsSequence seq => Floating (StringEdit seq) (StringEdit seq) where
    {
        floatingUpdate _ (StringReplaceWhole s) = StringReplaceWhole s;
        floatingUpdate edit (StringReplaceSection run s) = StringReplaceSection (floatingUpdate edit run) s;
    };

    instance IsSequence seq => Edit (StringEdit seq) where
    {
        type EditReader (StringEdit seq) = StringRead seq;

        applyEdit (StringReplaceWhole s) reader = return $ readFrom s reader;
        applyEdit (StringReplaceSection erun s) StringReadLength = do
        {
            oldlen <- readable StringReadLength;
            let
            {
                (MkSequenceRun _ elen) = clipRunBounds oldlen erun;
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
                rrun = clipRunBounds oldlen rrunRaw;
                slen = seqLength s;

                beforeRun = clipRunEnd estart rrun;
                middleRelRun = clipRunBounds slen $ shiftRun (- estart) rrun;
                afterRun = shiftRun (elen - slen) $ clipRunStart (estart + slen) rrun;

                middle = if positiveRun middleRelRun then seqSection middleRelRun s else mempty;
            };
            before <- if positiveRun beforeRun then readable $ StringReadSection beforeRun else return mempty;
            after <- if positiveRun afterRun then readable $ StringReadSection afterRun else return mempty;
            return $ mappend before $ mappend middle after;
        };

        invertEdit (StringReplaceWhole _) = do
        {
            olds <- fromReader;
            return [StringReplaceWhole olds];
        };
        invertEdit (StringReplaceSection run@(MkSequenceRun start _) s) = do
        {
            olds <- readable $ StringReadSection run;
            return [StringReplaceSection (MkSequenceRun start (seqLength s)) olds];
        };
    };

{-
    stringSectionLens :: SequenceRun seq -> FloatingEditLens' Identity (SequenceRun seq) (StringEdit seq) (StringEdit seq);
    stringSectionLens floatingEditInitial = let
    {
        floatingEditGet stateRaw reader = do
        {
            len <- readable StringReadLength;
            let
            {
                state = clipRunEnd len stateRaw;
            };
            case reader of
            {
                StringReadLength -> return $ runLength state;
                StringReadSection run -> readable $ StringReadSection $ shiftRun (runStart state) $ clipWithin state run;
            };
        };

        floatingEditUpdate :: edita -> state -> (state,Maybe editb);

        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit :: state -> editb -> Readable (EditReader edita) (m edita);
        floatingEditLensPutEdit stateRaw editb = do
        {
            len <- readable StringReadLength;
            let
            {
                state = clipRunEnd len stateRaw;
            };
            case editb of
            {

            };
        };

    } in MkFloatingEditLens{..}
-}

}
