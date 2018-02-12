module Truth.Core.Types.String where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence

data StringRead seq t where
    StringReadLength :: StringRead seq (SequencePoint seq)
    StringReadSection :: SequenceRun seq -> StringRead seq seq

instance Integral (Index seq) => Show (StringRead seq t) where
    show StringReadLength = "length"
    show (StringReadSection run) = "section " ++ show run

instance (c seq, c (SequencePoint seq)) => WitnessConstraint c (StringRead seq) where
    witnessConstraint StringReadLength = Dict
    witnessConstraint (StringReadSection _) = Dict

instance Integral (Index seq) => AllWitnessConstraint Show (StringRead seq) where
    allWitnessConstraint = Dict

instance IsSequence seq => SubjectReader (StringRead seq) where
    type ReaderSubject (StringRead seq) = seq
    subjectToRead s StringReadLength = seqLength s
    subjectToRead s (StringReadSection run) = seqSection run s

instance IsSequence seq => FullSubjectReader (StringRead seq) where
    mutableReadToSubject mr = do
        len <- mr StringReadLength
        mr $ StringReadSection $ MkSequenceRun 0 len

data StringEdit seq
    = StringReplaceWhole seq
    | StringReplaceSection (SequenceRun seq)
                           seq

instance (Show seq, Integral (Index seq)) => Show (StringEdit seq) where
    show (StringReplaceWhole s) = "whole " ++ show s
    show (StringReplaceSection r s) = "section " ++ show r ++ " " ++ show s

floatingUpdateLeft :: IsSequence seq => StringEdit seq -> SequencePoint seq -> SequencePoint seq
floatingUpdateLeft (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    uend = ustart + ulen
    slen = seqLength u
    in if i > uend
           then i + slen - ulen
           else if i > ustart + slen
                    then ustart + slen
                    else i
floatingUpdateLeft _ i = i

floatingUpdateRight :: IsSequence seq => StringEdit seq -> SequencePoint seq -> SequencePoint seq
floatingUpdateRight (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    uend = ustart + ulen
    slen = seqLength u
    in if i >= uend
           then i + slen - ulen
           else if i > ustart + slen
                    then ustart + slen
                    else i
floatingUpdateRight _ i = i

instance IsSequence seq => Floating (StringEdit seq) (SequencePoint seq) where
    floatingUpdate = floatingUpdateRight

instance IsSequence seq => Floating (StringEdit seq) (SequenceRun seq) where
    floatingUpdate edit (MkSequenceRun ostart olen) = let
        oend = ostart + olen
        in startEndRun (floatingUpdateRight edit ostart) (floatingUpdateLeft edit oend)

instance IsSequence seq => Floating (StringEdit seq) (StringEdit seq) where
    floatingUpdate _ (StringReplaceWhole s) = StringReplaceWhole s
    floatingUpdate edit (StringReplaceSection run s) = StringReplaceSection (floatingUpdate edit run) s

type instance EditReader (StringEdit seq) = StringRead seq

instance IsSequence seq => ApplicableEdit (StringEdit seq) where
    applyEdit (StringReplaceWhole s) _ reader = return $ subjectToRead s reader
    applyEdit (StringReplaceSection erunRaw s) mr StringReadLength = do
        oldlen <- mr StringReadLength
        let
            (MkSequenceRun _estart elen) = clipRunBounds oldlen erunRaw
            slen = seqLength s
        return $ oldlen + slen - elen
    applyEdit (StringReplaceSection erunRaw s) mr (StringReadSection rrunRaw) = do
        oldlen <- mr StringReadLength
        let
            (MkSequenceRun estart elen) = clipRunBounds oldlen erunRaw
            slen = seqLength s
            newlen = oldlen + slen - elen
            rrun = clipRunBounds newlen rrunRaw
            beforeRun = clipRunEnd estart rrun
            middleRelRun = clipRunBounds slen $ relativeRun estart rrun
            afterRun = relativeRun (slen - elen) $ clipRunStart (estart + slen) rrun
            middle =
                if positiveRun middleRelRun
                    then seqSection middleRelRun s
                    else mempty
        before <-
            if positiveRun beforeRun
                then mr $ StringReadSection beforeRun
                else return mempty
        after <-
            if positiveRun afterRun
                then mr $ StringReadSection afterRun
                else return mempty
        return $ mappend before $ mappend middle after

instance IsSequence seq => InvertibleEdit (StringEdit seq) where
    invertEdit (StringReplaceWhole _) mr = do
        olds <- mutableReadToSubject mr
        return [StringReplaceWhole olds]
    invertEdit (StringReplaceSection run@(MkSequenceRun start _) s) mr = do
        olds <- mr $ StringReadSection run
        return [StringReplaceSection (MkSequenceRun start (seqLength s)) olds]

instance IsSequence seq => FullEdit (StringEdit seq) where
    replaceEdit mr writeEdit = do
        a <- mutableReadToSubject mr
        writeEdit $ StringReplaceWhole a

stringSectionLens ::
       forall seq. IsSequence seq
    => SequenceRun seq
    -> IO (EditLens (StringEdit seq) (StringEdit seq))
stringSectionLens initial =
    newMVar initial >>= \var ->
        return $ let
            getState ::
                   forall m. MonadIO m
                => MutableRead m (EditReader (StringEdit seq))
                -> StateT (SequenceRun seq) m (SequenceRun seq)
            getState mr = do
                len <- lift $ mr StringReadLength
                stateRaw <- get
                return $ clipRunBounds len stateRaw
            efGet :: ReadFunctionT (StateT (SequenceRun seq)) (StringRead seq) (StringRead seq)
            efGet mr rt = do
                st <- getState mr
                case rt of
                    StringReadLength -> return $ runLength st
                    StringReadSection run ->
                        lift $ mr $ StringReadSection $ clipWithin st $ relativeRun (negate $ runStart st) run
            efUpdate ::
                   forall m. MonadIO m
                => StringEdit seq
                -> MutableRead m (EditReader (StringEdit seq))
                -> StateT (SequenceRun seq) m [StringEdit seq]
            efUpdate edita mr = do
                oldstate <- getState mr
                let newstate = floatingUpdate edita oldstate
                put newstate
                case edita of
                    StringReplaceWhole s -> return $ return $ StringReplaceWhole $ seqSection newstate s
                    StringReplaceSection rawruna sa -> do
                        let runa = rawruna {- clipRunBounds len -}
                        return $
                            maybeToList $ do
                                runb' <- seqIntersectInside oldstate runa
                                let
                                    runb = relativeRun (runStart oldstate) runb'
                                    sb =
                                        seqSection
                                            (clipRunBounds (seqLength sa) $ relativeRun (runStart runa) newstate)
                                            sa
                                return $ StringReplaceSection runb sb
            elFunction :: AnEditFunction (StateT (SequenceRun seq)) (StringEdit seq) (StringEdit seq)
            elFunction = MkAnEditFunction {..}
            elPutEdit ::
                   forall m. MonadIO m
                => StringEdit seq
                -> MutableRead m (EditReader (StringEdit seq))
                -> StateT (SequenceRun seq) m (Maybe [StringEdit seq])
            elPutEdit editb mr = do
                oldstate <- getState mr
                case editb of
                    StringReplaceWhole sb -> do
                        put oldstate {runLength = seqLength sb}
                        return $ Just [StringReplaceSection oldstate sb]
                    StringReplaceSection runb sb -> do
                        let
                            newlength = runLength oldstate + seqLength sb - runLength runb
                            runa = relativeRun (negate $ runStart oldstate) runb
                        put oldstate {runLength = newlength}
                        return $ Just [StringReplaceSection runa sb]
            elPutEdits ::
                   forall m. MonadIO m
                => [StringEdit seq]
                -> MutableRead m (EditReader (StringEdit seq))
                -> StateT (SequenceRun seq) m (Maybe [StringEdit seq])
            elPutEdits = elPutEditsFromPutEdit elPutEdit
            in MkCloseUnlift (mvarUnlift var) MkAnEditLens {..}
