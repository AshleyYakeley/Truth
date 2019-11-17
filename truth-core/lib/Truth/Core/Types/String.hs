module Truth.Core.Types.String
    ( StringUpdate
    , StringRead(..)
    , StringEdit(..)
    , stringSectionLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Resource
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
           else if i > ustart
                    then ustart
                    else i
floatingUpdateLeft _ i = i

floatingUpdateRight :: IsSequence seq => StringEdit seq -> SequencePoint seq -> SequencePoint seq
floatingUpdateRight (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    uend = ustart + ulen
    slen = seqLength u
    in if i >= uend
           then i + slen - ulen
           else if i >= ustart
                    then ustart + slen
                    else i
floatingUpdateRight _ i = i

instance IsSequence seq => Floating (StringEdit seq) (SequencePoint seq) where
    floatingUpdate = floatingUpdateRight

instance IsSequence seq => Floating (StringEdit seq) (SequenceRun seq) where
    floatingUpdate edit (MkSequenceRun ostart olen) = let
        oend = ostart + olen
        in startEndRun (floatingUpdateLeft edit ostart) (floatingUpdateRight edit oend)

instance IsSequence seq => Floating (StringEdit seq) (StringEdit seq) where
    floatingUpdate _ (StringReplaceWhole s) = StringReplaceWhole s
    floatingUpdate edit (StringReplaceSection run s) = StringReplaceSection (floatingUpdate edit run) s

type instance EditReader (StringEdit seq) = StringRead seq

cleanEdit :: Integral (Index seq) => SequencePoint seq -> SequenceRun seq -> Maybe (SequenceRun seq)
cleanEdit _len run
    | not $ goodRun run = Nothing
cleanEdit len run
    | runStart run > len = Nothing
cleanEdit len run
    | runEnd run > len = Just $ startEndRun (runStart run) len
cleanEdit _len run = Just run

instance IsSequence seq => ApplicableEdit (StringEdit seq) where
    applyEdit (StringReplaceWhole s) _ reader = return $ subjectToRead s reader
    applyEdit (StringReplaceSection erunRaw s) mr StringReadLength = do
        oldlen <- mr StringReadLength
        return $
            case cleanEdit oldlen erunRaw of
                Just erun -> oldlen + seqLength s - runLength erun
                Nothing -> oldlen
    applyEdit (StringReplaceSection erunRaw s) mr (StringReadSection rrunRaw) = do
        oldlen <- mr StringReadLength
        case cleanEdit oldlen erunRaw of
            Just erun -> do
                let
                    (MkSequenceRun estart elen) = erun
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
            Nothing -> mr $ StringReadSection rrunRaw

instance IsSequence seq => InvertibleEdit (StringEdit seq) where
    invertEdit (StringReplaceWhole _) mr = do
        olds <- mutableReadToSubject mr
        return [StringReplaceWhole olds]
    invertEdit (StringReplaceSection run@(MkSequenceRun start _) s) mr = do
        olds <- mr $ StringReadSection run
        return [StringReplaceSection (MkSequenceRun start (seqLength s)) olds]

instance IsSequence seq => SubjectMapEdit (StringEdit seq)

instance IsSequence seq => FullEdit (StringEdit seq) where
    replaceEdit mr writeEdit = do
        a <- mutableReadToSubject mr
        writeEdit $ StringReplaceWhole a

type StringUpdate seq = EditUpdate (StringEdit seq)

stringSectionLens ::
       forall seq. IsSequence seq
    => SequenceRun seq
    -> IO (EditLens (StringUpdate seq) (StringUpdate seq))
stringSectionLens initial = do
    trun <- stateTransStackRunner initial
    return $ let
        getState ::
               forall m. MonadIO m
            => MutableRead m (StringRead seq)
            -> StateT (SequenceRun seq) m (SequenceRun seq)
        getState mr = do
            len <- lift $ mr StringReadLength
            stateRaw <- get
            return $ clipRunBounds len stateRaw
        ufGet :: ReadFunctionT (StateT (SequenceRun seq)) (StringRead seq) (StringRead seq)
        ufGet mr rt = do
            st <- getState mr
            case rt of
                StringReadLength -> return $ runLength st
                StringReadSection run ->
                    lift $ mr $ StringReadSection $ clipWithin st $ relativeRun (negate $ runStart st) run
        ufUpdate ::
               forall m. MonadIO m
            => StringUpdate seq
            -> MutableRead m (StringRead seq)
            -> StateT (SequenceRun seq) m [StringUpdate seq]
        ufUpdate (MkEditUpdate edita) mr = do
            oldstate <- get
            newlen <- lift $ mr StringReadLength
            let
                rawnewstate = floatingUpdate edita oldstate
                newstate = clipRunBounds newlen rawnewstate
            case edita of
                StringReplaceWhole s -> do
                    put newstate
                    return $ return $ MkEditUpdate $ StringReplaceWhole $ seqSection newstate s
                StringReplaceSection runa sa ->
                    case goodRun runa of
                        False -> return []
                        True -> do
                            put newstate
                            return $
                                maybeToList $ do
                                    runb' <- seqIntersectInside oldstate runa
                                    let
                                        runb = relativeRun (runStart oldstate) runb'
                                        sb =
                                            seqSection
                                                (clipRunBounds (seqLength sa) $ relativeRun (runStart runa) newstate)
                                                sa
                                    case (runLength runb, onull sb) of
                                        (0, True) -> Nothing
                                        _ -> return $ MkEditUpdate $ StringReplaceSection runb sb
        elFunction :: AnUpdateFunction ('[ StateT (SequenceRun seq)]) (StringUpdate seq) (StringUpdate seq)
        elFunction = MkAnUpdateFunction {..}
        elPutEdit ::
               forall m. MonadIO m
            => StringEdit seq
            -> MutableRead m (StringRead seq)
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
            -> MutableRead m (StringRead seq)
            -> StateT (SequenceRun seq) m (Maybe [StringEdit seq])
        elPutEdits = elPutEditsFromPutEdit @'[ StateT (SequenceRun seq)] elPutEdit
        in MkRunnable2 trun MkAnEditLens {..}
