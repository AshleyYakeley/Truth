module Changes.Core.Types.String
    ( StringUpdate
    , StringRead(..)
    , StringEdit(..)
    , stringSectionLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Sequence

data StringRead seq t where
    StringReadLength :: forall seq. StringRead seq SequencePoint
    StringReadSection :: forall seq. SequenceRun -> StringRead seq seq

instance forall seq t. Show (StringRead seq t) where
    show StringReadLength = "length"
    show (StringReadSection run) = "section " ++ show run

instance forall c seq. (c seq, c SequencePoint) => WitnessConstraint c (StringRead seq) where
    witnessConstraint StringReadLength = Dict
    witnessConstraint (StringReadSection _) = Dict

instance forall seq. AllConstraint Show (StringRead seq) where
    allConstraint = Dict

instance forall seq. IsSequence seq => SubjectReader (StringRead seq) where
    type ReaderSubject (StringRead seq) = seq
    subjectToRead s StringReadLength = seqLength s
    subjectToRead s (StringReadSection run) = seqSection run s

instance forall seq. IsSequence seq => FullSubjectReader (StringRead seq) where
    readableToSubject mr = do
        len <- mr StringReadLength
        mr $ StringReadSection $ MkSequenceRun 0 len

data StringEdit seq
    = StringReplaceWhole seq
    | StringReplaceSection SequenceRun
                           seq

instance forall seq. Show seq => Show (StringEdit seq) where
    show (StringReplaceWhole s) = "whole " ++ show s
    show (StringReplaceSection r s) = "section " ++ show r ++ " " ++ show s

floatingUpdateLeft ::
       forall seq. IsSequence seq
    => StringEdit seq
    -> SequencePoint
    -> SequencePoint
floatingUpdateLeft (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    uend = ustart + ulen
    slen = seqLength u
    in if i > uend
           then i + slen - ulen
           else if i > ustart
                    then ustart
                    else i
floatingUpdateLeft _ i = i

floatingUpdateRight ::
       forall seq. IsSequence seq
    => StringEdit seq
    -> SequencePoint
    -> SequencePoint
floatingUpdateRight (StringReplaceSection (MkSequenceRun ustart ulen) u) i = let
    uend = ustart + ulen
    slen = seqLength u
    in if i >= uend
           then i + slen - ulen
           else if i >= ustart
                    then ustart + slen
                    else i
floatingUpdateRight _ i = i

instance forall seq. IsSequence seq => Floating (StringEdit seq) SequencePoint where
    floatingUpdate = floatingUpdateRight

instance forall seq. IsSequence seq => Floating (StringEdit seq) SequenceRun where
    floatingUpdate edit (MkSequenceRun ostart olen) = let
        oend = ostart + olen
        in startEndRun (floatingUpdateLeft edit ostart) (floatingUpdateRight edit oend)

instance forall seq. IsSequence seq => Floating (StringEdit seq) (StringEdit seq) where
    floatingUpdate _ (StringReplaceWhole s) = StringReplaceWhole s
    floatingUpdate edit (StringReplaceSection run s) = StringReplaceSection (floatingUpdate edit run) s

type instance forall seq. EditReader (StringEdit seq) = StringRead seq

cleanEdit :: SequencePoint -> SequenceRun -> Maybe SequenceRun
cleanEdit _len run
    | not $ goodRun run = Nothing
cleanEdit len run
    | runStart run > len = Nothing
cleanEdit len run
    | runEnd run > len = Just $ startEndRun (runStart run) len
cleanEdit _len run = Just run

instance forall seq. IsSequence seq => ApplicableEdit (StringEdit seq) where
    applyEdit (StringReplaceWhole s) _ rd = return $ subjectToRead s rd
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

instance forall seq. IsSequence seq => InvertibleEdit (StringEdit seq) where
    invertEdit (StringReplaceWhole _) mr = do
        olds <- readableToSubject mr
        return [StringReplaceWhole olds]
    invertEdit (StringReplaceSection run@(MkSequenceRun start _) s) mr = do
        olds <- mr $ StringReadSection run
        return [StringReplaceSection (MkSequenceRun start (seqLength s)) olds]

instance forall seq. IsSequence seq => SubjectMapEdit (StringEdit seq)

instance forall seq. IsSequence seq => FullEdit (StringEdit seq) where
    replaceEdit mr writeEdit = do
        a <- readableToSubject mr
        writeEdit $ StringReplaceWhole a

type StringUpdate seq = EditUpdate (StringEdit seq)

stringSectionLens ::
       forall seq. IsSequence seq
    => SequenceRun
    -> FloatingChangeLens (StringUpdate seq) (StringUpdate seq)
stringSectionLens initRun = let
    sclInit ::
           forall m. MonadIO m
        => Readable m (StringRead seq)
        -> m SequenceRun
    sclInit _ = return initRun
    getState ::
           forall m. MonadIO m
        => Readable m (StringRead seq)
        -> StateT SequenceRun m SequenceRun
    getState mr = do
        len <- lift $ mr StringReadLength
        stateRaw <- get
        return $ clipRunBounds len stateRaw
    sclRead :: ReadFunctionT (StateT SequenceRun) (StringRead seq) (StringRead seq)
    sclRead mr rt = do
        st <- getState mr
        case rt of
            StringReadLength -> return $ runLength st
            StringReadSection run ->
                lift $ mr $ StringReadSection $ clipWithin st $ relativeRun (negate $ runStart st) run
    sclUpdate ::
           forall m. MonadIO m
        => StringUpdate seq
        -> Readable m (StringRead seq)
        -> StateT SequenceRun m [StringUpdate seq]
    sclUpdate (MkEditUpdate edita) mr = do
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
    sPutEdit ::
           forall m. MonadIO m
        => StringEdit seq
        -> Readable m (StringRead seq)
        -> StateT SequenceRun m (Maybe [StringEdit seq])
    sPutEdit editb mr = do
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
    sclPutEdits ::
           forall m. MonadIO m
        => [StringEdit seq]
        -> Readable m (StringRead seq)
        -> StateT SequenceRun m (Maybe [StringEdit seq])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens @'NonLinear MkStateChangeLens {..}
