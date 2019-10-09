module Truth.Core.Types.List where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.OneWhole
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

data ListReader seq reader t where
    ListReadLength :: ListReader seq reader (SequencePoint seq)
    ListReadItem :: SequencePoint seq -> reader t -> ListReader seq reader (Maybe t)

itemReadFunction :: SequencePoint seq -> ReadFunctionF Maybe (ListReader seq reader) reader
itemReadFunction i mr rt = MkComposeM $ mr $ ListReadItem i rt

knownItemReadFunction :: Integral (Index seq) => SequencePoint seq -> ReadFunction (ListReader seq reader) reader
knownItemReadFunction i mr rt = do
    mt <- getComposeM $ itemReadFunction i mr rt
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item " ++ show i ++ " in list"

instance (IsSequence seq, SubjectReader reader, ReaderSubject reader ~ Element seq) =>
             SubjectReader (ListReader seq reader) where
    type ReaderSubject (ListReader seq reader) = seq
    subjectToRead sq ListReadLength = seqLength sq
    subjectToRead sq (ListReadItem i reader) = fmap (\e -> subjectToRead e reader) $ seqIndex sq i

instance (IsSequence seq, FullSubjectReader reader, ReaderSubject reader ~ Element seq) =>
             FullSubjectReader (ListReader seq reader) where
    mutableReadToSubject mr = do
        len <- mr ListReadLength
        list <- for [0 .. pred len] $ \i -> mutableReadToSubject $ knownItemReadFunction i mr
        return $ fromList list

data ListEdit seq edit where
    ListEditItem :: SequencePoint seq -> edit -> ListEdit seq edit
    ListEditDelete :: SequencePoint seq -> ListEdit seq edit
    ListEditInsert :: SequencePoint seq -> EditSubject edit -> ListEdit seq edit
    ListEditClear :: ListEdit seq edit

instance (Enum (Index seq), Ord (Index seq)) => Floating (ListEdit seq edit) (SequencePoint seq) where
    floatingUpdate (ListEditDelete p) i
        | p < i = pred i
    floatingUpdate (ListEditInsert p _) i
        | p <= i = succ i
    floatingUpdate _ i = i

instance (Enum (Index seq), Ord (Index seq)) => Floating (ListEdit seq edit) (ListEdit seq edit) where
    floatingUpdate edit (ListEditItem i e) = ListEditItem (floatingUpdate edit i) e
    floatingUpdate edit (ListEditDelete i) = ListEditDelete (floatingUpdate edit i)
    floatingUpdate edit (ListEditInsert i a) = ListEditInsert (floatingUpdate edit i) a
    floatingUpdate _edit ListEditClear = ListEditClear

type instance EditReader (ListEdit seq edit) =
     ListReader seq (EditReader edit)

instance (IsSequence seq, FullSubjectReader (EditReader edit), ApplicableEdit edit, EditSubject edit ~ Element seq) =>
             ApplicableEdit (ListEdit seq edit) where
    applyEdit (ListEditItem p edit) mr (ListReadItem i reader)
        | p == i = getComposeM $ applyEdit edit (itemReadFunction i mr) reader -- already checks bounds
    applyEdit (ListEditItem _ _) mr reader = mr reader
    applyEdit (ListEditDelete p) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p < len
                then len - 1
                else len
    applyEdit (ListEditDelete p) mr (ListReadItem i reader)
        | p >= 0 && p < i = mr $ ListReadItem (i + 1) reader
    applyEdit (ListEditDelete _) mr (ListReadItem i reader) = mr $ ListReadItem i reader
    applyEdit (ListEditInsert p _) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p <= len
                then len + 1
                else len
    applyEdit (ListEditInsert p a) mr (ListReadItem i reader)
        | p == i = do
            len <- mr ListReadLength
            return $
                if p >= 0 && p <= len
                    then Just $ subjectToRead a reader
                    else Nothing
    applyEdit (ListEditInsert p _) mr (ListReadItem i reader)
        | p >= 0 && p < i = mr $ ListReadItem (i - 1) reader
    applyEdit (ListEditInsert _ _) mr (ListReadItem i reader) = mr $ ListReadItem i reader
    applyEdit ListEditClear _mr reader = subjectToMutableRead mempty reader

instance ( IsSequence seq
         , FullSubjectReader (EditReader edit)
         , SubjectMapEdit edit
         , ApplicableEdit edit
         , InvertibleEdit edit
         , EditSubject edit ~ Element seq
         ) => InvertibleEdit (ListEdit seq edit) where
    invertEdit (ListEditItem p edit) mr = do
        minvedits <- getComposeM $ invertEdit edit $ itemReadFunction p mr
        case minvedits of
            Just invedits -> return $ fmap (ListEditItem p) invedits
            Nothing -> return []
    invertEdit (ListEditInsert p _) mr = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p <= len
                then [ListEditDelete p]
                else []
    invertEdit (ListEditDelete p) mr = do
        ma <- getComposeM $ mutableReadToSubject $ itemReadFunction p mr
        case ma of
            Just a -> return [ListEditInsert p a]
            Nothing -> return []
    invertEdit ListEditClear mr = getReplaceEdits mr

instance (IsSequence seq, SubjectReader (EditReader edit), SubjectMapEdit edit, EditSubject edit ~ Element seq) =>
             SubjectMapEdit (ListEdit seq edit) where
    mapSubjectEdits =
        mapEditToMapEdits $ \listedit subj ->
            case listedit of
                ListEditItem p edit -> let
                    (before, after) = seqSplitAt p subj
                    in case uncons after of
                           Just (olditem, rest) -> do
                               newitem <- mapSubjectEdits [edit] olditem
                               return $ before `mappend` opoint newitem `mappend` rest
                           Nothing -> return $ subj
                ListEditDelete p -> let
                    (before, after) = seqSplitAt p subj
                    in case uncons after of
                           Just (_, rest) -> return $ mappend before rest
                           Nothing -> return $ subj
                ListEditInsert p item -> let
                    (before, after) = seqSplitAt p subj
                    in return $ before `mappend` opoint item `mappend` after
                ListEditClear -> return mempty

instance ( IsSequence seq
         , FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , EditSubject edit ~ Element seq
         ) => FullEdit (ListEdit seq edit) where
    replaceEdit mr write = do
        write ListEditClear
        len <- mr ListReadLength
        for_ [0 .. pred len] $ \i -> do
            item <- mutableReadToSubject $ knownItemReadFunction i mr
            write $ ListEditInsert i item

data ListUpdate seq update where
    ListUpdateItem :: SequencePoint seq -> update -> ListUpdate seq update
    ListUpdateDelete :: SequencePoint seq -> ListUpdate seq update
    ListUpdateInsert :: SequencePoint seq -> UpdateSubject update -> ListUpdate seq update
    ListUpdateClear :: ListUpdate seq update

instance IsUpdate update => IsUpdate (ListUpdate seq update) where
    type UpdateEdit (ListUpdate seq update) = ListEdit seq (UpdateEdit update)
    editUpdate (ListEditItem p edit) = ListUpdateItem p $ editUpdate edit
    editUpdate (ListEditDelete p) = ListUpdateDelete p
    editUpdate (ListEditInsert p subj) = ListUpdateInsert p subj
    editUpdate ListEditClear = ListUpdateClear

instance IsEditUpdate update => IsEditUpdate (ListUpdate seq update) where
    updateEdit (ListUpdateItem p update) = ListEditItem p $ updateEdit update
    updateEdit (ListUpdateDelete p) = ListEditDelete p
    updateEdit (ListUpdateInsert p subj) = ListEditInsert p subj
    updateEdit ListUpdateClear = ListEditClear

listItemLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => Untrans (StateT (SequencePoint seq))
    -> EditLens (ListUpdate seq update) (MaybeUpdate update)
listItemLens unlift = let
    ufGet ::
           ReadFunctionT (StateT (SequencePoint seq)) (ListReader seq (UpdateReader update)) (OneReader Maybe (UpdateReader update))
    ufGet mr (ReadOne rt) = do
        i <- get
        lift $ mr $ ListReadItem i rt
    ufGet mr ReadHasOne = do
        i <- get
        if i < 0
            then return Nothing
            else do
                len <- lift $ mr ListReadLength
                return $
                    if i >= len
                        then Nothing
                        else Just ()
    ufUpdate ::
           forall m. MonadIO m
        => ListUpdate seq update
        -> MutableRead m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m [MaybeUpdate update]
    ufUpdate (ListUpdateItem ie update) _ = do
        i <- get
        return $
            if i == ie
                then [SumUpdateRight $ MkOneUpdate update]
                else []
    ufUpdate (ListUpdateDelete ie) _ = do
        i <- get
        case compare ie i of
            LT -> do
                put $ i - 1
                return []
            EQ -> return [SumUpdateLeft $ MkWholeReaderUpdate Nothing]
            GT -> return []
    ufUpdate (ListUpdateInsert ie _) _ = do
        i <- get
        if ie <= i
            then put $ i + 1
            else return ()
        return []
    ufUpdate ListUpdateClear _ = do
        put 0
        return [SumUpdateLeft $ MkWholeReaderUpdate Nothing]
    elFunction :: AnUpdateFunction (StateT (SequencePoint seq)) (ListUpdate seq update) (MaybeUpdate update)
    elFunction = MkAnUpdateFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> MutableRead m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    elPutEdit (SumEditRight (MkOneEdit edit)) _ = do
        i <- get
        return $ Just [ListEditItem i edit]
    elPutEdit (SumEditLeft (MkWholeReaderEdit Nothing)) _ = do
        i <- get
        return $ Just [ListEditDelete i]
    elPutEdit (SumEditLeft (MkWholeReaderEdit (Just subj))) _ = do
        i <- get
        return $ Just [ListEditInsert i subj]
    elPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> MutableRead m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkRunnableT2 unlift MkAnEditLens {..}
