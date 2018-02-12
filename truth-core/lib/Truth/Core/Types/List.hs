module Truth.Core.Types.List where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.OneWholeEdit
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

packBijection :: Bijection ByteString [Word8]
packBijection = MkBijection unpack pack

data ListReader seq reader t where
    ListReadLength :: ListReader seq reader (SequencePoint seq)
    ListReadItem :: SequencePoint seq -> reader t -> ListReader seq reader (Maybe t)

itemReadFunction :: SequencePoint seq -> ReadFunctionF Maybe (ListReader seq reader) reader
itemReadFunction i mr rt = Compose $ mr $ ListReadItem i rt

knownItemReadFunction :: Integral (Index seq) => SequencePoint seq -> ReadFunction (ListReader seq reader) reader
knownItemReadFunction i mr rt = do
    mt <- getCompose $ itemReadFunction i mr rt
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
    ListDeleteItem :: SequencePoint seq -> ListEdit seq edit
    ListInsertItem :: SequencePoint seq -> EditSubject edit -> ListEdit seq edit
    ListClear :: ListEdit seq edit

instance (Enum (Index seq), Ord (Index seq)) => Floating (ListEdit seq edit) (SequencePoint seq) where
    floatingUpdate (ListDeleteItem p) i
        | p < i = pred i
    floatingUpdate (ListInsertItem p _) i
        | p <= i = succ i
    floatingUpdate _ i = i

instance (Enum (Index seq), Ord (Index seq)) => Floating (ListEdit seq edit) (ListEdit seq edit) where
    floatingUpdate edit (ListEditItem i e) = ListEditItem (floatingUpdate edit i) e
    floatingUpdate edit (ListDeleteItem i) = ListDeleteItem (floatingUpdate edit i)
    floatingUpdate edit (ListInsertItem i a) = ListInsertItem (floatingUpdate edit i) a
    floatingUpdate _edit ListClear = ListClear

type instance EditReader (ListEdit seq edit) =
     ListReader seq (EditReader edit)

instance (IsSequence seq, FullSubjectReader (EditReader edit), ApplicableEdit edit, EditSubject edit ~ Element seq) =>
         ApplicableEdit (ListEdit seq edit) where
    applyEdit (ListEditItem p edit) mr (ListReadItem i reader)
        | p == i = getCompose $ applyEdit edit (itemReadFunction i mr) reader -- already checks bounds
    applyEdit (ListEditItem _ _) mr reader = mr reader
    applyEdit (ListDeleteItem p) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p < len
                then len - 1
                else len
    applyEdit (ListDeleteItem p) mr (ListReadItem i reader)
        | p >= 0 && p < i = mr $ ListReadItem (i + 1) reader
    applyEdit (ListDeleteItem _) mr (ListReadItem i reader) = mr $ ListReadItem i reader
    applyEdit (ListInsertItem p _) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p <= len
                then len + 1
                else len
    applyEdit (ListInsertItem p a) mr (ListReadItem i reader)
        | p == i = do
            len <- mr ListReadLength
            return $
                if p >= 0 && p <= len
                    then Just $ subjectToRead a reader
                    else Nothing
    applyEdit (ListInsertItem p _) mr (ListReadItem i reader)
        | p >= 0 && p < i = mr $ ListReadItem (i - 1) reader
    applyEdit (ListInsertItem _ _) mr (ListReadItem i reader) = mr $ ListReadItem i reader
    applyEdit ListClear _mr reader = subjectToMutableRead mempty reader

instance ( IsSequence seq
         , FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , InvertibleEdit edit
         , EditSubject edit ~ Element seq
         ) =>
         InvertibleEdit (ListEdit seq edit) where
    invertEdit (ListEditItem p edit) mr = do
        minvedits <- getCompose $ invertEdit edit $ itemReadFunction p mr
        case minvedits of
            Just invedits -> return $ fmap (ListEditItem p) invedits
            Nothing -> return []
    invertEdit (ListInsertItem p _) mr = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p <= len
                then [ListDeleteItem p]
                else []
    invertEdit (ListDeleteItem p) mr = do
        ma <- getCompose $ mutableReadToSubject $ itemReadFunction p mr
        case ma of
            Just a -> return [ListInsertItem p a]
            Nothing -> return []
    invertEdit ListClear mr = getReplaceEdits mr

instance (IsSequence seq, FullSubjectReader (EditReader edit), ApplicableEdit edit, EditSubject edit ~ Element seq) =>
         FullEdit (ListEdit seq edit) where
    replaceEdit mr write = do
        write ListClear
        len <- mr ListReadLength
        for_ [0 .. pred len] $ \i -> do
            item <- mutableReadToSubject $ knownItemReadFunction i mr
            write $ ListInsertItem i item

listItemLens ::
       forall seq edit.
       (IsSequence seq, FullSubjectReader (EditReader edit), ApplicableEdit edit, EditSubject edit ~ Element seq)
    => Unlift (StateT (SequencePoint seq))
    -> EditLens (ListEdit seq edit) (MaybeEdit edit)
listItemLens unlift = let
    efGet ::
           ReadFunctionT (StateT (SequencePoint seq)) (ListReader seq (EditReader edit)) (OneReader Maybe (EditReader edit))
    efGet mr (ReadOne rt) = do
        i <- get
        lift $ mr $ ListReadItem i rt
    efGet mr ReadHasOne = do
        i <- get
        if i < 0
            then return Nothing
            else do
                len <- lift $ mr ListReadLength
                return $
                    if i >= len
                        then Nothing
                        else Just ()
    efUpdate ::
           forall m. MonadIO m
        => ListEdit seq edit
        -> MutableRead m (EditReader (ListEdit seq edit))
        -> StateT (SequencePoint seq) m [MaybeEdit edit]
    efUpdate (ListEditItem ie edit) _ = do
        i <- get
        return $
            if i == ie
                then [SumEditRight $ MkOneEdit edit]
                else []
    efUpdate (ListDeleteItem ie) _ = do
        i <- get
        case compare ie i of
            LT -> do
                put $ i - 1
                return []
            EQ -> return [SumEditLeft $ MkWholeEdit Nothing]
            GT -> return []
    efUpdate (ListInsertItem ie _) _ = do
        i <- get
        if ie <= i
            then put $ i + 1
            else return ()
        return []
    efUpdate ListClear _ = do
        put 0
        return [SumEditLeft $ MkWholeEdit Nothing]
    elFunction :: AnEditFunction (StateT (SequencePoint seq)) (ListEdit seq edit) (MaybeEdit edit)
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => MaybeEdit edit
        -> MutableRead m (EditReader (ListEdit seq edit))
        -> StateT (SequencePoint seq) m (Maybe [ListEdit seq edit])
    elPutEdit (SumEditRight (MkOneEdit edit)) _ = do
        i <- get
        return $ Just [ListEditItem i edit]
    elPutEdit (SumEditLeft (MkWholeEdit Nothing)) _ = do
        i <- get
        return $ Just [ListDeleteItem i]
    elPutEdit (SumEditLeft (MkWholeEdit (Just subj))) _ = do
        i <- get
        return $ Just [ListInsertItem i subj]
    elPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit edit]
        -> MutableRead m (EditReader (ListEdit seq edit))
        -> StateT (SequencePoint seq) m (Maybe [ListEdit seq edit])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkCloseUnlift unlift MkAnEditLens {..}
