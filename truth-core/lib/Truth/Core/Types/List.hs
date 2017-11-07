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
itemReadFunction i reader = readable $ ListReadItem i reader

knownItemReadFunction :: Integral (Index seq) => SequencePoint seq -> ReadFunction (ListReader seq reader) reader
knownItemReadFunction i reader = do
    mt <- itemReadFunction i reader
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item " ++ show i ++ " in list"

instance (IsSequence seq, SubjectReader reader, ReaderSubject reader ~ Element seq) =>
         SubjectReader (ListReader seq reader) where
    type ReaderSubject (ListReader seq reader) = seq
    readFromSubject sq ListReadLength = seqLength sq
    readFromSubject sq (ListReadItem i reader) = fmap (\e -> readFromSubject e reader) $ seqIndex sq i

instance (IsSequence seq, FullSubjectReader reader, ReaderSubject reader ~ Element seq) =>
         FullSubjectReader (ListReader seq reader) where
    subjectFromReader = do
        len <- readable ListReadLength
        list <- traverse (\i -> mapReadable (knownItemReadFunction i) subjectFromReader) [0 .. pred len]
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

instance (IsSequence seq, FullSubjectReader (EditReader edit), Edit edit, EditSubject edit ~ Element seq) =>
         Edit (ListEdit seq edit) where
    type EditReader (ListEdit seq edit) = ListReader seq (EditReader edit)
    applyEdit (ListEditItem p edit) (ListReadItem i reader)
        | p == i = mapReadableF (itemReadFunction i) $ applyEdit edit reader -- already checks bounds
    applyEdit (ListEditItem _ _) reader = readable reader
    applyEdit (ListDeleteItem p) ListReadLength = do
        len <- readable ListReadLength
        return $
            if p >= 0 && p < len
                then len - 1
                else len
    applyEdit (ListDeleteItem p) (ListReadItem i reader)
        | p >= 0 && p < i = readable $ ListReadItem (i + 1) reader
    applyEdit (ListDeleteItem _) (ListReadItem i reader) = readable $ ListReadItem i reader
    applyEdit (ListInsertItem p _) ListReadLength = do
        len <- readable ListReadLength
        return $
            if p >= 0 && p <= len
                then len + 1
                else len
    applyEdit (ListInsertItem p a) (ListReadItem i reader)
        | p == i = do
            len <- readable ListReadLength
            return $
                if p >= 0 && p <= len
                    then Just $ readFromSubject a reader
                    else Nothing
    applyEdit (ListInsertItem p _) (ListReadItem i reader)
        | p >= 0 && p < i = readable $ ListReadItem (i - 1) reader
    applyEdit (ListInsertItem _ _) (ListReadItem i reader) = readable $ ListReadItem i reader
    applyEdit ListClear reader = readFromSubjectM (return mempty) reader

instance (IsSequence seq, FullSubjectReader (EditReader edit), InvertableEdit edit, EditSubject edit ~ Element seq) =>
         InvertableEdit (ListEdit seq edit) where
    invertEdit (ListEditItem p edit) = do
        minvedits <- mapReadableF (itemReadFunction p) $ invertEdit edit
        case minvedits of
            Just invedits -> return $ fmap (ListEditItem p) invedits
            Nothing -> return []
    invertEdit (ListInsertItem p _) = do
        len <- readable ListReadLength
        return $
            if p >= 0 && p <= len
                then [ListDeleteItem p]
                else []
    invertEdit (ListDeleteItem p) = do
        ma <- mapReadableF (itemReadFunction p) subjectFromReader
        case ma of
            Just a -> return [ListInsertItem p a]
            Nothing -> return []
    invertEdit ListClear = writerToReadable replaceEdit

instance (IsSequence seq, FullSubjectReader (EditReader edit), Edit edit, EditSubject edit ~ Element seq) =>
         FullEdit (ListEdit seq edit) where
    replaceEdit = do
        wrWrite ListClear
        len <- readable ListReadLength
        let
            readWriteItem ::
                   SequencePoint seq -> WriterReadable (ListEdit seq edit) (ListReader seq (EditReader edit)) ()
            readWriteItem i = do
                item <- mapReadable (knownItemReadFunction i) $ readableToM subjectFromReader
                wrWrite $ ListInsertItem i item
        traverse_ readWriteItem [0 .. pred len]

listItemLens ::
       forall seq edit. (Num (Index seq), Ord (Index seq))
    => IOStateAccess (SequencePoint seq)
    -> GeneralLens (ListEdit seq edit) (MaybeEdit edit)
listItemLens editAccess = let
    editGet :: SequencePoint seq -> OneReader Maybe (EditReader edit) t -> Readable (ListReader seq (EditReader edit)) t
    editGet i (ReadOne rt) = readable $ ListReadItem i rt
    editGet i ReadHasOne
        | i < 0 = return Nothing
    editGet i ReadHasOne = do
        len <- readable ListReadLength
        return $
            if i >= len
                then Nothing
                else return ()
    editUpdate ::
           ListEdit seq edit
        -> SequencePoint seq
        -> Readable (ListReader seq (EditReader edit)) (SequencePoint seq, [MaybeEdit edit])
    editUpdate (ListEditItem ie edit) i
        | ie == i = return (i, [SumEditRight $ MkOneEdit edit])
    editUpdate (ListDeleteItem ie) i
        | ie <= i = return (i - 1, [])
    editUpdate (ListDeleteItem ie) i
        | ie == i = return (i, [SumEditLeft $ MkWholeEdit Nothing])
    editUpdate (ListInsertItem ie _) i
        | ie <= i = return (i + 1, [])
    editUpdate ListClear _ = return (0, [SumEditLeft $ MkWholeEdit Nothing])
    editUpdate _ i = return (i, [])
    editLensFunction = MkEditFunction {..}
    editLensPutEdit ::
           SequencePoint seq
        -> MaybeEdit edit
        -> Readable (EditReader (ListEdit seq edit)) (Maybe (SequencePoint seq, [ListEdit seq edit]))
    editLensPutEdit i (SumEditRight (MkOneEdit edit)) = return $ Just (i, [ListEditItem i edit])
    editLensPutEdit i (SumEditLeft (MkWholeEdit Nothing)) = return $ Just (i, [ListDeleteItem i])
    editLensPutEdit i (SumEditLeft (MkWholeEdit (Just subj))) = return $ Just (i, [ListInsertItem i subj])
    in MkCloseState MkEditLens {..}
