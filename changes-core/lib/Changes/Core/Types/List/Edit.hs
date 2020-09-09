module Truth.Core.Types.List.Edit
    ( ListEdit(..)
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.List.Read

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
    applyEdit ListEditClear _mr reader = subjectToReadable mempty reader

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
        ma <- getComposeM $ readableToSubject $ itemReadFunction p mr
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
            item <- readableToSubject $ knownItemReadFunction i mr
            write $ ListEditInsert i item
