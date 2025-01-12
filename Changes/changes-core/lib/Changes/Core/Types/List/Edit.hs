module Changes.Core.Types.List.Edit
    ( ListEdit(..)
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read
import Changes.Core.Sequence
import Changes.Core.Types.List.Read

data ListEdit edit where
    ListEditItem :: SequencePoint -> edit -> ListEdit edit
    ListEditDelete :: SequencePoint -> ListEdit edit
    ListEditInsert :: SequencePoint -> EditSubject edit -> ListEdit edit
    ListEditClear :: ListEdit edit

instance FloatingOn (ListEdit edit) SequencePoint where
    floatingUpdate (ListEditDelete p) i
        | p < i = pred i
    floatingUpdate (ListEditInsert p _) i
        | p <= i = succ i
    floatingUpdate _ i = i

instance FloatingOn (ListEdit edit) (ListEdit edit) where
    floatingUpdate edit (ListEditItem i e) = ListEditItem (floatingUpdate edit i) e
    floatingUpdate edit (ListEditDelete i) = ListEditDelete (floatingUpdate edit i)
    floatingUpdate edit (ListEditInsert i a) = ListEditInsert (floatingUpdate edit i) a
    floatingUpdate _edit ListEditClear = ListEditClear

type instance EditReader (ListEdit edit) = ListReader (EditReader edit)

instance (FullSubjectReader (EditReader edit), ApplicableEdit edit) => ApplicableEdit (ListEdit edit) where
    applyEdit (ListEditItem p edit) mr (ListReadItem i rd)
        | p == i = unComposeInner $ applyEdit edit (itemReadFunction i mr) rd -- already checks bounds
    applyEdit (ListEditItem _ _) mr rd = mr rd
    applyEdit (ListEditDelete p) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p < len
                then pred len
                else len
    applyEdit (ListEditDelete p) mr (ListReadItem i rd)
        | p >= 0 && p <= i = mr $ ListReadItem (succ i) rd
    applyEdit (ListEditDelete _) mr (ListReadItem i rd) = mr $ ListReadItem i rd
    applyEdit (ListEditInsert p _) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p <= len
                then succ len
                else len
    applyEdit (ListEditInsert p a) mr (ListReadItem i rd)
        | p == i = do
            len <- mr ListReadLength
            return $
                if p >= 0 && p <= len
                    then Just $ subjectToRead a rd
                    else Nothing
    applyEdit (ListEditInsert p _) mr (ListReadItem i rd)
        | p >= 0 && p < i = mr $ ListReadItem (pred i) rd
    applyEdit (ListEditInsert _ _) mr (ListReadItem i rd) = mr $ ListReadItem i rd
    applyEdit ListEditClear _mr rd = subjectToReadable mempty rd

instance (FullSubjectReader (EditReader edit), SubjectMapEdit edit, ApplicableEdit edit, InvertibleEdit edit) =>
             InvertibleEdit (ListEdit edit) where
    invertEdit (ListEditItem p edit) mr = do
        minvedits <- unComposeInner $ invertEdit edit $ itemReadFunction p mr
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
        ma <- unComposeInner $ readableToSubject $ itemReadFunction p mr
        case ma of
            Just a -> return [ListEditInsert p a]
            Nothing -> return []
    invertEdit ListEditClear mr = getReplaceEdits mr

instance (SubjectReader (EditReader edit), SubjectMapEdit edit) => SubjectMapEdit (ListEdit edit) where
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

instance (FullSubjectReader (EditReader edit), ApplicableEdit edit, SubjectMapEdit edit) => FullEdit (ListEdit edit) where
    replaceEdit mr write = do
        write ListEditClear
        len <- mr ListReadLength
        for_ [0 .. pred len] $ \i -> do
            item <- readableToSubject $ knownItemReadFunction i mr
            write $ ListEditInsert i item
