module Truth.Core.Types.List.Ordered where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.List.Edit
import Truth.Core.Types.List.Read
import Truth.Core.Types.List.Update
import Truth.Core.Types.One.FullResult
import Truth.Core.Types.One.Read
import Truth.Core.Types.One.Result

-- | Like ListEdit, except without a way of adding new elements.
-- This is what both lists and unordered sets presented in some order have in common.
data OrderedListEdit seq edit where
    OrderedListEditItem :: SequencePoint seq -> edit -> OrderedListEdit seq edit
    OrderedListEditDelete :: SequencePoint seq -> OrderedListEdit seq edit
    OrderedListEditClear :: OrderedListEdit seq edit

instance (Enum (Index seq), Ord (Index seq)) => Floating (OrderedListEdit seq edit) (SequencePoint seq) where
    floatingUpdate (OrderedListEditDelete p) i
        | p < i = pred i
    floatingUpdate _ i = i

instance (Enum (Index seq), Ord (Index seq)) => Floating (OrderedListEdit seq edit) (OrderedListEdit seq edit) where
    floatingUpdate edit (OrderedListEditItem i e) = OrderedListEditItem (floatingUpdate edit i) e
    floatingUpdate edit (OrderedListEditDelete i) = OrderedListEditDelete (floatingUpdate edit i)
    floatingUpdate _edit OrderedListEditClear = OrderedListEditClear

type instance EditReader (OrderedListEdit seq edit) =
     ListReader seq (EditReader edit)

instance (IsSequence seq, FullSubjectReader (EditReader edit), ApplicableEdit edit, EditSubject edit ~ Element seq) =>
             ApplicableEdit (OrderedListEdit seq edit) where
    applyEdit (OrderedListEditItem p edit) mr (ListReadItem i reader)
        | p == i = getComposeM $ applyEdit edit (itemReadFunction i mr) reader -- already checks bounds
    applyEdit (OrderedListEditItem _ _) mr reader = mr reader
    applyEdit (OrderedListEditDelete p) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p < len
                then len - 1
                else len
    applyEdit (OrderedListEditDelete p) mr (ListReadItem i reader)
        | p >= 0 && p < i = mr $ ListReadItem (i + 1) reader
    applyEdit (OrderedListEditDelete _) mr (ListReadItem i reader) = mr $ ListReadItem i reader
    applyEdit OrderedListEditClear _mr reader = subjectToReadable mempty reader

instance (IsSequence seq, SubjectReader (EditReader edit), SubjectMapEdit edit, EditSubject edit ~ Element seq) =>
             SubjectMapEdit (OrderedListEdit seq edit) where
    mapSubjectEdits =
        mapEditToMapEdits $ \listedit subj ->
            case listedit of
                OrderedListEditItem p edit -> let
                    (before, after) = seqSplitAt p subj
                    in case uncons after of
                           Just (olditem, rest) -> do
                               newitem <- mapSubjectEdits [edit] olditem
                               return $ before `mappend` opoint newitem `mappend` rest
                           Nothing -> return $ subj
                OrderedListEditDelete p -> let
                    (before, after) = seqSplitAt p subj
                    in case uncons after of
                           Just (_, rest) -> return $ mappend before rest
                           Nothing -> return $ subj
                OrderedListEditClear -> return mempty

data OrderedListUpdate seq update where
    OrderedListUpdateItem :: SequencePoint seq -> SequencePoint seq -> Maybe update -> OrderedListUpdate seq update
    OrderedListUpdateDelete :: SequencePoint seq -> OrderedListUpdate seq update
    OrderedListUpdateInsert :: SequencePoint seq -> UpdateSubject update -> OrderedListUpdate seq update
    OrderedListUpdateClear :: OrderedListUpdate seq update

type instance UpdateEdit (OrderedListUpdate seq update) =
     OrderedListEdit seq (UpdateEdit update)

-- no "instance IsUpdate OrderedListUpdate", because we cannot calculate moves without knowing the order

-- | prevents creation of the element
orderedListItemLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => SequencePoint seq
    -> FloatingChangeLens (OrderedListUpdate seq update) (MaybeUpdate update)
orderedListItemLens initpos = let
    sclInit ::
           forall m. MonadIO m
        => Readable m (ListReader seq (UpdateReader update))
        -> m (SequencePoint seq)
    sclInit _ = return initpos
    sclRead ::
           ReadFunctionT (StateT (SequencePoint seq)) (ListReader seq (UpdateReader update)) (OneReader Maybe (UpdateReader update))
    sclRead mr (ReadOne rt) = do
        i <- get
        lift $ mr $ ListReadItem i rt
    sclRead mr ReadHasOne = do
        i <- get
        if i < 0
            then return Nothing
            else do
                len <- lift $ mr ListReadLength
                return $
                    if i >= len
                        then Nothing
                        else Just ()
    sclUpdate ::
           forall m. MonadIO m
        => OrderedListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m [MaybeUpdate update]
    sclUpdate (OrderedListUpdateItem oldie newie mupdate) _ = do
        i <- get
        case compare oldie i of
            EQ -> do
                put newie
                return $
                    case mupdate of
                        Just update -> [MkFullResultOneUpdate $ SuccessResultOneUpdate update]
                        Nothing -> []
            LT -> do
                if newie >= i
                    then put $ pred i
                    else return ()
                return []
            GT -> do
                if newie <= i
                    then put $ succ i
                    else return ()
                return []
    sclUpdate (OrderedListUpdateDelete ie) _ = do
        i <- get
        case compare ie i of
            LT -> do
                put $ i - 1
                return []
            EQ -> return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
            GT -> return []
    sclUpdate (OrderedListUpdateInsert ie _) _ = do
        i <- get
        if ie <= i
            then put $ i + 1
            else return ()
        return []
    sclUpdate OrderedListUpdateClear _ = do
        put 0
        return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m (Maybe [OrderedListEdit seq (UpdateEdit update)])
    sPutEdit (SuccessFullResultOneEdit edit) _ = do
        i <- get
        return $ Just [OrderedListEditItem i edit]
    sPutEdit (NewFullResultOneEdit Nothing) _ = do
        i <- get
        return $ Just [OrderedListEditDelete i]
    sPutEdit (NewFullResultOneEdit (Just _)) _ = return Nothing
    sclPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m (Maybe [OrderedListEdit seq (UpdateEdit update)])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens MkStateChangeLens {..}

listOrderedListChangeLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => ChangeLens (ListUpdate seq update) (OrderedListUpdate seq update)
listOrderedListChangeLens = let
    clRead :: ReadFunction (ListReader seq (UpdateReader update)) (ListReader seq (UpdateReader update))
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => ListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> m [OrderedListUpdate seq update]
    clUpdate (ListUpdateItem p update) _ = return $ pure $ OrderedListUpdateItem p p $ Just update
    clUpdate (ListUpdateDelete p) _ = return $ pure $ OrderedListUpdateDelete p
    clUpdate (ListUpdateInsert p subj) _ = return $ pure $ OrderedListUpdateInsert p subj
    clUpdate ListUpdateClear _ = return $ pure $ OrderedListUpdateClear
    elPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit seq (UpdateEdit update)
        -> Readable m (ListReader seq (UpdateReader update))
        -> m (Maybe [ListEdit seq (UpdateEdit update)])
    elPutEdit (OrderedListEditItem p edit) _ = return $ Just $ pure $ ListEditItem p edit
    elPutEdit (OrderedListEditDelete p) _ = return $ Just $ pure $ ListEditDelete p
    elPutEdit OrderedListEditClear _ = return $ Just $ pure $ ListEditClear
    clPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit seq (UpdateEdit update)]
        -> Readable m (ListReader seq (UpdateReader update))
        -> m (Maybe [ListEdit seq (UpdateEdit update)])
    clPutEdits = clPutEditsFromPutEdit elPutEdit
    in MkChangeLens {..}
