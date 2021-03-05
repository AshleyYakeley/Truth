module Changes.Core.Types.List.Ordered where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Sequence
import Changes.Core.Types.List.Edit
import Changes.Core.Types.List.Read
import Changes.Core.Types.List.Update
import Changes.Core.Types.None
import Changes.Core.Types.One.FullResult
import Changes.Core.Types.One.Read
import Changes.Core.Types.One.Result
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Whole

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
    OrderedListUpdateItem :: SequencePoint seq -> SequencePoint seq -> [update] -> OrderedListUpdate seq update
    OrderedListUpdateDelete :: SequencePoint seq -> OrderedListUpdate seq update
    OrderedListUpdateInsert :: SequencePoint seq -> UpdateSubject update -> OrderedListUpdate seq update
    OrderedListUpdateClear :: OrderedListUpdate seq update

type instance UpdateEdit (OrderedListUpdate seq update) =
     OrderedListEdit seq (UpdateEdit update)

instance (IsSequence seq, FullSubjectReader (UpdateReader update)) => FullUpdate (OrderedListUpdate seq update) where
    replaceUpdate rd push = do
        push OrderedListUpdateClear
        len <- rd ListReadLength
        for_ [0 .. pred len] $ \i -> do
            msubj <- getComposeM $ readableToSubject $ itemReadFunction i rd
            case msubj of
                Just subj -> push $ OrderedListUpdateInsert i $ subj
                Nothing -> return ()

orderedListLengthLens ::
       forall seq update. (Num (Index seq))
    => ChangeLens (OrderedListUpdate seq update) (ROWUpdate (SequencePoint seq))
orderedListLengthLens = let
    clRead :: ReadFunction (ListReader seq (UpdateReader update)) (WholeReader (SequencePoint seq))
    clRead mr ReadWhole = mr ListReadLength
    clUpdate ::
           forall m. MonadIO m
        => OrderedListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> m [ROWUpdate (SequencePoint seq)]
    clUpdate OrderedListUpdateClear _ = return $ pure $ MkReadOnlyUpdate $ MkWholeUpdate 0
    clUpdate (OrderedListUpdateItem _ _ _) _ = return []
    clUpdate _ mr = do
        i <- mr ListReadLength
        return $ pure $ MkReadOnlyUpdate $ MkWholeUpdate i
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

-- no "instance IsUpdate OrderedListUpdate", because we cannot calculate moves without knowing the order

-- | prevents creation of the element
orderedListItemLinearLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => SequencePoint seq
    -> LinearFloatingChangeLens (StateLensVar (SequencePoint seq)) (OrderedListUpdate seq update) (MaybeUpdate update)
orderedListItemLinearLens initpos = let
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
    sclUpdate (OrderedListUpdateItem oldie newie lupdate) _ = do
        i <- get
        case compare oldie i of
            EQ -> do
                put newie
                return $ fmap (\update -> MkFullResultOneUpdate $ SuccessResultOneUpdate update) lupdate
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
        -> StateT (SequencePoint seq) m (Maybe [OrderedListEdit seq (UpdateEdit update)])
    sPutEdit (SuccessFullResultOneEdit edit) = do
        i <- get
        return $ Just [OrderedListEditItem i edit]
    sPutEdit (NewFullResultOneEdit Nothing) = do
        i <- get
        return $ Just [OrderedListEditDelete i]
    sPutEdit (NewFullResultOneEdit (Just _)) = return Nothing
    sclPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> Readable m NullReader
        -> StateT (SequencePoint seq) m (Maybe [OrderedListEdit seq (UpdateEdit update)])
    sclPutEdits = linearPutEditsFromPutEdit sPutEdit
    in makeStateExpLens MkStateChangeLens {..}

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
orderedListItemLens initpos = expToFloatingChangeLens $ orderedListItemLinearLens initpos

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
    clUpdate (ListUpdateItem p update) _ = return $ pure $ OrderedListUpdateItem p p [update]
    clUpdate (ListUpdateDelete p) _ = return $ pure $ OrderedListUpdateDelete p
    clUpdate (ListUpdateInsert p subj) _ = return $ pure $ OrderedListUpdateInsert p subj
    clUpdate ListUpdateClear _ = return $ pure OrderedListUpdateClear
    clPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit seq (UpdateEdit update)
        -> Readable m (ListReader seq (UpdateReader update))
        -> m (Maybe [ListEdit seq (UpdateEdit update)])
    clPutEdit (OrderedListEditItem p edit) _ = return $ Just $ pure $ ListEditItem p edit
    clPutEdit (OrderedListEditDelete p) _ = return $ Just $ pure $ ListEditDelete p
    clPutEdit OrderedListEditClear _ = return $ Just $ pure ListEditClear
    clPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit seq (UpdateEdit update)]
        -> Readable m (ListReader seq (UpdateReader update))
        -> m (Maybe [ListEdit seq (UpdateEdit update)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens {..}

liftOrderedListChangeLens ::
       forall seqA updateA seqB updateB.
       ( IsSequence seqA
       , UpdateSubject updateA ~ Element seqA
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateB)
       , Index seqA ~ Index seqB
       )
    => ChangeLens updateA updateB
    -> ChangeLens (OrderedListUpdate seqA updateA) (OrderedListUpdate seqB updateB)
liftOrderedListChangeLens (MkChangeLens g up pe) = let
    clRead :: ReadFunction (ListReader seqA (UpdateReader updateA)) (ListReader seqB (UpdateReader updateB))
    clRead rd ListReadLength = fmap seqPointConvert $ rd ListReadLength
    clRead rd (ListReadItem i rb) = getComposeM $ g (\ra -> MkComposeM $ rd (ListReadItem (seqPointConvert i) ra)) rb
    clUpdate ::
           forall m. MonadIO m
        => OrderedListUpdate seqA updateA
        -> Readable m (ListReader seqA (UpdateReader updateA))
        -> m [OrderedListUpdate seqB updateB]
    clUpdate (OrderedListUpdateItem i1 i2 lu) rd = do
        u' <- for lu $ \u -> getComposeM $ up u $ \ra -> MkComposeM $ rd $ ListReadItem (seqPointConvert i1) ra
        return $
            case sequenceA u' of
                Nothing -> []
                Just ubb -> pure $ OrderedListUpdateItem (seqPointConvert i1) (seqPointConvert i2) $ mconcat ubb
    clUpdate (OrderedListUpdateDelete i) _ = return $ pure $ OrderedListUpdateDelete $ seqPointConvert i
    clUpdate (OrderedListUpdateInsert i subjA) _ = do
        subjB <- readableToSubject $ g $ subjectToReadable subjA
        return $ pure $ OrderedListUpdateInsert (seqPointConvert i) subjB
    clUpdate OrderedListUpdateClear _ = return $ pure OrderedListUpdateClear
    clPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit seqB (UpdateEdit updateB)
        -> Readable m (ListReader seqA (UpdateReader updateA))
        -> m (Maybe [OrderedListEdit seqA (UpdateEdit updateA)])
    clPutEdit (OrderedListEditItem i editB) rd =
        getComposeM $ do
            meditAs <- pe [editB] $ \ra -> MkComposeM $ rd (ListReadItem (seqPointConvert i) ra)
            editAs <- liftInner meditAs
            return $ fmap (OrderedListEditItem $ seqPointConvert i) editAs
    clPutEdit (OrderedListEditDelete i) _ = return $ Just $ pure $ OrderedListEditDelete $ seqPointConvert i
    clPutEdit OrderedListEditClear _ = return $ Just $ pure OrderedListEditClear
    clPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit seqB (UpdateEdit updateB)]
        -> Readable m (ListReader seqA (UpdateReader updateA))
        -> m (Maybe [OrderedListEdit seqA (UpdateEdit updateA)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens {..}
