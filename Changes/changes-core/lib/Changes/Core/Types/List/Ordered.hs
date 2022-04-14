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
data OrderedListEdit edit where
    OrderedListEditItem :: SequencePoint -> edit -> OrderedListEdit edit
    OrderedListEditDelete :: SequencePoint -> OrderedListEdit edit
    OrderedListEditClear :: OrderedListEdit edit

instance Floating (OrderedListEdit edit) SequencePoint where
    floatingUpdate (OrderedListEditDelete p) i
        | p < i = pred i
    floatingUpdate _ i = i

instance Floating (OrderedListEdit edit) (OrderedListEdit edit) where
    floatingUpdate edit (OrderedListEditItem i e) = OrderedListEditItem (floatingUpdate edit i) e
    floatingUpdate edit (OrderedListEditDelete i) = OrderedListEditDelete (floatingUpdate edit i)
    floatingUpdate _edit OrderedListEditClear = OrderedListEditClear

type instance EditReader (OrderedListEdit edit) =
     ListReader (EditReader edit)

instance (FullSubjectReader (EditReader edit), ApplicableEdit edit) => ApplicableEdit (OrderedListEdit edit) where
    applyEdit (OrderedListEditItem p edit) mr (ListReadItem i reader)
        | p == i = getComposeInner $ applyEdit edit (itemReadFunction i mr) reader -- already checks bounds
    applyEdit (OrderedListEditItem _ _) mr reader = mr reader
    applyEdit (OrderedListEditDelete p) mr ListReadLength = do
        len <- mr ListReadLength
        return $
            if p >= 0 && p < len
                then pred len
                else len
    applyEdit (OrderedListEditDelete p) mr (ListReadItem i reader)
        | p >= 0 && p < i = mr $ ListReadItem (succ i) reader
    applyEdit (OrderedListEditDelete _) mr (ListReadItem i reader) = mr $ ListReadItem i reader
    applyEdit OrderedListEditClear _mr reader = subjectToReadable mempty reader

instance (SubjectReader (EditReader edit), SubjectMapEdit edit) => SubjectMapEdit (OrderedListEdit edit) where
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

data OrderedListUpdate update where
    OrderedListUpdateItem :: SequencePoint -> SequencePoint -> [update] -> OrderedListUpdate update
    OrderedListUpdateDelete :: SequencePoint -> OrderedListUpdate update
    OrderedListUpdateInsert :: SequencePoint -> UpdateSubject update -> OrderedListUpdate update
    OrderedListUpdateClear :: OrderedListUpdate update

type instance UpdateEdit (OrderedListUpdate update) =
     OrderedListEdit (UpdateEdit update)

instance FullSubjectReader (UpdateReader update) => FullUpdate (OrderedListUpdate update) where
    replaceUpdate rd push = do
        push OrderedListUpdateClear
        len <- rd ListReadLength
        for_ [0 .. pred len] $ \i -> do
            msubj <- getComposeInner $ readableToSubject $ itemReadFunction i rd
            case msubj of
                Just subj -> push $ OrderedListUpdateInsert i $ subj
                Nothing -> return ()

orderedListLengthLens :: forall update. ChangeLens (OrderedListUpdate update) (ROWUpdate SequencePoint)
orderedListLengthLens = let
    clRead :: ReadFunction (ListReader (UpdateReader update)) (WholeReader SequencePoint)
    clRead mr ReadWhole = mr ListReadLength
    clUpdate ::
           forall m. MonadIO m
        => OrderedListUpdate update
        -> Readable m (ListReader (UpdateReader update))
        -> m [ROWUpdate SequencePoint]
    clUpdate OrderedListUpdateClear _ = return $ pure $ MkReadOnlyUpdate $ MkWholeUpdate 0
    clUpdate (OrderedListUpdateItem _ _ _) _ = return []
    clUpdate _ mr = do
        i <- mr ListReadLength
        return $ pure $ MkReadOnlyUpdate $ MkWholeUpdate i
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

-- no "instance IsUpdate OrderedListUpdate", because we cannot calculate moves without knowing the order

-- | prevents creation of the element
orderedListItemLinearLens ::
       forall update. (FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => SequencePoint
    -> LinearFloatingChangeLens (StateLensVar SequencePoint) (OrderedListUpdate update) (MaybeUpdate update)
orderedListItemLinearLens initpos = let
    sclInit ::
           forall m. MonadIO m
        => Readable m (ListReader (UpdateReader update))
        -> m SequencePoint
    sclInit _ = return initpos
    sclRead ::
           ReadFunctionT (StateT SequencePoint) (ListReader (UpdateReader update)) (OneReader Maybe (UpdateReader update))
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
        => OrderedListUpdate update
        -> Readable m (ListReader (UpdateReader update))
        -> StateT SequencePoint m [MaybeUpdate update]
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
                put $ pred i
                return []
            EQ -> return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
            GT -> return []
    sclUpdate (OrderedListUpdateInsert ie _) _ = do
        i <- get
        if ie <= i
            then put $ succ i
            else return ()
        return []
    sclUpdate OrderedListUpdateClear _ = do
        put 0
        return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> StateT SequencePoint m (Maybe [OrderedListEdit (UpdateEdit update)])
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
        -> StateT SequencePoint m (Maybe [OrderedListEdit (UpdateEdit update)])
    sclPutEdits = linearPutEditsFromPutEdit sPutEdit
    in makeStateExpLens MkStateChangeLens {..}

-- | prevents creation of the element
orderedListItemLens ::
       forall update. (FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => SequencePoint
    -> FloatingChangeLens (OrderedListUpdate update) (MaybeUpdate update)
orderedListItemLens initpos = expToFloatingChangeLens $ orderedListItemLinearLens initpos

listOrderedListChangeLens ::
       forall update. (FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => ChangeLens (ListUpdate update) (OrderedListUpdate update)
listOrderedListChangeLens = let
    clRead :: ReadFunction (ListReader (UpdateReader update)) (ListReader (UpdateReader update))
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => ListUpdate update
        -> Readable m (ListReader (UpdateReader update))
        -> m [OrderedListUpdate update]
    clUpdate (ListUpdateItem p update) _ = return $ pure $ OrderedListUpdateItem p p [update]
    clUpdate (ListUpdateDelete p) _ = return $ pure $ OrderedListUpdateDelete p
    clUpdate (ListUpdateInsert p subj) _ = return $ pure $ OrderedListUpdateInsert p subj
    clUpdate ListUpdateClear _ = return $ pure OrderedListUpdateClear
    clPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit (UpdateEdit update)
        -> Readable m (ListReader (UpdateReader update))
        -> m (Maybe [ListEdit (UpdateEdit update)])
    clPutEdit (OrderedListEditItem p edit) _ = return $ Just $ pure $ ListEditItem p edit
    clPutEdit (OrderedListEditDelete p) _ = return $ Just $ pure $ ListEditDelete p
    clPutEdit OrderedListEditClear _ = return $ Just $ pure ListEditClear
    clPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit (UpdateEdit update)]
        -> Readable m (ListReader (UpdateReader update))
        -> m (Maybe [ListEdit (UpdateEdit update)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens {..}

liftOrderedListChangeLens ::
       forall updateA updateB.
       ( FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => ChangeLens updateA updateB
    -> ChangeLens (OrderedListUpdate updateA) (OrderedListUpdate updateB)
liftOrderedListChangeLens (MkChangeLens g up pe) = let
    clRead :: ReadFunction (ListReader (UpdateReader updateA)) (ListReader (UpdateReader updateB))
    clRead rd ListReadLength = rd ListReadLength
    clRead rd (ListReadItem i rb) = getComposeInner $ g (\ra -> MkComposeInner $ rd (ListReadItem i ra)) rb
    clUpdate ::
           forall m. MonadIO m
        => OrderedListUpdate updateA
        -> Readable m (ListReader (UpdateReader updateA))
        -> m [OrderedListUpdate updateB]
    clUpdate (OrderedListUpdateItem i1 i2 lu) rd = do
        u' <- for lu $ \u -> getComposeInner $ up u $ \ra -> MkComposeInner $ rd $ ListReadItem i1 ra
        return $
            case sequenceA u' of
                Nothing -> []
                Just ubb -> pure $ OrderedListUpdateItem i1 i2 $ mconcat ubb
    clUpdate (OrderedListUpdateDelete i) _ = return $ pure $ OrderedListUpdateDelete $ i
    clUpdate (OrderedListUpdateInsert i subjA) _ = do
        subjB <- readableToSubject $ g $ subjectToReadable subjA
        return $ pure $ OrderedListUpdateInsert i subjB
    clUpdate OrderedListUpdateClear _ = return $ pure OrderedListUpdateClear
    clPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit (UpdateEdit updateB)
        -> Readable m (ListReader (UpdateReader updateA))
        -> m (Maybe [OrderedListEdit (UpdateEdit updateA)])
    clPutEdit (OrderedListEditItem i editB) rd =
        getComposeInner $ do
            meditAs <- pe [editB] $ \ra -> MkComposeInner $ rd (ListReadItem i ra)
            editAs <- liftInner meditAs
            return $ fmap (OrderedListEditItem $ i) editAs
    clPutEdit (OrderedListEditDelete i) _ = return $ Just $ pure $ OrderedListEditDelete $ i
    clPutEdit OrderedListEditClear _ = return $ Just $ pure OrderedListEditClear
    clPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit (UpdateEdit updateB)]
        -> Readable m (ListReader (UpdateReader updateA))
        -> m (Maybe [OrderedListEdit (UpdateEdit updateA)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens {..}
