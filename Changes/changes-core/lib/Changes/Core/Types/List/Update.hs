module Changes.Core.Types.List.Update
    ( ListUpdate(..)
    , listLengthLens
    , listItemLinearLens
    , listItemLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Sequence
import Changes.Core.Types.List.Edit
import Changes.Core.Types.List.Read
import Changes.Core.Types.None
import Changes.Core.Types.One.FullResult
import Changes.Core.Types.One.Read
import Changes.Core.Types.One.Result
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Whole

data ListUpdate seq update where
    ListUpdateItem :: SequencePoint seq -> update -> ListUpdate seq update
    ListUpdateDelete :: SequencePoint seq -> ListUpdate seq update
    ListUpdateInsert :: SequencePoint seq -> UpdateSubject update -> ListUpdate seq update
    ListUpdateClear :: ListUpdate seq update

type instance UpdateEdit (ListUpdate seq update) =
     ListEdit seq (UpdateEdit update)

instance IsUpdate update => IsUpdate (ListUpdate seq update) where
    editUpdate (ListEditItem p edit) = ListUpdateItem p $ editUpdate edit
    editUpdate (ListEditDelete p) = ListUpdateDelete p
    editUpdate (ListEditInsert p subj) = ListUpdateInsert p subj
    editUpdate ListEditClear = ListUpdateClear

instance IsEditUpdate update => IsEditUpdate (ListUpdate seq update) where
    updateEdit (ListUpdateItem p update) = ListEditItem p $ updateEdit update
    updateEdit (ListUpdateDelete p) = ListEditDelete p
    updateEdit (ListUpdateInsert p subj) = ListEditInsert p subj
    updateEdit ListUpdateClear = ListEditClear

instance (IsSequence seq, FullSubjectReader (UpdateReader update)) => FullUpdate (ListUpdate seq update) where
    replaceUpdate mr write = do
        write ListUpdateClear
        len <- mr ListReadLength
        for_ [0 .. pred len] $ \i -> do
            item <- readableToSubject $ knownItemReadFunction i mr
            write $ ListUpdateInsert i item

listLengthLens ::
       forall seq update. (Num (Index seq))
    => ChangeLens (ListUpdate seq update) (ROWUpdate (SequencePoint seq))
listLengthLens = let
    clRead :: ReadFunction (ListReader seq (UpdateReader update)) (WholeReader (SequencePoint seq))
    clRead mr ReadWhole = mr ListReadLength
    clUpdate ::
           forall m. MonadIO m
        => ListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> m [ROWUpdate (SequencePoint seq)]
    clUpdate ListUpdateClear _ = return $ pure $ MkReadOnlyUpdate $ MkWholeUpdate 0
    clUpdate (ListUpdateItem _ _) _ = return []
    clUpdate _ mr = do
        i <- mr ListReadLength
        return $ pure $ MkReadOnlyUpdate $ MkWholeUpdate i
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

listItemLinearLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , FullEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => Bool
    -> SequencePoint seq
    -> LinearFloatingChangeLens (StateLensVar (Bool, Index seq)) (ListUpdate seq update) (MaybeUpdate update)
listItemLinearLens initpresent (MkSequencePoint initpos) = let
    sclInit ::
           forall m. MonadIO m
        => Readable m (ListReader seq (UpdateReader update))
        -> m (Bool, Index seq)
    sclInit _ = return (initpresent, initpos)
    getSP ::
           forall m. Monad m
        => StateT (Bool, Index seq) m (Bool, SequencePoint seq)
    getSP = do
        (present, i) <- get
        return (present, MkSequencePoint i)
    putSP ::
           forall m. Monad m
        => Bool
        -> SequencePoint seq
        -> StateT (Bool, Index seq) m ()
    putSP present (MkSequencePoint i) = put (present, i)
    sclRead ::
           ReadFunctionT (StateT (Bool, Index seq)) (ListReader seq (UpdateReader update)) (OneReader Maybe (UpdateReader update))
    sclRead mr (ReadOne rt) = do
        (present, i) <- getSP
        if present
            then lift $ mr $ ListReadItem i rt
            else return Nothing
    sclRead mr ReadHasOne = do
        (present, i) <- getSP
        if present
            then if i < 0
                     then return Nothing
                     else do
                         len <- lift $ mr ListReadLength
                         return $
                             if i >= len
                                 then Nothing
                                 else Just ()
            else return Nothing
    sclUpdate ::
           forall m. MonadIO m
        => ListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (Bool, Index seq) m [MaybeUpdate update]
    sclUpdate (ListUpdateItem ie update) _ = do
        (present, i) <- getSP
        return $
            if present && i == ie
                then [MkFullResultOneUpdate $ SuccessResultOneUpdate update]
                else []
    sclUpdate (ListUpdateDelete ie) _ = do
        (present, i) <- getSP
        case (present, compare ie i) of
            (_, LT) -> do
                putSP present $ pred i
                return []
            (True, EQ) -> do
                putSP False i
                return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
            _ -> return []
    sclUpdate (ListUpdateInsert ie _) _ = do
        (present, i) <- getSP
        case (present, compare ie i) of
            (_, GT) -> return []
            (False, EQ) -> do
                putSP True i
                return [MkFullResultOneUpdate $ NewResultOneUpdate $ Just ()]
            _ -> do
                putSP present $ succ i
                return []
    sclUpdate ListUpdateClear _ = do
        putSP False 0
        return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> StateT (Bool, Index seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    sPutEdit (SuccessFullResultOneEdit edit) = do
        (present, i) <- getSP
        return $
            Just $
            if present
                then [ListEditItem i edit]
                else []
    sPutEdit (NewFullResultOneEdit Nothing) = do
        (present, i) <- getSP
        if present
            then do
                putSP False i
                return $ Just [ListEditDelete i]
            else return $ Just []
    sPutEdit (NewFullResultOneEdit (Just subj)) = do
        (present, i) <- getSP
        if present
            then do
                edits <- getReplaceEditsFromSubject subj
                return $ Just $ fmap (ListEditItem i) edits
            else do
                putSP True i
                return $ Just [ListEditInsert i subj]
    sclPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> Readable m NullReader
        -> StateT (Bool, Index seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    sclPutEdits = linearPutEditsFromPutEdit sPutEdit
    in makeStateExpLens MkStateChangeLens {..}

listItemLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , FullEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => Bool
    -> SequencePoint seq
    -> FloatingChangeLens (ListUpdate seq update) (MaybeUpdate update)
listItemLens present initpos = expToFloatingChangeLens $ listItemLinearLens present initpos
