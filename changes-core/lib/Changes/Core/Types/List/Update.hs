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
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => SequencePoint seq
    -> LinearFloatingChangeLens (StateLensVar (Index seq)) (ListUpdate seq update) (MaybeUpdate update)
listItemLinearLens (MkSequencePoint initpos) = let
    sclInit ::
           forall m. MonadIO m
        => Readable m (ListReader seq (UpdateReader update))
        -> m (Index seq)
    sclInit _ = return initpos
    getSP ::
           forall m. Monad m
        => StateT (Index seq) m (SequencePoint seq)
    getSP = fmap MkSequencePoint get
    putSP ::
           forall m. Monad m
        => SequencePoint seq
        -> StateT (Index seq) m ()
    putSP (MkSequencePoint i) = put i
    sclRead ::
           ReadFunctionT (StateT (Index seq)) (ListReader seq (UpdateReader update)) (OneReader Maybe (UpdateReader update))
    sclRead mr (ReadOne rt) = do
        i <- getSP
        lift $ mr $ ListReadItem i rt
    sclRead mr ReadHasOne = do
        i <- getSP
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
        => ListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (Index seq) m [MaybeUpdate update]
    sclUpdate (ListUpdateItem ie update) _ = do
        i <- getSP
        return $
            if i == ie
                then [MkFullResultOneUpdate $ SuccessResultOneUpdate update]
                else []
    sclUpdate (ListUpdateDelete ie) _ = do
        i <- getSP
        case compare ie i of
            LT -> do
                putSP $ pred i
                return []
            EQ -> return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
            GT -> return []
    sclUpdate (ListUpdateInsert ie _) _ = do
        i <- getSP
        if ie <= i
            then putSP $ succ i
            else return ()
        return []
    sclUpdate ListUpdateClear _ = do
        put 0
        return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> StateT (Index seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    sPutEdit (SuccessFullResultOneEdit edit) = do
        i <- getSP
        return $ Just [ListEditItem i edit]
    sPutEdit (NewFullResultOneEdit Nothing) = do
        i <- getSP
        return $ Just [ListEditDelete i]
    sPutEdit (NewFullResultOneEdit (Just subj)) = do
        i <- getSP
        return $ Just [ListEditInsert i subj]
    sclPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> Readable m NullReader
        -> StateT (Index seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    sclPutEdits = linearPutEditsFromPutEdit sPutEdit
    in makeStateExpLens MkStateChangeLens {..}

listItemLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => SequencePoint seq
    -> FloatingChangeLens (ListUpdate seq update) (MaybeUpdate update)
listItemLens initpos = expToFloatingChangeLens $ listItemLinearLens initpos
