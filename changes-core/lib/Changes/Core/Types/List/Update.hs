module Changes.Core.Types.List.Update
    ( ListUpdate(..)
    , listItemLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Sequence
import Changes.Core.Types.List.Edit
import Changes.Core.Types.List.Read
import Changes.Core.Types.One.FullResult
import Changes.Core.Types.One.Read
import Changes.Core.Types.One.Result

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

listItemLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => SequencePoint seq
    -> FloatingChangeLens (ListUpdate seq update) (MaybeUpdate update)
listItemLens initpos = let
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
        => ListUpdate seq update
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m [MaybeUpdate update]
    sclUpdate (ListUpdateItem ie update) _ = do
        i <- get
        return $
            if i == ie
                then [MkFullResultOneUpdate $ SuccessResultOneUpdate update]
                else []
    sclUpdate (ListUpdateDelete ie) _ = do
        i <- get
        case compare ie i of
            LT -> do
                put $ i - 1
                return []
            EQ -> return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
            GT -> return []
    sclUpdate (ListUpdateInsert ie _) _ = do
        i <- get
        if ie <= i
            then put $ i + 1
            else return ()
        return []
    sclUpdate ListUpdateClear _ = do
        put 0
        return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    sPutEdit (SuccessFullResultOneEdit edit) _ = do
        i <- get
        return $ Just [ListEditItem i edit]
    sPutEdit (NewFullResultOneEdit Nothing) _ = do
        i <- get
        return $ Just [ListEditDelete i]
    sPutEdit (NewFullResultOneEdit (Just subj)) _ = do
        i <- get
        return $ Just [ListEditInsert i subj]
    sclPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> Readable m (ListReader seq (UpdateReader update))
        -> StateT (SequencePoint seq) m (Maybe [ListEdit seq (UpdateEdit update)])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens MkStateChangeLens {..}
