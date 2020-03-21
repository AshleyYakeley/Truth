{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Tuple.Cons where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Tuple.Tuple

instance TupleUpdateWitness c EmptyType where
    tupleUpdateWitness = never

instance SubjectTupleSelector EmptyType

instance TupleReaderWitness c EmptyType where
    tupleReaderWitness = never

instance FiniteTupleSelector EmptyType where
    tupleConstruct _ = pure $ MkTuple never

emptyTuple :: Tuple EmptyType
emptyTuple = MkTuple never

emptyTupleLens :: forall updateA. EditLens updateA (TupleUpdate EmptyType)
emptyTupleLens = let
    elGet :: ReadFunction (UpdateReader updateA) (TupleUpdateReader EmptyType)
    elGet _ (MkTupleUpdateReader sel _) = never sel
    elUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [TupleUpdate EmptyType]
    elUpdate _ _ = return []
    elPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit EmptyType]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits [] _ = return $ Just []
    elPutEdits ((MkTupleUpdateEdit sel _):_) _ = never sel
    in MkEditLens {..}

instance (c a, TupleUpdateWitness c r) => TupleUpdateWitness c (ConsType a r) where
    tupleUpdateWitness FirstType = Dict
    tupleUpdateWitness (RestType r) = tupleUpdateWitness r

instance (SubjectReader (UpdateReader a), TestEquality r, TupleReaderWitness SubjectReader r) =>
             SubjectTupleSelector (ConsType a r)

instance (c (UpdateReader a), TupleReaderWitness c r) => TupleReaderWitness c (ConsType a r) where
    tupleReaderWitness FirstType = Dict
    tupleReaderWitness (RestType r) = tupleReaderWitness r

instance (FiniteTupleSelector r, TupleSubject r ~ Tuple r) => FiniteTupleSelector (ConsType a r) where
    tupleConstruct getsel =
        (\f (MkTuple r) ->
             MkTuple $ \sel ->
                 case sel of
                     FirstType -> f
                     RestType rt -> r rt) <$>
        getsel FirstType <*>
        tupleConstruct (getsel . RestType)

firstEditLens :: forall sel update1. EditLens (TupleUpdate (ConsType update1 sel)) update1
firstEditLens = let
    elGet :: ReadFunction (TupleUpdateReader (ConsType update1 sel)) (UpdateReader update1)
    elGet mr rt = mr $ MkTupleUpdateReader FirstType rt
    elUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> m [update1]
    elUpdate (MkTupleUpdate FirstType update) _ = return [update]
    elUpdate (MkTupleUpdate (RestType _) _) _ = return []
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update1]
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    elPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit FirstType) edits
    in MkEditLens {..}

restEditLens :: forall sel update1. EditLens (TupleUpdate (ConsType update1 sel)) (TupleUpdate sel)
restEditLens = let
    elGet :: ReadFunction (TupleUpdateReader (ConsType update1 sel)) (TupleUpdateReader sel)
    elGet mr (MkTupleUpdateReader sel rt) = mr $ MkTupleUpdateReader (RestType sel) rt
    elUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> m [TupleUpdate sel]
    elUpdate (MkTupleUpdate FirstType _) _ = return []
    elUpdate (MkTupleUpdate (RestType sel) edit) _ = return [MkTupleUpdate sel edit]
    elPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit sel]
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    elPutEdits edits _ =
        return $ Just $ fmap (\(MkTupleUpdateEdit sel edit) -> MkTupleUpdateEdit (RestType sel) edit) edits
    in MkEditLens {..}

consTuple :: UpdateSubject a -> Tuple r -> Tuple (ConsType a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            FirstType -> a
            RestType sel -> tup sel
