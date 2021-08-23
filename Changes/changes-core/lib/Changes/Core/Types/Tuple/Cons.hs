{-# OPTIONS -fno-warn-orphans #-}

module Changes.Core.Types.Tuple.Cons where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Tuple.Tuple

instance TupleUpdateWitness c EmptyType where
    tupleUpdateWitness = never

instance SubjectTupleSelector EmptyType

instance TupleReaderWitness c EmptyType where
    tupleReaderWitness = never

instance FiniteTupleSelector EmptyType where
    tupleConstruct _ = pure $ MkTuple never

emptyTuple :: Tuple EmptyType
emptyTuple = MkTuple never

emptyTupleLens :: forall updateA. ChangeLens updateA (TupleUpdate EmptyType)
emptyTupleLens = let
    clRead :: ReadFunction (UpdateReader updateA) (TupleUpdateReader EmptyType)
    clRead _ (MkTupleUpdateReader sel _) = never sel
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [TupleUpdate EmptyType]
    clUpdate _ _ = return []
    clPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit EmptyType]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits [] _ = return $ Just []
    clPutEdits ((MkTupleUpdateEdit sel _):_) _ = never sel
    in MkChangeLens {..}

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

firstChangeLens :: forall sel update1. ChangeLens (TupleUpdate (ConsType update1 sel)) update1
firstChangeLens = let
    clRead :: ReadFunction (TupleUpdateReader (ConsType update1 sel)) (UpdateReader update1)
    clRead mr rt = mr $ MkTupleUpdateReader FirstType rt
    clUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m [update1]
    clUpdate (MkTupleUpdate FirstType update) _ = return [update]
    clUpdate (MkTupleUpdate (RestType _) _) _ = return []
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update1]
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    clPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit FirstType) edits
    in MkChangeLens {..}

restChangeLens :: forall sel update1. ChangeLens (TupleUpdate (ConsType update1 sel)) (TupleUpdate sel)
restChangeLens = let
    clRead :: ReadFunction (TupleUpdateReader (ConsType update1 sel)) (TupleUpdateReader sel)
    clRead mr (MkTupleUpdateReader sel rt) = mr $ MkTupleUpdateReader (RestType sel) rt
    clUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m [TupleUpdate sel]
    clUpdate (MkTupleUpdate FirstType _) _ = return []
    clUpdate (MkTupleUpdate (RestType sel) edit) _ = return [MkTupleUpdate sel edit]
    clPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit sel]
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    clPutEdits edits _ =
        return $ Just $ fmap (\(MkTupleUpdateEdit sel edit) -> MkTupleUpdateEdit (RestType sel) edit) edits
    in MkChangeLens {..}

consTuple :: UpdateSubject a -> Tuple r -> Tuple (ConsType a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            FirstType -> a
            RestType sel -> tup sel
