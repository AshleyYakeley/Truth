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

instance (TupleUpdateWitness c a, TupleUpdateWitness c b) => TupleUpdateWitness c (EitherType a b) where
    tupleUpdateWitness (LeftType r) = tupleUpdateWitness r
    tupleUpdateWitness (RightType r) = tupleUpdateWitness r

instance (TestEquality a, TupleReaderWitness SubjectReader a, TestEquality b, TupleReaderWitness SubjectReader b) =>
             SubjectTupleSelector (EitherType a b)

instance (TupleReaderWitness c a, TupleReaderWitness c b) => TupleReaderWitness c (EitherType a b) where
    tupleReaderWitness (LeftType r) = tupleReaderWitness r
    tupleReaderWitness (RightType r) = tupleReaderWitness r

instance (FiniteTupleSelector a, TupleSubject a ~ Tuple a, FiniteTupleSelector b, TupleSubject b ~ Tuple b) =>
             FiniteTupleSelector (EitherType a b) where
    tupleConstruct getsel =
        (\(MkTuple ra) (MkTuple rb) ->
             MkTuple $ \sel ->
                 case sel of
                     LeftType rt -> ra rt
                     RightType rt -> rb rt) <$>
        tupleConstruct (getsel . LeftType) <*>
        tupleConstruct (getsel . RightType)

firstChangeLens :: forall sel update1. ChangeLens (TupleUpdate (ConsType update1 sel)) update1
firstChangeLens = let
    clRead :: ReadFunction (TupleUpdateReader (ConsType update1 sel)) (UpdateReader update1)
    clRead mr rt = mr $ MkTupleUpdateReader (LeftType Refl) rt
    clUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m [update1]
    clUpdate (MkTupleUpdate (LeftType Refl) update) _ = return [update]
    clUpdate (MkTupleUpdate (RightType _) _) _ = return []
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update1]
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    clPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit (LeftType Refl)) edits
    in MkChangeLens {..}

restChangeLens :: forall sel update1. ChangeLens (TupleUpdate (ConsType update1 sel)) (TupleUpdate sel)
restChangeLens = let
    clRead :: ReadFunction (TupleUpdateReader (ConsType update1 sel)) (TupleUpdateReader sel)
    clRead mr (MkTupleUpdateReader sel rt) = mr $ MkTupleUpdateReader (RightType sel) rt
    clUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m [TupleUpdate sel]
    clUpdate (MkTupleUpdate (LeftType Refl) _) _ = return []
    clUpdate (MkTupleUpdate (RightType sel) edit) _ = return [MkTupleUpdate sel edit]
    clPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit sel]
        -> Readable m (TupleUpdateReader (ConsType update1 sel))
        -> m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    clPutEdits edits _ =
        return $ Just $ fmap (\(MkTupleUpdateEdit sel edit) -> MkTupleUpdateEdit (RightType sel) edit) edits
    in MkChangeLens {..}

consTuple :: UpdateSubject a -> Tuple r -> Tuple (ConsType a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            (LeftType Refl) -> a
            RightType sel -> tup sel
