{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.ConsTuple where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

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
    ufGet :: ReadFunctionT IdentityT (UpdateReader updateA) (TupleUpdateReader EmptyType)
    ufGet _ (MkTupleUpdateReader sel _) = never sel
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m [TupleUpdate EmptyType]
    ufUpdate _ _ = return []
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit EmptyType]
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m (Maybe [UpdateEdit updateA])
    elPutEdits [] _ = return $ Just []
    elPutEdits ((MkTupleUpdateEdit sel _):_) _ = never sel
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

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
    ufGet :: ReadFunctionT IdentityT (TupleUpdateReader (ConsType update1 sel)) (UpdateReader update1)
    ufGet mr rt = lift $ mr $ MkTupleUpdateReader FirstType rt
    ufUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> IdentityT m [update1]
    ufUpdate (MkTupleUpdate FirstType update) _ = return [update]
    ufUpdate (MkTupleUpdate (RestType _) _) _ = return []
    elFunction :: AnUpdateFunction IdentityT (TupleUpdate (ConsType update1 sel)) update1
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update1]
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> IdentityT m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    elPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit FirstType) edits
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

restEditLens :: forall sel update1. EditLens (TupleUpdate (ConsType update1 sel)) (TupleUpdate sel)
restEditLens = let
    ufGet :: ReadFunctionT IdentityT (TupleUpdateReader (ConsType update1 sel)) (TupleUpdateReader sel)
    ufGet mr (MkTupleUpdateReader sel rt) = lift $ mr $ MkTupleUpdateReader (RestType sel) rt
    ufUpdate ::
           forall m. MonadIO m
        => TupleUpdate (ConsType update1 sel)
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> IdentityT m [TupleUpdate sel]
    ufUpdate (MkTupleUpdate FirstType _) _ = return []
    ufUpdate (MkTupleUpdate (RestType sel) edit) _ = return [MkTupleUpdate sel edit]
    elFunction :: AnUpdateFunction IdentityT (TupleUpdate (ConsType update1 sel)) (TupleUpdate sel)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit sel]
        -> MutableRead m (TupleUpdateReader (ConsType update1 sel))
        -> IdentityT m (Maybe [TupleUpdateEdit (ConsType update1 sel)])
    elPutEdits edits _ =
        return $ Just $ fmap (\(MkTupleUpdateEdit sel edit) -> MkTupleUpdateEdit (RestType sel) edit) edits
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

consTuple :: UpdateSubject a -> Tuple r -> Tuple (ConsType a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            FirstType -> a
            RestType sel -> tup sel
