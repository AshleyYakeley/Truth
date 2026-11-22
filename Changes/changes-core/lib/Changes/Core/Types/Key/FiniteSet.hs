{-# OPTIONS -fno-warn-orphans #-}

module Changes.Core.Types.Key.FiniteSet where

import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Key.Map
import Changes.Core.Types.Lattice
import Changes.Core.Types.None
import Changes.Core.Types.One.FullResult
import Changes.Core.Types.Partial
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Set
import Changes.Core.Types.Tuple.Function
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Whole

-- | A finite set is a map with keys as elements and no readable or editable values.
type FiniteSetReader subj = MapReader' subj (NoUpdate ()) ()

type FiniteSetEdit subj = MapEdit' subj (NoUpdate ()) ()

type FiniteSetUpdate subj = MapUpdate' subj (NoUpdate ()) ()

wholeFiniteSetReadFunction :: Eq subj => ReadFunction (WholeReader (ListMap subj ())) (FiniteSetReader subj)
wholeFiniteSetReadFunction = wholeMapReadFunction

finiteSetChangeLens :: forall subj. Equivalence subj -> subj -> ChangeLens (FiniteSetUpdate subj) (WholeUpdate Bool)
finiteSetChangeLens eqv subj = maybeExistsChangeLens . mapElementChangeLens eqv subj

instance Eq subj => JoinSemiLatticeReadOnlyChangeLens (FiniteSetUpdate subj) where
    joinChangeLens =
        toReadOnlyChangeLens
            . liftMapElementChangeLens (\() -> pure Nothing) (constChangeLens ())
            . alignMapChangeLens

instance Eq subj => MeetSemiLatticeReadOnlyChangeLens (FiniteSetUpdate subj) where
    meetChangeLens =
        liftReadOnlyChangeLens
            (toReadOnlyChangeLens . liftMapElementChangeLens (\() -> pure Nothing) (constChangeLens ()))
            . mapMaybeMapChangeLens
                ( \case
                    These () () -> Just ()
                    _ -> Nothing
                )
            . alignMapChangeLens

injectiveMapFiniteSetChangeLens :: forall a b. Codec a b -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
injectiveMapFiniteSetChangeLens = injectiveMapKeysChangeLens

collectFiniteSetChangeLens ::
    (b -> a) -> ChangeLens (PairUpdate (ROWUpdate (a -> Maybe b)) (FiniteSetUpdate a)) (FiniteSetUpdate b)
collectFiniteSetChangeLens = collectMapKeysChangeLens

filterFiniteSetChangeLens :: forall a. (a -> Bool) -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate a)
filterFiniteSetChangeLens = filterMapKeysChangeLens

codecFiniteSetChangeLens :: forall a b. Codec a b -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
codecFiniteSetChangeLens = codecMapKeysChangeLens

bijectionFiniteSetChangeLens :: forall a b. Bijection a b -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
bijectionFiniteSetChangeLens = bijectionMapKeysChangeLens

finiteSetCartesianSumChangeLens ::
    forall a b. ChangeLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
finiteSetCartesianSumChangeLens = mapCartesianSumChangeLens

finiteSetCartesianProductUpdateFunction ::
    forall a b.
    ChangeLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (ReadOnlyUpdate (FiniteSetUpdate (a, b)))
finiteSetCartesianProductUpdateFunction = let
    clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (a, b))
    clRead _ (MapReadValue _ rt) = never rt
    clRead mr MapReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst MapReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond MapReadKeys
        return $ aa <***> bb
    clRead mr (MapReadKey (a, b)) =
        unComposeInner $ do
            a' <- MkComposeInner $ mr $ MkTupleUpdateReader SelectFirst $ MapReadKey a
            b' <- MkComposeInner $ mr $ MkTupleUpdateReader SelectSecond $ MapReadKey b
            return (a', b')
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b) ->
        Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) ->
        m [ReadOnlyUpdate (FiniteSetUpdate (a, b))]
    clUpdate (MkTupleUpdate SelectFirst MapUpdateClear) _ = return [MkReadOnlyUpdate MapUpdateClear]
    clUpdate (MkTupleUpdate SelectSecond MapUpdateClear) _ = return [MkReadOnlyUpdate MapUpdateClear]
    clUpdate (MkTupleUpdate SelectFirst (MapUpdateValue _ update)) _ = never update
    clUpdate (MkTupleUpdate SelectSecond (MapUpdateValue _ update)) _ = never update
    clUpdate (MkTupleUpdate SelectFirst (MapUpdateDelete a)) mr = do
        bb <- mr $ MkTupleUpdateReader SelectSecond MapReadKeys
        return $ fmap (\b -> MkReadOnlyUpdate $ MapUpdateDelete (a, b)) $ toList bb
    clUpdate (MkTupleUpdate SelectSecond (MapUpdateDelete b)) mr = do
        aa <- mr $ MkTupleUpdateReader SelectFirst MapReadKeys
        return $ fmap (\a -> MkReadOnlyUpdate $ MapUpdateDelete (a, b)) $ toList aa
    clUpdate (MkTupleUpdate SelectFirst (MapUpdateSet a ())) mr = do
        bb <- mr $ MkTupleUpdateReader SelectSecond MapReadKeys
        return $ fmap (\b -> MkReadOnlyUpdate $ MapUpdateSet (a, b) ()) $ toList bb
    clUpdate (MkTupleUpdate SelectSecond (MapUpdateSet b ())) mr = do
        aa <- mr $ MkTupleUpdateReader SelectFirst MapReadKeys
        return $ fmap (\a -> MkReadOnlyUpdate $ MapUpdateSet (a, b) ()) $ toList aa
    in MkChangeLens{clPutEdits = clPutEditsNone, ..}

finiteSetFunctionChangeLens :: forall a. ChangeLens (FiniteSetUpdate a) (PartialSetUpdate a)
finiteSetFunctionChangeLens = let
    clRead :: ReadFunction (FiniteSetReader a) (FunctionUpdateReader a (WholeUpdate Bool))
    clRead mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        mu <- mr $ MapReadKey a
        return $ isJust mu
    clUpdate ::
        forall m.
        MonadIO m =>
        FiniteSetUpdate a ->
        Readable m (FiniteSetReader a) ->
        m [PartialUpdate (FunctionUpdate a (WholeUpdate Bool))]
    clUpdate (MapUpdateValue _ update) _ = never update
    clUpdate (MapUpdateDelete p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate False)]
    clUpdate (MapUpdateSet p ()) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate True)]
    clUpdate MapUpdateClear _ = return [UnknownPartialUpdate $ \_ -> True]
    clPutEdit ::
        forall m.
        MonadIO m =>
        FunctionUpdateEdit a (WholeUpdate Bool) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdit (MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit b)) =
        return
            $ Just
                [ if b
                    then MapEditSet a ()
                    else MapEditDelete a
                ]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FunctionUpdateEdit a (WholeUpdate Bool)] ->
        Readable m (FiniteSetReader a) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdits = clPutEditsFromSimplePutEdit clPutEdit
    in MkChangeLens{..}

filterFiniteSetUpdateFunction ::
    forall a. ChangeLens (PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)) (ReadOnlyUpdate (FiniteSetUpdate a))
filterFiniteSetUpdateFunction = filterMapUpdateFunction
