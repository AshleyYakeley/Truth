module Truth.Core.Types.WholeFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Tuple.Function
import Truth.Core.Types.Tuple.Pair
import Truth.Core.Types.Tuple.Tuple
import Truth.Core.Types.Whole

type WholeFunctionUpdate a b = FunctionUpdate a (WholeUpdate b)

type WholeFunctionEdit a b = UpdateEdit (WholeFunctionUpdate a b)

type WholeFunctionReader a b = UpdateReader (WholeFunctionUpdate a b)

wholeFunctionMapChangeLens ::
       forall a p q. (p -> q) -> (q -> Maybe p) -> ChangeLens (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
wholeFunctionMapChangeLens pq qmp = let
    clRead :: ReadFunction (WholeFunctionReader a p) (WholeFunctionReader a q)
    clRead mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        p <- mr $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return $ pq p
    clUpdate ::
           forall m. MonadIO m
        => WholeFunctionUpdate a p
        -> Readable m (WholeFunctionReader a p)
        -> m [WholeFunctionUpdate a q]
    clUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate p)) _mr =
        return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pq p]
    clPutEdits ::
           forall m. MonadIO m
        => [WholeFunctionEdit a q]
        -> Readable m (WholeFunctionReader a p)
        -> m (Maybe [WholeFunctionEdit a p])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit q)) ->
            return $ do
                p <- qmp q
                return [MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit p)]
    in MkChangeLens {..}

wholeFunctionPairChangeLens ::
       forall a p q r.
       (p -> q -> r)
    -> (r -> Maybe (p, q))
    -> ChangeLens (PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionUpdate a r)
wholeFunctionPairChangeLens pqr rmpq = let
    clRead ::
           ReadFunction (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionReader a r)
    clRead mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        p <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        q <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return $ pqr p q
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
        -> Readable m (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q))
        -> m [WholeFunctionUpdate a r]
    clUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate p))) mr = do
        q <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pqr p q]
    clUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate q))) mr = do
        p <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pqr p q]
    clPutEdits ::
           forall m. MonadIO m
        => [WholeFunctionEdit a r]
        -> Readable m (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q))
        -> m (Maybe [PairUpdateEdit (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit r)) ->
            return $ do
                (p, q) <- rmpq r
                return
                    [ MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit p)
                    , MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit q)
                    ]
    in MkChangeLens {..}
