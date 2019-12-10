module Truth.Core.Types.WholeFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Function
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

type WholeFunctionUpdate a b = FunctionUpdate a (WholeUpdate b)

type WholeFunctionEdit a b = UpdateEdit (WholeFunctionUpdate a b)

type WholeFunctionReader a b = UpdateReader (WholeFunctionUpdate a b)

wholeFunctionMapEditLens ::
       forall a p q. (p -> q) -> (q -> Maybe p) -> EditLens (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
wholeFunctionMapEditLens pq qmp = let
    ufGet :: ReadFunction (WholeFunctionReader a p) (WholeFunctionReader a q)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        p <- mr $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return $ pq p
    ufUpdate ::
           forall m. MonadIO m
        => WholeFunctionUpdate a p
        -> MutableRead m (WholeFunctionReader a p)
        -> m [WholeFunctionUpdate a q]
    ufUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate p)) _mr =
        return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pq p]
    elFunction :: UpdateFunction (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeFunctionEdit a q]
        -> MutableRead m (WholeFunctionReader a p)
        -> m (Maybe [WholeFunctionEdit a p])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit q)) ->
            return $ do
                p <- qmp q
                return [MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit p)]
    in MkEditLens {..}

wholeFunctionPairEditLens ::
       forall a p q r.
       (p -> q -> r)
    -> (r -> Maybe (p, q))
    -> EditLens (PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionUpdate a r)
wholeFunctionPairEditLens pqr rmpq = let
    ufGet ::
           ReadFunction (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionReader a r)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        p <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        q <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return $ pqr p q
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
        -> MutableRead m (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q))
        -> m [WholeFunctionUpdate a r]
    ufUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate p))) mr = do
        q <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pqr p q]
    ufUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate q))) mr = do
        p <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pqr p q]
    elFunction ::
           UpdateFunction (PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionUpdate a r)
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeFunctionEdit a r]
        -> MutableRead m (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q))
        -> m (Maybe [PairUpdateEdit (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit r)) ->
            return $ do
                (p, q) <- rmpq r
                return
                    [ MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit p)
                    , MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit q)
                    ]
    in MkEditLens {..}
