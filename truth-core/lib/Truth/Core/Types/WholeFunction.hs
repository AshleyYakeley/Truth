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
    ufGet :: ReadFunctionT IdentityT (WholeFunctionReader a p) (WholeFunctionReader a q)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        lift $ do
            p <- mr $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            return $ pq p
    ufUpdate ::
           forall m. MonadIO m
        => WholeFunctionUpdate a p
        -> MutableRead m (WholeFunctionReader a p)
        -> IdentityT m [WholeFunctionUpdate a q]
    ufUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate p)) _mr =
        lift $ return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pq p]
    elFunction :: AnUpdateFunction IdentityT (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeFunctionEdit a q]
        -> MutableRead m (WholeFunctionReader a p)
        -> IdentityT m (Maybe [WholeFunctionEdit a p])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit q)) ->
            return $ do
                p <- qmp q
                return [MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit p)]
    in MkRunnableT2 identityUntrans MkAnEditLens {..}

wholeFunctionPairEditLens ::
       forall a p q r.
       (p -> q -> r)
    -> (r -> Maybe (p, q))
    -> EditLens (PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionUpdate a r)
wholeFunctionPairEditLens pqr rmpq = let
    ufGet ::
           ReadFunctionT IdentityT (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionReader a r)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        lift $ do
            p <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            q <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            return $ pqr p q
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)
        -> MutableRead m (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q))
        -> IdentityT m [WholeFunctionUpdate a r]
    ufUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate p))) mr =
        lift $ do
            q <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pqr p q]
    ufUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate q))) mr =
        lift $ do
            p <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            return [MkTupleUpdate (MkFunctionSelector a) $ MkWholeUpdate $ pqr p q]
    elFunction ::
           AnUpdateFunction IdentityT (PairUpdate (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)) (WholeFunctionUpdate a r)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeFunctionEdit a r]
        -> MutableRead m (PairUpdateReader (WholeFunctionUpdate a p) (WholeFunctionUpdate a q))
        -> IdentityT m (Maybe [PairUpdateEdit (WholeFunctionUpdate a p) (WholeFunctionUpdate a q)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit r)) ->
            return $ do
                (p, q) <- rmpq r
                return
                    [ MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit p)
                    , MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit q)
                    ]
    in MkRunnableT2 identityUntrans MkAnEditLens {..}
