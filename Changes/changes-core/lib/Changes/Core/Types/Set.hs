module Changes.Core.Types.Set where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.Partial
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Tuple.Function
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Whole
import Changes.Core.Types.WholeFunction

type SetUpdate a = WholeFunctionUpdate a Bool

type SetEdit a = UpdateEdit (SetUpdate a)

type SetReader a = UpdateReader (SetUpdate a)

setUpdateComplement :: ChangeLens (SetUpdate a) (SetUpdate a)
setUpdateComplement = wholeFunctionMapChangeLens not $ Just . not

setUpdateUnion :: ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateUnion =
    wholeFunctionPairChangeLens (||) $ \r ->
        if r
            then Nothing
            else Just (False, False)

setUpdateIntersection :: ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateIntersection =
    wholeFunctionPairChangeLens (&&) $ \r ->
        if r
            then Just (True, True)
            else Nothing

setUpdateDifference :: ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateDifference =
    wholeFunctionPairChangeLens (\a b -> a && not b) $ \r ->
        if r
            then Just (True, False)
            else Nothing

setUpdateSymmetricDifference :: ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateSymmetricDifference = wholeFunctionPairChangeLens (/=) $ \_ -> Nothing

setCartesianSumChangeLens :: forall a b. ChangeLens (PairUpdate (SetUpdate a) (SetUpdate b)) (SetUpdate (Either a b))
setCartesianSumChangeLens = functionEitherPairChangeLens

type PartialSetUpdate a = PartialUpdate (SetUpdate a)

setCartesianProductLens ::
       forall a b.
       (a -> a -> Bool)
    -> (b -> b -> Bool)
    -> ChangeLens (PairUpdate (SetUpdate a) (SetUpdate b)) (ReadOnlyUpdate (PartialSetUpdate (a, b)))
setCartesianProductLens aeq beq = let
    clRead :: ReadFunction (PairUpdateReader (SetUpdate a) (SetUpdate b)) (SetReader (a, b))
    clRead mr (MkTupleUpdateReader (MkFunctionSelector (a, b)) ReadWhole) = do
        hasA <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        hasB <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
        return $ hasA && hasB
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (SetUpdate a) (SetUpdate b)
        -> Readable m (PairUpdateReader (SetUpdate a) (SetUpdate b))
        -> m [ReadOnlyUpdate (PartialSetUpdate (a, b))]
    clUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) _)) _ =
        return $
        pure $
        MkReadOnlyUpdate $
        UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector (a', _)) ReadWhole) -> aeq a a'
    clUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector b) _)) _ =
        return $
        pure $
        MkReadOnlyUpdate $
        UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector (_, b')) ReadWhole) -> beq b b'
    clPutEdits ::
           forall m. MonadIO m
        => [ConstEdit (SetReader (a, b))]
        -> Readable m (PairUpdateReader (SetUpdate a) (SetUpdate b))
        -> m (Maybe [PairUpdateEdit (SetUpdate a) (SetUpdate b)])
    clPutEdits = clPutEditsNone
    in MkChangeLens {..}

setCartesianProductPartialLens ::
       forall a b.
       (a -> a -> Bool)
    -> (b -> b -> Bool)
    -> ChangeLens (PairUpdate (PartialSetUpdate a) (PartialSetUpdate b)) (ReadOnlyUpdate (PartialSetUpdate (a, b)))
setCartesianProductPartialLens aeq beq = let
    setmap :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (a, b))
    setmap rset (MkTupleUpdateReader (MkFunctionSelector (a, b)) ReadWhole) =
        rset (MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) ||
        rset (MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole)
    in partialiseReadOnlyChangeLens setmap (setCartesianProductLens aeq beq) . partialPairChangeLens
