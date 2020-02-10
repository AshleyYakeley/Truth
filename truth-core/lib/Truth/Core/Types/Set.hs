module Truth.Core.Types.Set where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Function
import Truth.Core.Types.None
import Truth.Core.Types.Pair
import Truth.Core.Types.Partial
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole
import Truth.Core.Types.WholeFunction

type SetUpdate a = WholeFunctionUpdate a Bool

type SetEdit a = UpdateEdit (SetUpdate a)

type SetReader a = UpdateReader (SetUpdate a)

setUpdateComplement :: EditLens (SetUpdate a) (SetUpdate a)
setUpdateComplement = wholeFunctionMapEditLens not $ Just . not

setUpdateUnion :: EditLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateUnion =
    wholeFunctionPairEditLens (||) $ \r ->
        if r
            then Nothing
            else Just (False, False)

setUpdateIntersection :: EditLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateIntersection =
    wholeFunctionPairEditLens (&&) $ \r ->
        if r
            then Just (True, True)
            else Nothing

setUpdateDifference :: EditLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateDifference =
    wholeFunctionPairEditLens (\a b -> a && not b) $ \r ->
        if r
            then Just (True, False)
            else Nothing

setUpdateSymmetricDifference :: EditLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
setUpdateSymmetricDifference = wholeFunctionPairEditLens (/=) $ \_ -> Nothing

setCartesianSumEditLens :: forall a b. EditLens (PairUpdate (SetUpdate a) (SetUpdate b)) (SetUpdate (Either a b))
setCartesianSumEditLens = functionEitherPairEditLens

type PartialSetUpdate a = PartialUpdate (SetUpdate a)

setCartesianProductLens ::
       forall a b.
       (a -> a -> Bool)
    -> (b -> b -> Bool)
    -> EditLens (PairUpdate (SetUpdate a) (SetUpdate b)) (ReadOnlyUpdate (PartialSetUpdate (a, b)))
setCartesianProductLens aeq beq = let
    elGet ::
           forall m t. MonadIO m
        => MutableRead m (PairUpdateReader (SetUpdate a) (SetUpdate b))
        -> SetReader (a, b) t
        -> m t
    elGet mr (MkTupleUpdateReader (MkFunctionSelector (a, b)) ReadWhole) = do
        hasA <- mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        hasB <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
        return $ hasA && hasB
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (SetUpdate a) (SetUpdate b)
        -> MutableRead m (PairUpdateReader (SetUpdate a) (SetUpdate b))
        -> m [ReadOnlyUpdate (PartialSetUpdate (a, b))]
    elUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) _)) _ =
        return $
        pure $
        MkReadOnlyUpdate $
        UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector (a', _)) ReadWhole) -> aeq a a'
    elUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector b) _)) _ =
        return $
        pure $
        MkReadOnlyUpdate $
        UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector (_, b')) ReadWhole) -> beq b b'
    elPutEdits ::
           forall m. MonadIO m
        => [ConstEdit (SetReader (a, b))]
        -> MutableRead m (PairUpdateReader (SetUpdate a) (SetUpdate b))
        -> m (Maybe [PairUpdateEdit (SetUpdate a) (SetUpdate b)])
    elPutEdits = elPutEditsNone
    in MkEditLens {..}

setCartesianProductPartialLens ::
       forall a b.
       (a -> a -> Bool)
    -> (b -> b -> Bool)
    -> EditLens (PairUpdate (PartialSetUpdate a) (PartialSetUpdate b)) (ReadOnlyUpdate (PartialSetUpdate (a, b)))
setCartesianProductPartialLens aeq beq = let
    setmap :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (a, b))
    setmap rset (MkTupleUpdateReader (MkFunctionSelector (a, b)) ReadWhole) =
        rset (MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) ||
        rset (MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole)
    in partialiseReadOnlyEditLens setmap (setCartesianProductLens aeq beq) . partialPairEditLens
