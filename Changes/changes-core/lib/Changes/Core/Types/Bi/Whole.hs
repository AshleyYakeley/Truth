module Changes.Core.Types.Bi.Whole where

import Shapes

import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Bi.Bi
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Whole

type BiWholeEdit p q = BiEdit (WholeEdit p) (WholeEdit q)

pattern MkBiWholeEdit :: p -> BiWholeEdit p q
pattern MkBiWholeEdit p = MkBiEdit (MkWholeReaderEdit p)

{-# COMPLETE MkBiWholeEdit #-}

type BiWholeUpdate p q = BiUpdate (WholeUpdate p) (WholeUpdate q)

pattern MkBiWholeUpdate :: q -> BiWholeUpdate p q
pattern MkBiWholeUpdate q = MkBiUpdate (MkWholeUpdate q)

{-# COMPLETE MkBiWholeUpdate #-}

lensBiWholeChangeLens ::
    forall p1 q1 p2 q2.
    (q1 -> q2) ->
    (p2 -> q1 -> Maybe p1) ->
    ChangeLens (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)
lensBiWholeChangeLens g pb = let
    clRead :: ReadFunction (WholeReader q1) (WholeReader q2)
    clRead rd ReadWhole = fmap g $ rd ReadWhole
    clUpdate ::
        forall m.
        MonadIO m =>
        BiWholeUpdate p1 q1 ->
        Readable m (WholeReader q1) ->
        m [BiWholeUpdate p2 q2]
    clUpdate (MkBiWholeUpdate q1) _ = return [MkBiWholeUpdate $ g q1]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [BiWholeEdit p2 q2] ->
        Readable m (WholeReader q1) ->
        m (Maybe [BiWholeEdit p1 q1])
    clPutEdits edits mr =
        case lastM edits of
            Just (MkBiWholeEdit p2) -> do
                q1 <- mr ReadWhole
                return $ do
                    p1 <- pb p2 q1
                    return [MkBiWholeEdit p1]
            Nothing -> return $ Just []
    in MkChangeLens{..}

mapBiWholeChangeLens :: (p2 -> p1) -> (q1 -> q2) -> ChangeLens (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)
mapBiWholeChangeLens pp qq = lensBiWholeChangeLens qq $ \p2 _ -> Just $ pp p2

pairBiWholeChangeLens ::
    forall p1 q1 p2 q2.
    ChangeLens (PairUpdate (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)) (BiWholeUpdate (p1, p2) (q1, q2))
pairBiWholeChangeLens = let
    clRead :: ReadFunction (PairUpdateReader (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)) (WholeReader (q1, q2))
    clRead rd ReadWhole = do
        q1 <- rd $ MkTupleUpdateReader SelectFirst ReadWhole
        q2 <- rd $ MkTupleUpdateReader SelectSecond ReadWhole
        return (q1, q2)
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2) ->
        Readable m (PairUpdateReader (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)) ->
        m [BiWholeUpdate (p1, p2) (q1, q2)]
    clUpdate (MkTupleUpdate SelectFirst (MkBiWholeUpdate q1)) rd = do
        q2 <- rd $ MkTupleUpdateReader SelectSecond ReadWhole
        return $ pure $ MkBiWholeUpdate (q1, q2)
    clUpdate (MkTupleUpdate SelectSecond (MkBiWholeUpdate q2)) rd = do
        q1 <- rd $ MkTupleUpdateReader SelectFirst ReadWhole
        return $ pure $ MkBiWholeUpdate (q1, q2)
    clPutEdits ::
        forall m.
        MonadIO m =>
        [BiWholeEdit (p1, p2) (q1, q2)] ->
        Readable m (PairUpdateReader (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)) ->
        m (Maybe [PairUpdateEdit (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)])
    clPutEdits edits _ =
        case lastM edits of
            Just (MkBiWholeEdit (p1, p2)) ->
                return
                    $ Just
                    $ [MkTupleUpdateEdit SelectFirst $ MkBiWholeEdit p1, MkTupleUpdateEdit SelectSecond $ MkBiWholeEdit p2]
            Nothing -> return $ Just []
    in MkChangeLens{..}
