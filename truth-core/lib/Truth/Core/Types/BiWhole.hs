module Truth.Core.Types.BiWhole where

import Shapes
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Bi
import Truth.Core.Types.Whole

type BiWholeEdit p q = BiEdit (WholeEdit p) (WholeEdit q)

pattern MkBiWholeEdit :: p -> BiWholeEdit p q

pattern MkBiWholeEdit p = MkBiEdit (MkWholeReaderEdit p)

{-# COMPLETE MkBiWholeEdit #-}

type BiWholeUpdate p q = BiUpdate (WholeUpdate p) (WholeUpdate q)

pattern MkBiWholeUpdate :: q -> BiWholeUpdate p q

pattern MkBiWholeUpdate q = MkBiUpdate (MkWholeUpdate q)

{-# COMPLETE MkBiWholeUpdate #-}

{-
instance Floating (BiWholeEdit p q) (BiWholeEdit p q)

instance ApplicableEdit (BiWholeEdit t t) where
    applyEdit (MkBiWholeEdit t) _ ReadWhole = return t
-}
lensBiWholeChangeLens ::
       forall p1 q1 p2 q2.
       (q1 -> q2)
    -> (p2 -> q1 -> Maybe p1)
    -> ChangeLens (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)
lensBiWholeChangeLens g pb = let
    clRead :: ReadFunction (WholeReader q1) (WholeReader q2)
    clRead rd ReadWhole = fmap g $ rd ReadWhole
    clUpdate ::
           forall m. MonadIO m
        => BiWholeUpdate p1 q1
        -> Readable m (WholeReader q1)
        -> m [BiWholeUpdate p2 q2]
    clUpdate (MkBiWholeUpdate q1) _ = return [MkBiWholeUpdate $ g q1]
    clPutEdits ::
           forall m. MonadIO m
        => [BiWholeEdit p2 q2]
        -> Readable m (WholeReader q1)
        -> m (Maybe [BiWholeEdit p1 q1])
    clPutEdits edits mr =
        case lastM edits of
            Just (MkBiWholeEdit p2) -> do
                q1 <- mr ReadWhole
                return $ do
                    p1 <- pb p2 q1
                    return [MkBiWholeEdit p1]
            Nothing -> return $ Just []
    in MkChangeLens {..}

mapBiWholeChangeLens :: (p2 -> p1) -> (q1 -> q2) -> ChangeLens (BiWholeUpdate p1 q1) (BiWholeUpdate p2 q2)
mapBiWholeChangeLens pp qq = lensBiWholeChangeLens qq $ \p2 _ -> Just $ pp p2
