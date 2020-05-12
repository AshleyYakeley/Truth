module Pinafore.Base.FunctionMorphism
    ( PinaforeFunctionMorphism(..)
    , pinaforeFunctionMorphismUpdateFunction
    , mapPinaforeFunctionMorphismBase
    ) where

import Shapes
import Truth.Core

-- equivalent to: type PinaforeFunctionMorphism baseupdate a b = UpdateFunction baseupdate (OpaqueUpdate (FunctionUpdateEdit a (WholeUpdate b)))
data PinaforeFunctionMorphism baseupdate a b = MkPinaforeFunctionMorphism
    { pfFuncRead :: a -> ReadM (UpdateReader baseupdate) b
    , pfUpdate :: baseupdate -> ReadM (UpdateReader baseupdate) Bool
    }

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (PinaforeFunctionMorphism baseupdate) where
    cfmap (MkCatDual f) = MkNestedMorphism $ \(MkPinaforeFunctionMorphism fr u) -> MkPinaforeFunctionMorphism (fr . f) u

instance Functor (PinaforeFunctionMorphism baseupdate a) where
    fmap :: forall p q. (p -> q) -> PinaforeFunctionMorphism baseupdate a p -> PinaforeFunctionMorphism baseupdate a q
    fmap pq (MkPinaforeFunctionMorphism fr up) = MkPinaforeFunctionMorphism (\a -> fmap pq (fr a)) up

instance Applicative (PinaforeFunctionMorphism baseupdate a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance Category (PinaforeFunctionMorphism baseupdate) where
    id = let
        pfFuncRead = return
        pfUpdate _ = return False
        in MkPinaforeFunctionMorphism {..}
    (.) :: forall a b c.
           PinaforeFunctionMorphism baseupdate b c
        -> PinaforeFunctionMorphism baseupdate a b
        -> PinaforeFunctionMorphism baseupdate a c
    MkPinaforeFunctionMorphism lbc ubc . MkPinaforeFunctionMorphism lab uab = let
        pfFuncRead :: a -> ReadM (UpdateReader baseupdate) c
        pfFuncRead a = do
            b <- lab a
            lbc b
        pfUpdate :: baseupdate -> ReadM (UpdateReader baseupdate) Bool
        pfUpdate update = do
            chab <- uab update
            chbc <- ubc update
            return $ chab || chbc
        in MkPinaforeFunctionMorphism {..}

instance Arrow (PinaforeFunctionMorphism baseupdate) where
    arr ab = MkPinaforeFunctionMorphism {pfFuncRead = \a -> return $ ab a, pfUpdate = \_ -> return False}
    first :: forall b c d. PinaforeFunctionMorphism baseupdate b c -> PinaforeFunctionMorphism baseupdate (b, d) (c, d)
    first (MkPinaforeFunctionMorphism bc pfUpdate) = let
        pfFuncRead :: (b, d) -> ReadM (UpdateReader baseupdate) (c, d)
        pfFuncRead (b, d) = do
            c <- bc b
            return (c, d)
        in MkPinaforeFunctionMorphism {..}
    second = cfmap

instance ArrowChoice (PinaforeFunctionMorphism baseupdate) where
    left (MkPinaforeFunctionMorphism pfr pfUpdate) = let
        pfFuncRead ebd =
            case ebd of
                Left b -> fmap Left (pfr b)
                Right d -> return $ Right d
        in MkPinaforeFunctionMorphism {..}
    right (MkPinaforeFunctionMorphism pfr pfUpdate) = let
        pfFuncRead ebd =
            case ebd of
                Left d -> return $ Left d
                Right b -> fmap Right (pfr b)
        in MkPinaforeFunctionMorphism {..}

instance Traversable f => CatFunctor (PinaforeFunctionMorphism baseupdate) (PinaforeFunctionMorphism baseupdate) f where
    cfmap :: forall a b. PinaforeFunctionMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate (f a) (f b)
    cfmap (MkPinaforeFunctionMorphism f pfUpdate) = let
        pfFuncRead fa = for fa f
        in MkPinaforeFunctionMorphism {..}

pinaforeFunctionMorphismUpdateFunction ::
       forall baseupdate a b.
       PinaforeFunctionMorphism baseupdate a b
    -> ChangeLens (ContextUpdate baseupdate (WholeUpdate a)) (ROWUpdate b)
pinaforeFunctionMorphismUpdateFunction MkPinaforeFunctionMorphism {..} = let
    getB ::
           forall m. MonadIO m
        => Readable m (ContextUpdateReader baseupdate (WholeUpdate a))
        -> m b
    getB mr = do
        a <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        unReadM (pfFuncRead a) (tupleReadFunction SelectContext mr)
    clRead :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate a)) (WholeReader b)
    clRead mr ReadWhole = getB mr
    clUpdate ::
           forall m. MonadIO m
        => (ContextUpdate baseupdate (WholeUpdate a))
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate a))
        -> m [ROWUpdate b]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- unReadM (pfUpdate pinupdate) $ tupleReadFunction SelectContext mr
        if ch
            then do
                b <- getB mr
                return [MkReadOnlyUpdate $ MkWholeReaderUpdate b]
            else return []
    clUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) mr = do
        b <- unReadM (pfFuncRead a) (tupleReadFunction SelectContext mr)
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate b]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       ChangeLens baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase aef (MkPinaforeFunctionMorphism frA updateA) = let
    rf :: ReadFunction (UpdateReader baseB) (UpdateReader baseA)
    rf = clRead aef
    frB a = mapReadM rf $ frA a
    updateB beditB = do
        beditAs <- MkReadM $ clUpdate aef beditB
        chs <- for beditAs $ \beditA -> mapReadM rf $ updateA beditA
        return $ or chs
    in MkPinaforeFunctionMorphism frB updateB
