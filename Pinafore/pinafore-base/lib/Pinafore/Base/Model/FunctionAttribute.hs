module Pinafore.Base.Model.FunctionAttribute
    ( StorageFunctionAttribute(..)
    , storageFunctionAttributeContextChangeLens
    , mapStorageFunctionAttributeBase
    ) where

import Changes.Core
import Shapes

data StorageFunctionAttribute baseupdate a b = MkStorageFunctionAttribute
    { pfFuncRead :: a -> ReadM (UpdateReader baseupdate) b
    , pfUpdate :: baseupdate -> ReadM (UpdateReader baseupdate) (Maybe (a -> ReadM (UpdateReader baseupdate) (Maybe b)))
    }

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (StorageFunctionAttribute baseupdate) where
    cfmap f =
        MkNestedMorphism $ \(MkStorageFunctionAttribute fr up) -> let
            fr' = (cfmap1 f) fr
            up' = (fmap $ fmap $ fmap $ cfmap1 f) up
            in MkStorageFunctionAttribute fr' up'

instance Functor (StorageFunctionAttribute baseupdate a) where
    fmap :: forall p q. (p -> q) -> StorageFunctionAttribute baseupdate a p -> StorageFunctionAttribute baseupdate a q
    fmap pq (MkStorageFunctionAttribute fr up) = let
        fr' = (fmap $ fmap pq) fr
        up' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap pq) up
        in MkStorageFunctionAttribute fr' up'

instance Applicative (StorageFunctionAttribute baseupdate a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance Category (StorageFunctionAttribute baseupdate) where
    id = let
        pfFuncRead = return
        pfUpdate _ = return Nothing
        in MkStorageFunctionAttribute {..}
    (.) :: forall a b c.
           StorageFunctionAttribute baseupdate b c
        -> StorageFunctionAttribute baseupdate a b
        -> StorageFunctionAttribute baseupdate a c
    MkStorageFunctionAttribute getBC buBC . MkStorageFunctionAttribute getAB buAB = let
        pfFuncRead a = getAB a >>= getBC
        pfUpdate ::
               baseupdate -> ReadM (UpdateReader baseupdate) (Maybe (a -> ReadM (UpdateReader baseupdate) (Maybe c)))
        pfUpdate update = do
            mfAB <- buAB update
            mfBC <- buBC update
            return $
                case (mfAB, mfBC) of
                    (Nothing, Nothing) -> Nothing
                    _ ->
                        Just $ \a -> do
                            mb <-
                                case mfAB of
                                    Nothing -> return Nothing
                                    Just amb -> amb a
                            case mb of
                                Nothing ->
                                    case mfBC of
                                        Nothing -> return Nothing
                                        Just bmc -> do
                                            b <- getAB a
                                            bmc b
                                Just b -> do
                                    c <- getBC b
                                    return $ Just c
        in MkStorageFunctionAttribute {..}

instance Arrow (StorageFunctionAttribute baseupdate) where
    arr ab = let
        pfFuncRead a = return $ ab a
        pfUpdate _ = return Nothing
        in MkStorageFunctionAttribute {..}
    first :: forall b c d. StorageFunctionAttribute baseupdate b c -> StorageFunctionAttribute baseupdate (b, d) (c, d)
    first (MkStorageFunctionAttribute bmc umbmc) = let
        pfFuncRead :: (b, d) -> ReadM (UpdateReader baseupdate) (c, d)
        pfFuncRead (b, d) = do
            c <- bmc b
            return (c, d)
        pfUpdate ::
               baseupdate
            -> ReadM (UpdateReader baseupdate) (Maybe ((b, d) -> ReadM (UpdateReader baseupdate) (Maybe (c, d))))
        pfUpdate update = do
            mf <- umbmc update
            return $
                case mf of
                    Nothing -> Nothing
                    Just brmc ->
                        Just $ \(b, d) -> do
                            mc <- brmc b
                            return $ fmap (\c -> (c, d)) mc
        in MkStorageFunctionAttribute {..}
    second = cfmap

instance ArrowChoice (StorageFunctionAttribute baseupdate) where
    left ::
           forall b c d.
           StorageFunctionAttribute baseupdate b c
        -> StorageFunctionAttribute baseupdate (Either b d) (Either c d)
    left (MkStorageFunctionAttribute fr upd) = let
        pfFuncRead (Left b) = fmap Left $ fr b
        pfFuncRead (Right d) = return $ Right d
        pfUpdate ::
               baseupdate
            -> ReadM (UpdateReader baseupdate) (Maybe (Either b d -> ReadM (UpdateReader baseupdate) (Maybe (Either c d))))
        pfUpdate update = do
            mf <- upd update
            return $
                case mf of
                    Nothing -> Nothing
                    Just brmc ->
                        Just $ \case
                            Left b -> do
                                mc <- brmc b
                                return $ fmap Left mc
                            Right _ -> return Nothing
        in MkStorageFunctionAttribute {..}
    right ::
           forall b c d.
           StorageFunctionAttribute baseupdate b c
        -> StorageFunctionAttribute baseupdate (Either d b) (Either d c)
    right (MkStorageFunctionAttribute fr upd) = let
        pfFuncRead (Left d) = return $ Left d
        pfFuncRead (Right b) = fmap Right $ fr b
        pfUpdate ::
               baseupdate
            -> ReadM (UpdateReader baseupdate) (Maybe (Either d b -> ReadM (UpdateReader baseupdate) (Maybe (Either d c))))
        pfUpdate update = do
            mf <- upd update
            return $
                case mf of
                    Nothing -> Nothing
                    Just brmc ->
                        Just $ \case
                            Left _ -> return Nothing
                            Right b -> do
                                mc <- brmc b
                                return $ fmap Right mc
        in MkStorageFunctionAttribute {..}

instance Traversable f => CatFunctor (StorageFunctionAttribute baseupdate) (StorageFunctionAttribute baseupdate) f where
    cfmap :: forall a b. StorageFunctionAttribute baseupdate a b -> StorageFunctionAttribute baseupdate (f a) (f b)
    cfmap (MkStorageFunctionAttribute f upd) = let
        pfFuncRead fa = for fa f
        pfUpdate ::
               baseupdate
            -> ReadM (UpdateReader baseupdate) (Maybe (f a -> ReadM (UpdateReader baseupdate) (Maybe (f b))))
        pfUpdate update = do
            mf <- upd update
            return $
                case mf of
                    Nothing -> Nothing
                    Just brmc ->
                        Just $ \fa -> do
                            fmb <-
                                for fa $ \a -> do
                                    mb <- brmc a
                                    return $
                                        case mb of
                                            Just b -> (True, return b)
                                            Nothing -> (False, f a)
                            case any fst fmb of
                                False -> return Nothing
                                True -> do
                                    fb <- for fmb $ \(_, mb) -> mb
                                    return $ Just fb
        in MkStorageFunctionAttribute {..}

storageFunctionAttributeContextChangeLens ::
       forall baseupdate a b.
       StorageFunctionAttribute baseupdate a b
    -> ChangeLens (ContextUpdate baseupdate (WholeUpdate a)) (ROWUpdate b)
storageFunctionAttributeContextChangeLens MkStorageFunctionAttribute {..} = let
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
        mf <- unReadM (pfUpdate pinupdate) $ tupleReadFunction SelectContext mr
        case mf of
            Nothing -> return []
            Just armb -> do
                a <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                mb <- unReadM (armb a) $ tupleReadFunction SelectContext mr
                case mb of
                    Nothing -> return []
                    Just b -> return [MkReadOnlyUpdate $ MkWholeReaderUpdate b]
    clUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) mr = do
        b <- unReadM (pfFuncRead a) (tupleReadFunction SelectContext mr)
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate b]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

mapStorageFunctionAttributeBase ::
       forall baseA baseB a b.
       ChangeLens baseB baseA
    -> StorageFunctionAttribute baseA a b
    -> StorageFunctionAttribute baseB a b
mapStorageFunctionAttributeBase aef (MkStorageFunctionAttribute frA updA) = let
    rf :: ReadFunction (UpdateReader baseB) (UpdateReader baseA)
    rf = clRead aef
    frB a = mapReadM rf $ frA a
    updB :: baseB -> ReadM (UpdateReader baseB) (Maybe (a -> ReadM (UpdateReader baseB) (Maybe b)))
    updB updateB =
        return $
        Just $ \a -> do
            updateAs <- MkReadM $ clUpdate aef updateB
            chs <-
                for updateAs $ \updateA ->
                    mapReadM rf $ do
                        mf <- updA updateA
                        case mf of
                            Nothing -> return Nothing
                            Just armb -> armb a
            return $ lastM $ catMaybes chs
    in MkStorageFunctionAttribute frB updB
