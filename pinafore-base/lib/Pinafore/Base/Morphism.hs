module Pinafore.Base.Morphism
    ( PinaforeFunctionMorphism(..)
    , pinaforeFunctionMorphismUpdateFunction
    , PinaforeLensMorphism(..)
    , pinaforeLensMorphismInverseChangeLens
    , pinaforeLensMorphismInverseChangeLensSet
    , mapPinaforeFunctionMorphismBase
    , mapPinaforeLensMorphismBase
    , funcPinaforeLensMorphism
    , nullPinaforeLensMorphism
    , bijectionPinaforeLensMorphism
    , pairPinaforeLensMorphism
    , eitherPinaforeLensMorphism
    , lensFunctionMorphism
    , lensInverseFunctionMorphism
    ) where

import Pinafore.Base.Know
import Shapes
import Truth.Core

-- equivalent to: type PinaforeFunctionMorphism baseupdate a b = UpdateFunction baseupdate (OpaqueUpdate (FunctionUpdateEdit a (WholeUpdate b)))
data PinaforeFunctionMorphism baseupdate a b = MkPinaforeFunctionMorphism
    { pfFuncRead :: forall m. MonadIO m => Readable m (UpdateReader baseupdate) -> a -> m b
    , pfUpdate :: forall m. MonadIO m => baseupdate -> Readable m (UpdateReader baseupdate) -> m Bool
    }

instance Functor (PinaforeFunctionMorphism baseupdate a) where
    fmap :: forall p q. (p -> q) -> PinaforeFunctionMorphism baseupdate a p -> PinaforeFunctionMorphism baseupdate a q
    fmap pq (MkPinaforeFunctionMorphism fr up) = let
        fr' :: forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> a
            -> m q
        fr' mr a = fmap pq $ fr mr a
        in MkPinaforeFunctionMorphism fr' up

instance Applicative (PinaforeFunctionMorphism baseupdate a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance Category (PinaforeFunctionMorphism baseupdate) where
    id = let
        pfFuncRead _ = return
        pfUpdate _ _ = return False
        in MkPinaforeFunctionMorphism {..}
    (.) :: forall a b c.
           PinaforeFunctionMorphism baseupdate b c
        -> PinaforeFunctionMorphism baseupdate a b
        -> PinaforeFunctionMorphism baseupdate a c
    MkPinaforeFunctionMorphism lbc ubc . MkPinaforeFunctionMorphism lab uab = let
        pfFuncRead ::
               forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> a
            -> m c
        pfFuncRead mr a = do
            b <- lab mr a
            lbc mr b
        pfUpdate ::
               forall m. MonadIO m
            => baseupdate
            -> Readable m (UpdateReader baseupdate)
            -> m Bool
        pfUpdate update mr = do
            chab <- uab update mr
            chbc <- ubc update mr
            return $ chab || chbc
        in MkPinaforeFunctionMorphism {..}

instance Arrow (PinaforeFunctionMorphism baseupdate) where
    arr ab = MkPinaforeFunctionMorphism {pfFuncRead = \_ a -> return $ ab a, pfUpdate = \_ _ -> return False}
    first :: forall b c d. PinaforeFunctionMorphism baseupdate b c -> PinaforeFunctionMorphism baseupdate (b, d) (c, d)
    first (MkPinaforeFunctionMorphism bc pfUpdate) = let
        pfFuncRead ::
               forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> (b, d)
            -> m (c, d)
        pfFuncRead mr (b, d) = do
            c <- bc mr b
            return (c, d)
        in MkPinaforeFunctionMorphism {..}
    second = cfmap

instance ArrowChoice (PinaforeFunctionMorphism baseupdate) where
    left (MkPinaforeFunctionMorphism pfr pfUpdate) = let
        pfFuncRead ::
               forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> _
            -> m _
        pfFuncRead mr ebd =
            case ebd of
                Left b -> fmap Left (pfr mr b)
                Right d -> return $ Right d
        in MkPinaforeFunctionMorphism {..}
    right (MkPinaforeFunctionMorphism pfr pfUpdate) = let
        pfFuncRead ::
               forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> _
            -> m _
        pfFuncRead mr ebd =
            case ebd of
                Left d -> return $ Left d
                Right b -> fmap Right (pfr mr b)
        in MkPinaforeFunctionMorphism {..}

instance Traversable f => CatFunctor (PinaforeFunctionMorphism baseupdate) (PinaforeFunctionMorphism baseupdate) f where
    cfmap :: forall a b. PinaforeFunctionMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate (f a) (f b)
    cfmap (MkPinaforeFunctionMorphism f pfUpdate) = let
        pfFuncRead ::
               forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> f a
            -> m (f b)
        pfFuncRead mr fa = for fa $ f mr
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
        pfFuncRead (tupleReadFunction SelectContext mr) a
    clRead :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate a)) (WholeReader b)
    clRead mr ReadWhole = getB mr
    clUpdate ::
           forall m. MonadIO m
        => (ContextUpdate baseupdate (WholeUpdate a))
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate a))
        -> m [ROWUpdate b]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- pfUpdate pinupdate $ tupleReadFunction SelectContext mr
        if ch
            then do
                b <- getB mr
                return [MkReadOnlyUpdate $ MkWholeReaderUpdate b]
            else return []
    clUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) mr = do
        b <- pfFuncRead (tupleReadFunction SelectContext mr) a
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate b]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       ChangeLens baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase aef (MkPinaforeFunctionMorphism frA updateA) = let
    readFunc :: ReadFunction (UpdateReader baseB) (UpdateReader baseA)
    readFunc = clRead aef
    frB :: forall m. MonadIO m
        => Readable m (UpdateReader baseB)
        -> a
        -> m b
    frB mr a = frA (readFunc mr) a
    updateB ::
           forall m. MonadIO m
        => baseB
        -> Readable m (UpdateReader baseB)
        -> m Bool
    updateB beditB mr = do
        beditAs <- clUpdate aef beditB mr
        chs <- for beditAs $ \beditA -> updateA beditA $ readFunc mr
        return $ or chs
    in MkPinaforeFunctionMorphism frB updateB

data PinaforeLensMorphism baseupdate a b = MkPinaforeLensMorphism
    { pmForward :: ChangeLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    , pmInverse :: PinaforeFunctionMorphism baseupdate b [a]
    }

pairPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate a (b, c)
pairPinaforeLensMorphism (MkPinaforeLensMorphism (MkChangeLens getB updateB putEditsB) (MkPinaforeFunctionMorphism frB updB)) (MkPinaforeLensMorphism (MkChangeLens getC updateC putEditsC) (MkPinaforeFunctionMorphism frC updC)) = let
    getBC ::
           forall m. MonadIO m
        => Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> Readable m (WholeReader (Know (b, c)))
    getBC mr ReadWhole = do
        kb <- getB mr ReadWhole
        case kb of
            Unknown -> return Unknown
            Known b -> do
                kc <- getC mr ReadWhole
                return $ fmap (\c -> (b, c)) kc
    updateBC ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know a))
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m [WholeUpdate (Know (b, c))]
    updateBC update mr = do
        ebs <- updateB update mr
        ecs <- updateC update mr
        case (lastM ebs, lastM ecs) of
            (Nothing, Nothing) -> return []
            (Just (MkWholeReaderUpdate Unknown), _) -> return [MkWholeReaderUpdate Unknown]
            (_, Just (MkWholeReaderUpdate Unknown)) -> return [MkWholeReaderUpdate Unknown]
            (Just (MkWholeReaderUpdate (Known b)), Just (MkWholeReaderUpdate (Known c))) ->
                return [MkWholeReaderUpdate $ Known (b, c)]
            (Just (MkWholeReaderUpdate (Known b)), Nothing) -> do
                kc <- getC mr ReadWhole
                return [MkWholeReaderUpdate $ fmap (\c -> (b, c)) kc]
            (Nothing, Just (MkWholeReaderUpdate (Known c))) -> do
                kb <- getB mr ReadWhole
                return [MkWholeReaderUpdate $ fmap (\b -> (b, c)) kb]
    putEditsBC ::
           forall m. MonadIO m
        => [WholeEdit (Know (b, c))]
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    putEditsBC =
        wholePutEdits $ \kbc mr ->
            case kbc of
                Unknown -> return Nothing -- can't delete
                (Known (b, c)) ->
                    getComposeM $ do
                        aa1 <- MkComposeM $ putEditsB [MkWholeReaderEdit $ Known b] mr
                        aa2 <- MkComposeM $ putEditsC [MkWholeReaderEdit $ Known c] mr
                        return $ aa1 ++ aa2
    frBC ::
           forall m. MonadIO m
        => Readable m (UpdateReader baseupdate)
        -> (b, c)
        -> m [a]
    frBC mr (b, c) = do
        aa1 <- frB mr b
        aa2 <- frC mr c
        return $ aa1 ++ aa2
    updBC ::
           forall m. MonadIO m
        => baseupdate
        -> Readable m (UpdateReader baseupdate)
        -> m Bool
    updBC update mr = do
        eb <- updB update mr
        case eb of
            True -> return True
            False -> updC update mr
    in MkPinaforeLensMorphism (MkChangeLens getBC updateBC putEditsBC) (MkPinaforeFunctionMorphism frBC updBC)

eitherPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate b c
    -> PinaforeLensMorphism baseupdate (Either a b) c
eitherPinaforeLensMorphism (MkPinaforeLensMorphism (MkChangeLens getA updateA putEditsA) (MkPinaforeFunctionMorphism frA updA)) (MkPinaforeLensMorphism (MkChangeLens getB updateB putEditsB) (MkPinaforeFunctionMorphism frB updB)) = let
    getMRA ::
           forall m. MonadIO m
        => Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
        -> a
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
    getMRA mr _ (MkTupleUpdateReader SelectContext r) = mr $ MkTupleUpdateReader SelectContext r
    getMRA _ a (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known a
    getMRB ::
           forall m. MonadIO m
        => Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
        -> b
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
    getMRB mr _ (MkTupleUpdateReader SelectContext r) = mr $ MkTupleUpdateReader SelectContext r
    getMRB _ b (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known b
    getAB :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b)))) (WholeReader (Know c))
    getAB (mr :: Readable m _) ReadWhole = do
        keab <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        case keab of
            Unknown -> return Unknown
            Known (Left a) -> getA (getMRA mr a) ReadWhole
            Known (Right b) -> getB (getMRB mr b) ReadWhole
    updateAB ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know (Either a b)))
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
        -> m [WholeUpdate (Know c)]
    updateAB (MkTupleUpdate SelectContext update) mr = do
        keab <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        case keab of
            Unknown -> return [MkWholeReaderUpdate Unknown]
            Known (Left a) -> updateA (MkTupleUpdate SelectContext update) (getMRA mr a)
            Known (Right b) -> updateB (MkTupleUpdate SelectContext update) (getMRB mr b)
    updateAB (MkTupleUpdate SelectContent (MkWholeReaderUpdate keab)) mr =
        case keab of
            Unknown -> return [MkWholeReaderUpdate Unknown]
            Known (Left a) -> updateA (MkTupleUpdate SelectContent $ MkWholeReaderUpdate $ Known a) (getMRA mr a)
            Known (Right b) -> updateB (MkTupleUpdate SelectContent $ MkWholeReaderUpdate $ Known b) (getMRB mr b)
    putEditsAB ::
           forall m. MonadIO m
        => [WholeEdit (Know c)]
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know (Either a b)))])
    putEditsAB =
        wholePutEdits $ \kc mr -> do
            keab <- mr $ MkTupleUpdateReader SelectContent ReadWhole
            case keab of
                Unknown ->
                    case kc of
                        Unknown -> return $ Just []
                        Known _ -> return Nothing
                Known (Left a) -> do
                    mea <- putEditsA [MkWholeReaderEdit kc] (getMRA mr a)
                    return $ (fmap $ fmap $ mapContextEdit $ mapWholeEdit $ fmap Left) mea
                Known (Right b) -> do
                    meb <- putEditsB [MkWholeReaderEdit kc] (getMRB mr b)
                    return $ (fmap $ fmap $ mapContextEdit $ mapWholeEdit $ fmap Right) meb
    frAB ::
           forall m. MonadIO m
        => Readable m (UpdateReader baseupdate)
        -> c
        -> m [Either a b]
    frAB mr c = do
        aa <- frA mr c
        bb <- frB mr c
        return $ fmap Left aa <> fmap Right bb
    updAB ::
           forall m. MonadIO m
        => baseupdate
        -> Readable m (UpdateReader baseupdate)
        -> m Bool
    updAB update mr = do
        u <- updA update mr
        case u of
            True -> return True
            False -> updB update mr
    in MkPinaforeLensMorphism (MkChangeLens getAB updateAB putEditsAB) (MkPinaforeFunctionMorphism frAB updAB)

mapPinaforeLensMorphismBase ::
       forall baseA baseB a b.
       ChangeLens baseB baseA
    -> PinaforeLensMorphism baseA a b
    -> PinaforeLensMorphism baseB a b
mapPinaforeLensMorphismBase alens (MkPinaforeLensMorphism fwdA (MkPinaforeFunctionMorphism frA updateA)) = let
    readFunc :: ReadFunction (UpdateReader baseB) (UpdateReader baseA)
    readFunc = clRead alens
    fwdB :: ChangeLens (ContextUpdate baseB (WholeUpdate (Know a))) (WholeUpdate (Know b))
    fwdB = fwdA . liftContentChangeLens alens
    frB :: forall m. MonadIO m
        => Readable m (UpdateReader baseB)
        -> b
        -> m [a]
    frB mr b = frA (readFunc mr) b
    updateB ::
           forall m. MonadIO m
        => baseB
        -> Readable m (UpdateReader baseB)
        -> m Bool
    updateB beditB mr = do
        beditAs <- clUpdate alens beditB mr
        chs <- for beditAs $ \beditA -> updateA beditA $ readFunc mr
        return $ or chs
    invB :: PinaforeFunctionMorphism baseB b [a]
    invB = MkPinaforeFunctionMorphism frB updateB
    plmB :: PinaforeLensMorphism baseB a b
    plmB = MkPinaforeLensMorphism fwdB invB
    in plmB

bindReadContext ::
       Readable m (ContextUpdateReader updateX updateA)
    -> Readable m (UpdateReader updateB)
    -> Readable m (ContextUpdateReader updateX updateB)
bindReadContext mr _ (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
bindReadContext _ mr (MkTupleUpdateReader SelectContent rt) = mr rt

instance Category (PinaforeLensMorphism baseupdate) where
    id = let
        clRead :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know a))
        clRead mr ReadWhole = mr $ MkTupleUpdateReader SelectContent ReadWhole
        clUpdate ::
               MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know a)]
        clUpdate (MkTupleUpdate SelectContext _) _ = return []
        clUpdate (MkTupleUpdate SelectContent update) _ = return [update]
        clPutEdits ::
               MonadIO m
            => [WholeEdit (Know a)]
            -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        clPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit SelectContent) edits
        pmForward :: ChangeLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        pmForward = MkChangeLens {..}
        pfFuncRead :: MonadIO m => Readable m (UpdateReader baseupdate) -> a -> m [a]
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: MonadIO m => baseupdate -> Readable m (UpdateReader baseupdate) -> m Bool
        pfUpdate _ _ = return False
        pmInverse :: PinaforeFunctionMorphism baseupdate a [a]
        pmInverse = MkPinaforeFunctionMorphism {..}
        in MkPinaforeLensMorphism {..}
    (.) :: forall a b c.
           PinaforeLensMorphism baseupdate b c
        -> PinaforeLensMorphism baseupdate a b
        -> PinaforeLensMorphism baseupdate a c
    MkPinaforeLensMorphism (MkChangeLens bcGet bcUpdate bcPutEdit) (MkPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate) . MkPinaforeLensMorphism (MkChangeLens abGet abUpdate abPutEdit) (MkPinaforeFunctionMorphism abInvFuncRead abInvUpdate) = let
        acGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know c))
        acGet (mra :: Readable m _) ReadWhole = do
            mb <- abGet mra ReadWhole
            bcGet (bindReadContext mra $ subjectToReadable mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know c)]
        acUpdate (MkTupleUpdate SelectContext pinupdate) mra = do
            editbs <- abUpdate (MkTupleUpdate SelectContext pinupdate) mra
            editcs1 <- bcUpdate (MkTupleUpdate SelectContext pinupdate) $ bindReadContext mra $ abGet mra
            editcss2 <-
                for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                    bcUpdate (MkTupleUpdate SelectContent updateB) $ bindReadContext mra $ subjectToReadable mb
            return $ editcs1 ++ mconcat editcss2
        acUpdate (MkTupleUpdate SelectContent updateA) mra = do
            editbs <- abUpdate (MkTupleUpdate SelectContent updateA) mra
            editcss <-
                for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                    bcUpdate (MkTupleUpdate SelectContent updateB) $ bindReadContext mra $ subjectToReadable mb
            return $ mconcat editcss
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit (Know c)]
            -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        acPutEdit editcs mra =
            getComposeM $ do
                editpbs <- MkComposeM $ bcPutEdit editcs $ bindReadContext mra $ abGet mra
                case partitionContextEdits editpbs of
                    (pinedits, editbs) -> do
                        editpas2 <- MkComposeM $ abPutEdit editbs mra
                        return $ (fmap (MkTupleUpdateEdit SelectContext) pinedits) ++ editpas2
        acForward :: ChangeLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
        acForward = MkChangeLens acGet acUpdate acPutEdit
        acInvFuncRead ::
               forall m. MonadIO m
            => Readable m (UpdateReader baseupdate)
            -> c
            -> m [a]
        acInvFuncRead mr c = do
            bset <- bcInvFuncRead mr c
            asetset <- for bset $ \b -> abInvFuncRead mr b
            return $ mconcat asetset
        acInvUpdate ::
               forall m. MonadIO m
            => baseupdate
            -> Readable m (UpdateReader baseupdate)
            -> m Bool
        acInvUpdate pedit mr = do
            chbc <- bcInvUpdate pedit mr
            chab <- abInvUpdate pedit mr
            return $ chbc || chab
        acInverse :: PinaforeFunctionMorphism baseupdate c [a]
        acInverse = MkPinaforeFunctionMorphism acInvFuncRead acInvUpdate
        in MkPinaforeLensMorphism acForward acInverse

funcPinaforeLensMorphism ::
       forall baseupdate a b.
       (Know a -> Know b)
    -> (b -> [a])
    -> (Know b -> Maybe (Know a))
    -> PinaforeLensMorphism baseupdate a b
funcPinaforeLensMorphism ab bsa bma = let
    clRead :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know b))
    clRead mr ReadWhole = fmap ab $ mr $ MkTupleUpdateReader SelectContent ReadWhole
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know a))
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m [WholeUpdate (Know b)]
    clUpdate (MkTupleUpdate SelectContext _) _ = return []
    clUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) _ = return [MkWholeReaderUpdate $ ab a]
    clPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    clPutEdits edits _ =
        return $
        case lastWholeEdit edits of
            Nothing -> Just []
            Just kb -> fmap (pure . MkTupleUpdateEdit SelectContent . MkWholeReaderEdit) (bma kb)
    pmForward :: ChangeLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    pmForward = MkChangeLens {..}
    pfFuncRead ::
           forall m. MonadIO m
        => Readable m (UpdateReader baseupdate)
        -> b
        -> m [a]
    pfFuncRead _ b = return $ bsa b
    pfUpdate ::
           forall m. MonadIO m
        => baseupdate
        -> Readable m (UpdateReader baseupdate)
        -> m Bool
    pfUpdate _ _ = return False
    pmInverse :: PinaforeFunctionMorphism baseupdate b [a]
    pmInverse = MkPinaforeFunctionMorphism {..}
    in MkPinaforeLensMorphism {..}

nullPinaforeLensMorphism :: forall baseupdate a b. PinaforeLensMorphism baseupdate a b
nullPinaforeLensMorphism = funcPinaforeLensMorphism (\_ -> Unknown) (\_ -> mempty) (\_ -> Nothing)

bijectionPinaforeLensMorphism :: Bijection a b -> PinaforeLensMorphism baseupdate a b
bijectionPinaforeLensMorphism (MkIsomorphism ab ba) =
    funcPinaforeLensMorphism (fmap ab) (\b -> opoint $ ba b) (\kb -> Just $ fmap ba kb)

instance IsoVariant (PinaforeLensMorphism baseupdate t) where
    isoMap ab ba m = bijectionPinaforeLensMorphism (MkIsomorphism ab ba) . m

instance IsoVariant' (PinaforeLensMorphism baseupdate) where
    isoMap' ab ba m = m . bijectionPinaforeLensMorphism (MkIsomorphism ba ab)

lensFunctionMorphism ::
       forall baseupdate a b.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
lensFunctionMorphism MkPinaforeLensMorphism {..} = let
    funcRead ::
           forall m. MonadIO m
        => Readable m (UpdateReader baseupdate)
        -> Know a
        -> m (Know b)
    funcRead mr a = let
        mr' :: Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mr' (MkTupleUpdateReader SelectContext rt) = mr rt
        mr' (MkTupleUpdateReader SelectContent ReadWhole) = return a
        in clRead pmForward mr' ReadWhole
    in MkPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate b [a]
lensInverseFunctionMorphism = pmInverse

pinaforeLensMorphismInverseChangeLens ::
       forall baseupdate a b. Eq a
    => PinaforeLensMorphism baseupdate a b
    -> ChangeLens (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
pinaforeLensMorphismInverseChangeLens MkPinaforeLensMorphism {..} = let
    getFiniteSet ::
           forall m update. (MonadIO m, MonadIO (m))
        => Know b
        -> Readable m (ContextUpdateReader baseupdate update)
        -> m (FiniteSet a)
    getFiniteSet (Known b) mr = fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getFiniteSet Unknown _ = return mempty
    fsetReadFunction :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (WholeReader (FiniteSet a))
    fsetReadFunction (mr :: Readable m _) ReadWhole = do
        kb <- mr (MkTupleUpdateReader SelectContent ReadWhole)
        getFiniteSet kb mr
    clRead :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (FiniteSetReader a)
    clRead (mr :: Readable m _) rt = wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know b))
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> m [FiniteSetUpdate a]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- pfUpdate pmInverse pinupdate $ tupleReadFunction SelectContext mr
        if ch
            then do
                kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                aset <- getFiniteSet kb mr
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
            else return []
    clUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate kb)) mr = do
        aset <- getFiniteSet kb mr
        aedits <- getReplaceEditsFromSubject aset
        return $ fmap editUpdate aedits
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkChangeLens _ _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> Readable m (UpdateReader baseupdate)
        -> m (Maybe [UpdateEdit baseupdate])
    putEditAB a kb mr = do
        medits <-
            putEditBA @m [MkWholeReaderEdit kb] $ \case
                MkTupleUpdateReader SelectContext rt -> mr rt
                MkTupleUpdateReader SelectContent ReadWhole -> return $ Known a
        return $
            fmap
                (\edits ->
                     mapMaybe
                         (\case
                              MkTupleUpdateEdit SelectContext edit -> Just edit
                              MkTupleUpdateEdit SelectContent _ -> Nothing)
                         edits)
                medits
    elPutEdit ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    elPutEdit (KeyEditItem _ update) _ = never update
    elPutEdit (KeyEditDelete a) mr = do
        mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit (KeyEditInsertReplace a) mr = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        mpedits <- putEditAB a kb $ tupleReadFunction SelectContext mr
        return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit KeyEditClear mr = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        aa <- getFiniteSet kb mr
        lmpedits <- for (toList aa) $ \a -> putEditAB a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap (\lpedits -> fmap (MkTupleUpdateEdit SelectContext) $ mconcat lpedits) $ sequenceA lmpedits
    applyEdit' ::
           ContextUpdateEdit baseupdate (WholeUpdate (Know b))
        -> ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
    -- removed line to avoid (ApplicableEdit baseupdate) constraint, possibly kinda hacky.
    -- applyEdit' (MkTupleUpdateEdit SelectContext update) mr (MkTupleUpdateReader SelectContext rt) = applyEdit update (mr . MkTupleUpdateReader SelectContext) rt
    applyEdit' (MkTupleUpdateEdit SelectContent update) mr (MkTupleUpdateReader SelectContent rt) =
        applyEdit update (mr . MkTupleUpdateReader SelectContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextUpdateEdit baseupdate (WholeUpdate (Know b))]
        -> ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    clPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    clPutEdits [] _ = getComposeM $ return []
    clPutEdits (e:ee) mr =
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit e mr
            eea <- MkComposeM $ clPutEdits ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkChangeLens {..}

pinaforeLensMorphismInverseChangeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a b
    -> ChangeLens (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
pinaforeLensMorphismInverseChangeLensSet newb MkPinaforeLensMorphism {..} = let
    getPointPreimage ::
           forall m update. (MonadIO m, MonadIO (m))
        => b
        -> Readable m (ContextUpdateReader baseupdate update)
        -> m (FiniteSet a)
    getPointPreimage b mr = fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getSetPreimage ::
           forall m update. (MonadIO m, MonadIO (m))
        => FiniteSet b
        -> Readable m (ContextUpdateReader baseupdate update)
        -> m (FiniteSet a)
    getSetPreimage bs mr = do
        as <- for bs $ \b -> getPointPreimage b mr
        return $ mconcat $ toList as
    getAB ::
           forall m update. MonadIO m
        => Readable m (ContextUpdateReader baseupdate update)
        -> a
        -> m (Know b)
    getAB mr a = let
        mra :: Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mra (MkTupleUpdateReader SelectContext rp) = mr $ MkTupleUpdateReader SelectContext rp
        mra (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known a
        in clRead pmForward mra ReadWhole
    clRead' :: ReadFunction (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (FiniteSetReader a)
    clRead' (mr :: Readable m _) KeyReadKeys = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        getSetPreimage bs mr
    clRead' (mr :: Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))) (KeyReadItem a ReadWhole) = do
        kb <- getAB mr a
        case kb of
            Known b -> do
                mb <- mr $ MkTupleUpdateReader SelectContent $ KeyReadItem b ReadWhole
                case mb of
                    Just _ -> return $ Just a
                    Nothing -> return Nothing
            Unknown -> return Nothing
    clUpdate' ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (FiniteSetUpdate b)
        -> Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m [FiniteSetUpdate a]
    clUpdate' (MkTupleUpdate SelectContext pinaedit) mr = do
        ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
        if ch
            then do
                bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                aset <- getSetPreimage bs mr
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
            else return []
    clUpdate' (MkTupleUpdate SelectContent (KeyUpdateItem _ update)) _ = never update
    clUpdate' (MkTupleUpdate SelectContent KeyUpdateClear) _ = return [KeyUpdateClear]
    clUpdate' (MkTupleUpdate SelectContent (KeyUpdateInsertReplace _)) _ = return []
    clUpdate' (MkTupleUpdate SelectContent (KeyUpdateDelete b)) mr = do
        aset <- getPointPreimage b mr
        return $ fmap KeyUpdateDelete $ toList aset
    applyEdit' ::
           ContextUpdateEdit baseupdate (FiniteSetUpdate b)
        -> ReadFunction (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (ContextUpdateReader baseupdate (FiniteSetUpdate b))
    applyEdit' (MkTupleUpdateEdit SelectContent update) mr (MkTupleUpdateReader SelectContent rt) =
        applyEdit update (mr . MkTupleUpdateReader SelectContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextUpdateEdit baseupdate (FiniteSetUpdate b)]
        -> ReadFunction (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (ContextUpdateReader baseupdate (FiniteSetUpdate b))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> Readable m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkChangeLens _ _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> Readable m (UpdateReader baseupdate)
        -> m (Maybe [UpdateEdit baseupdate])
    putEditAB a b mr = do
        medits <-
            putEditBA @m [MkWholeReaderEdit b] $ \case
                MkTupleUpdateReader SelectContext rt -> mr rt
                MkTupleUpdateReader SelectContent ReadWhole -> return $ Known a
        return $
            fmap
                (\edits ->
                     mapMaybe
                         (\case
                              MkTupleUpdateEdit SelectContext edit -> Just edit
                              MkTupleUpdateEdit SelectContent _ -> Nothing)
                         edits)
                medits
    elPutEdit' ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdit' (KeyEditItem _ update) _ = never update
    elPutEdit' (KeyEditDelete a) mr = do
        mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit' (KeyEditInsertReplace a) mr = do
        b <- liftIO newb
        getComposeM $ do
            pedits <- MkComposeM $ putEditAB a (Known b) $ tupleReadFunction SelectContext mr
            return $
                (MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace b) :
                fmap (MkTupleUpdateEdit SelectContext) pedits
    elPutEdit' KeyEditClear mr = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        getComposeM $ do
            lpedits <-
                for (toList bs) $ \b -> do
                    aa <- lift $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
                    lpedits <-
                        for (toList aa) $ \a -> MkComposeM $ putEditAB a Unknown $ tupleReadFunction SelectContext mr
                    return $ mconcat lpedits
            return $ fmap (MkTupleUpdateEdit SelectContext) $ mconcat lpedits
    clPutEdits' ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    clPutEdits' [] _ = getComposeM $ return []
    clPutEdits' (e:ee) mr =
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit' @m e mr
            eea <- MkComposeM $ clPutEdits' ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkChangeLens clRead' clUpdate' clPutEdits'
