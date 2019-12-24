module Pinafore.Base.Morphism
    ( PinaforeFunctionMorphism(..)
    , pinaforeFunctionMorphismUpdateFunction
    , PinaforeLensMorphism(..)
    , pinaforeLensMorphismInverseEditLens
    , pinaforeLensMorphismInverseEditLensSet
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
    { pfFuncRead :: forall m. MonadIO m => MutableRead m (UpdateReader baseupdate) -> a -> m b
    , pfUpdate :: forall m. MonadIO m => baseupdate -> MutableRead m (UpdateReader baseupdate) -> m Bool
    }

instance Functor (PinaforeFunctionMorphism baseupdate a) where
    fmap :: forall p q. (p -> q) -> PinaforeFunctionMorphism baseupdate a p -> PinaforeFunctionMorphism baseupdate a q
    fmap pq (MkPinaforeFunctionMorphism fr up) = let
        fr' :: forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
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
            => MutableRead m (UpdateReader baseupdate)
            -> a
            -> m c
        pfFuncRead mr a = do
            b <- lab mr a
            lbc mr b
        pfUpdate ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
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
            => MutableRead m (UpdateReader baseupdate)
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
            => MutableRead m (UpdateReader baseupdate)
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
            => MutableRead m (UpdateReader baseupdate)
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
            => MutableRead m (UpdateReader baseupdate)
            -> f a
            -> m (f b)
        pfFuncRead mr fa = for fa $ f mr
        in MkPinaforeFunctionMorphism {..}

pinaforeFunctionMorphismUpdateFunction ::
       forall baseupdate a b.
       PinaforeFunctionMorphism baseupdate a b
    -> UpdateFunction (ContextUpdate baseupdate (WholeUpdate a)) (WholeUpdate b)
pinaforeFunctionMorphismUpdateFunction MkPinaforeFunctionMorphism {..} = let
    getB ::
           forall m. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate (WholeUpdate a))
        -> m b
    getB mr = do
        a <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        pfFuncRead (tupleReadFunction SelectContext mr) a
    ufGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate a)) (WholeReader b)
    ufGet mr ReadWhole = getB mr
    ufUpdate ::
           forall m. MonadIO m
        => (ContextUpdate baseupdate (WholeUpdate a))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate a))
        -> m [WholeUpdate b]
    ufUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- pfUpdate pinupdate $ tupleReadFunction SelectContext mr
        if ch
            then do
                b <- getB mr
                return [MkWholeReaderUpdate b]
            else return []
    ufUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) mr = do
        b <- pfFuncRead (tupleReadFunction SelectContext mr) a
        return [MkWholeReaderUpdate b]
    in MkUpdateFunction {..}

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       UpdateFunction baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase aef (MkPinaforeFunctionMorphism frA updateA) = let
    readFunc :: ReadFunction (UpdateReader baseB) (UpdateReader baseA)
    readFunc = ufGet aef
    frB :: forall m. MonadIO m
        => MutableRead m (UpdateReader baseB)
        -> a
        -> m b
    frB mr a = frA (readFunc mr) a
    updateB ::
           forall m. MonadIO m
        => baseB
        -> MutableRead m (UpdateReader baseB)
        -> m Bool
    updateB beditB mr = do
        beditAs <- ufUpdate aef beditB mr
        chs <- for beditAs $ \beditA -> updateA beditA $ readFunc mr
        return $ or chs
    in MkPinaforeFunctionMorphism frB updateB

data PinaforeLensMorphism baseupdate a b = MkPinaforeLensMorphism
    { pmForward :: EditLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    , pmInverse :: PinaforeFunctionMorphism baseupdate b [a]
    }

pairPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate a (b, c)
pairPinaforeLensMorphism (MkPinaforeLensMorphism (MkEditLens (MkUpdateFunction getB updateB) putEditsB) (MkPinaforeFunctionMorphism frB updB)) (MkPinaforeLensMorphism (MkEditLens (MkUpdateFunction getC updateC) putEditsC) (MkPinaforeFunctionMorphism frC updC)) = let
    getBC ::
           forall m. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> MutableRead m (WholeReader (Know (b, c)))
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
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
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
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
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
        => MutableRead m (UpdateReader baseupdate)
        -> (b, c)
        -> m [a]
    frBC mr (b, c) = do
        aa1 <- frB mr b
        aa2 <- frC mr c
        return $ aa1 ++ aa2
    updBC ::
           forall m. MonadIO m
        => baseupdate
        -> MutableRead m (UpdateReader baseupdate)
        -> m Bool
    updBC update mr = do
        eb <- updB update mr
        case eb of
            True -> return True
            False -> updC update mr
    in MkPinaforeLensMorphism
           (MkEditLens (MkUpdateFunction getBC updateBC) putEditsBC)
           (MkPinaforeFunctionMorphism frBC updBC)

eitherPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate b c
    -> PinaforeLensMorphism baseupdate (Either a b) c
eitherPinaforeLensMorphism (MkPinaforeLensMorphism (MkEditLens (MkUpdateFunction getA updateA) putEditsA) (MkPinaforeFunctionMorphism frA updA)) (MkPinaforeLensMorphism (MkEditLens (MkUpdateFunction getB updateB) putEditsB) (MkPinaforeFunctionMorphism frB updB)) = let
    getMRA ::
           forall m. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
        -> a
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
    getMRA mr _ (MkTupleUpdateReader SelectContext r) = mr $ MkTupleUpdateReader SelectContext r
    getMRA _ a (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known a
    getMRB ::
           forall m. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
        -> b
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
    getMRB mr _ (MkTupleUpdateReader SelectContext r) = mr $ MkTupleUpdateReader SelectContext r
    getMRB _ b (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known b
    getAB :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b)))) (WholeReader (Know c))
    getAB (mr :: MutableRead m _) ReadWhole = do
        keab <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        case keab of
            Unknown -> return Unknown
            Known (Left a) -> getA (getMRA mr a) ReadWhole
            Known (Right b) -> getB (getMRB mr b) ReadWhole
    updateAB ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know (Either a b)))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
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
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
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
        => MutableRead m (UpdateReader baseupdate)
        -> c
        -> m [Either a b]
    frAB mr c = do
        aa <- frA mr c
        bb <- frB mr c
        return $ fmap Left aa <> fmap Right bb
    updAB ::
           forall m. MonadIO m
        => baseupdate
        -> MutableRead m (UpdateReader baseupdate)
        -> m Bool
    updAB update mr = do
        u <- updA update mr
        case u of
            True -> return True
            False -> updB update mr
    in MkPinaforeLensMorphism
           (MkEditLens (MkUpdateFunction getAB updateAB) putEditsAB)
           (MkPinaforeFunctionMorphism frAB updAB)

mapPinaforeLensMorphismBase ::
       forall baseA baseB a b. EditLens baseB baseA -> PinaforeLensMorphism baseA a b -> PinaforeLensMorphism baseB a b
mapPinaforeLensMorphismBase alens (MkPinaforeLensMorphism fwdA (MkPinaforeFunctionMorphism frA updateA)) = let
    readFunc :: ReadFunction (UpdateReader baseB) (UpdateReader baseA)
    readFunc = ufGet $ elFunction alens
    fwdB :: EditLens (ContextUpdate baseB (WholeUpdate (Know a))) (WholeUpdate (Know b))
    fwdB = fwdA . liftContentEditLens alens
    frB :: forall m. MonadIO m
        => MutableRead m (UpdateReader baseB)
        -> b
        -> m [a]
    frB mr b = frA (readFunc mr) b
    updateB ::
           forall m. MonadIO m
        => baseB
        -> MutableRead m (UpdateReader baseB)
        -> m Bool
    updateB beditB mr = do
        beditAs <- ufUpdate (elFunction alens) beditB mr
        chs <- for beditAs $ \beditA -> updateA beditA $ readFunc mr
        return $ or chs
    invB :: PinaforeFunctionMorphism baseB b [a]
    invB = MkPinaforeFunctionMorphism frB updateB
    plmB :: PinaforeLensMorphism baseB a b
    plmB = MkPinaforeLensMorphism fwdB invB
    in plmB

bindReadContext ::
       MutableRead m (ContextUpdateReader updateX updateA)
    -> MutableRead m (UpdateReader updateB)
    -> MutableRead m (ContextUpdateReader updateX updateB)
bindReadContext mr _ (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
bindReadContext _ mr (MkTupleUpdateReader SelectContent rt) = mr rt

instance Category (PinaforeLensMorphism baseupdate) where
    id = let
        ufGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know a))
        ufGet mr ReadWhole = mr $ MkTupleUpdateReader SelectContent ReadWhole
        ufUpdate ::
               MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know a)]
        ufUpdate (MkTupleUpdate SelectContext _) _ = return []
        ufUpdate (MkTupleUpdate SelectContent update) _ = return [update]
        elFunction :: UpdateFunction (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        elFunction = MkUpdateFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit (Know a)]
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit SelectContent) edits
        pmForward :: EditLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        pmForward = MkEditLens {..}
        pfFuncRead :: MonadIO m => MutableRead m (UpdateReader baseupdate) -> a -> m [a]
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: MonadIO m => baseupdate -> MutableRead m (UpdateReader baseupdate) -> m Bool
        pfUpdate _ _ = return False
        pmInverse :: PinaforeFunctionMorphism baseupdate a [a]
        pmInverse = MkPinaforeFunctionMorphism {..}
        in MkPinaforeLensMorphism {..}
    (.) :: forall a b c.
           PinaforeLensMorphism baseupdate b c
        -> PinaforeLensMorphism baseupdate a b
        -> PinaforeLensMorphism baseupdate a c
    MkPinaforeLensMorphism (MkEditLens (MkUpdateFunction bcGet bcUpdate) bcPutEdit) (MkPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate) . MkPinaforeLensMorphism (MkEditLens (MkUpdateFunction abGet abUpdate) abPutEdit) (MkPinaforeFunctionMorphism abInvFuncRead abInvUpdate) = let
        acGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know c))
        acGet (mra :: MutableRead m _) ReadWhole = do
            mb <- abGet mra ReadWhole
            bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know c)]
        acUpdate (MkTupleUpdate SelectContext pinupdate) mra = do
            editbs <- abUpdate (MkTupleUpdate SelectContext pinupdate) mra
            editcs1 <- bcUpdate (MkTupleUpdate SelectContext pinupdate) $ bindReadContext mra $ abGet mra
            editcss2 <-
                for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                    bcUpdate (MkTupleUpdate SelectContent updateB) $ bindReadContext mra $ subjectToMutableRead mb
            return $ editcs1 ++ mconcat editcss2
        acUpdate (MkTupleUpdate SelectContent updateA) mra = do
            editbs <- abUpdate (MkTupleUpdate SelectContent updateA) mra
            editcss <-
                for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                    bcUpdate (MkTupleUpdate SelectContent updateB) $ bindReadContext mra $ subjectToMutableRead mb
            return $ mconcat editcss
        acFunc :: UpdateFunction (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
        acFunc = MkUpdateFunction acGet acUpdate
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit (Know c)]
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        acPutEdit editcs mra =
            getComposeM $ do
                editpbs <- MkComposeM $ bcPutEdit editcs $ bindReadContext mra $ abGet mra
                case partitionContextEdits editpbs of
                    (pinedits, editbs) -> do
                        editpas2 <- MkComposeM $ abPutEdit editbs mra
                        return $ (fmap (MkTupleUpdateEdit SelectContext) pinedits) ++ editpas2
        acForward :: EditLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
        acForward = MkEditLens acFunc acPutEdit
        acInvFuncRead ::
               forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> c
            -> m [a]
        acInvFuncRead mr c = do
            bset <- bcInvFuncRead mr c
            asetset <- for bset $ \b -> abInvFuncRead mr b
            return $ mconcat asetset
        acInvUpdate ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
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
    ufGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know b))
    ufGet mr ReadWhole = fmap ab $ mr $ MkTupleUpdateReader SelectContent ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know a))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m [WholeUpdate (Know b)]
    ufUpdate (MkTupleUpdate SelectContext _) _ = return []
    ufUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) _ = return [MkWholeReaderUpdate $ ab a]
    elFunction :: UpdateFunction (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    elPutEdits edits _ =
        return $
        case lastWholeEdit edits of
            Nothing -> Just []
            Just kb -> fmap (pure . MkTupleUpdateEdit SelectContent . MkWholeReaderEdit) (bma kb)
    pmForward :: EditLens (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    pmForward = MkEditLens {..}
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m (UpdateReader baseupdate)
        -> b
        -> m [a]
    pfFuncRead _ b = return $ bsa b
    pfUpdate ::
           forall m. MonadIO m
        => baseupdate
        -> MutableRead m (UpdateReader baseupdate)
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
        => MutableRead m (UpdateReader baseupdate)
        -> Know a
        -> m (Know b)
    funcRead mr a = let
        mr' :: MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mr' (MkTupleUpdateReader SelectContext rt) = mr rt
        mr' (MkTupleUpdateReader SelectContent ReadWhole) = return a
        in ufGet (elFunction pmForward) mr' ReadWhole
    in MkPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate b [a]
lensInverseFunctionMorphism = pmInverse

pinaforeLensMorphismInverseEditLens ::
       forall baseupdate a b. Eq a
    => PinaforeLensMorphism baseupdate a b
    -> EditLens (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
pinaforeLensMorphismInverseEditLens MkPinaforeLensMorphism {..} = let
    getFiniteSet ::
           forall m update. (MonadIO m, MonadIO (m))
        => Know b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> m (FiniteSet a)
    getFiniteSet (Known b) mr = fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getFiniteSet Unknown _ = return mempty
    fsetReadFunction :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (WholeReader (FiniteSet a))
    fsetReadFunction (mr :: MutableRead m _) ReadWhole = do
        kb <- mr (MkTupleUpdateReader SelectContent ReadWhole)
        getFiniteSet kb mr
    ufGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (FiniteSetReader a)
    ufGet (mr :: MutableRead m _) rt = wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    ufUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know b))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> m [FiniteSetUpdate a]
    ufUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- pfUpdate pmInverse pinupdate $ tupleReadFunction SelectContext mr
        if ch
            then do
                kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                aset <- getFiniteSet kb mr
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
            else return []
    ufUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate kb)) mr = do
        aset <- getFiniteSet kb mr
        aedits <- getReplaceEditsFromSubject aset
        return $ fmap editUpdate aedits
    elFunction :: UpdateFunction (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
    elFunction = MkUpdateFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (UpdateReader baseupdate)
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
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
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
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    elPutEdits [] _ = getComposeM $ return []
    elPutEdits (e:ee) mr =
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit e mr
            eea <- MkComposeM $ elPutEdits ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkEditLens {..}

pinaforeLensMorphismInverseEditLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a b
    -> EditLens (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
pinaforeLensMorphismInverseEditLensSet newb MkPinaforeLensMorphism {..} = let
    getPointPreimage ::
           forall m update. (MonadIO m, MonadIO (m))
        => b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> m (FiniteSet a)
    getPointPreimage b mr = fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getSetPreimage ::
           forall m update. (MonadIO m, MonadIO (m))
        => FiniteSet b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> m (FiniteSet a)
    getSetPreimage bs mr = do
        as <- for bs $ \b -> getPointPreimage b mr
        return $ mconcat $ toList as
    getAB ::
           forall m update. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate update)
        -> a
        -> m (Know b)
    getAB mr a = let
        mra :: MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mra (MkTupleUpdateReader SelectContext rp) = mr $ MkTupleUpdateReader SelectContext rp
        mra (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known a
        in ufGet (elFunction pmForward) mra ReadWhole
    ufGet' :: ReadFunction (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (FiniteSetReader a)
    ufGet' (mr :: MutableRead m _) KeyReadKeys = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        getSetPreimage bs mr
    ufGet' (mr :: MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))) (KeyReadItem a ReadWhole) = do
        kb <- getAB mr a
        case kb of
            Known b -> do
                mb <- mr $ MkTupleUpdateReader SelectContent $ KeyReadItem b ReadWhole
                case mb of
                    Just _ -> return $ Just a
                    Nothing -> return Nothing
            Unknown -> return Nothing
    ufUpdate' ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (FiniteSetUpdate b)
        -> MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m [FiniteSetUpdate a]
    ufUpdate' (MkTupleUpdate SelectContext pinaedit) mr = do
        ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
        if ch
            then do
                bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                aset <- getSetPreimage bs mr
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
            else return []
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateItem _ update)) _ = never update
    ufUpdate' (MkTupleUpdate SelectContent KeyUpdateClear) _ = return [KeyUpdateClear]
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateInsertReplace _)) _ = return []
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateDelete b)) mr = do
        aset <- getPointPreimage b mr
        return $ fmap KeyUpdateDelete $ toList aset
    elFunction' :: UpdateFunction (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
    elFunction' = MkUpdateFunction ufGet' ufUpdate'
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
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (UpdateReader baseupdate)
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
        -> MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
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
    elPutEdits' ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdits' [] _ = getComposeM $ return []
    elPutEdits' (e:ee) mr =
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit' @m e mr
            eea <- MkComposeM $ elPutEdits' ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkEditLens elFunction' elPutEdits'
