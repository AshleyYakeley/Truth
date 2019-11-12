module Pinafore.Base.Morphism
    ( APinaforeFunctionMorphism(..)
    , APinaforeLensMorphism(..)
    , PinaforeFunctionValue
    , PinaforeFunctionMorphism
    , applyPinaforeFunction
    , mapPinaforeFunctionMorphismBase
    , PinaforeLensValue
    , lensFunctionValue
    , PinaforeLensMorphism
    , mapPinaforeLensMorphismBase
    , funcPinaforeLensMorphism
    , nullPinaforeLensMorphism
    , bijectionPinaforeLensMorphism
    , pairPinaforeLensMorphism
    , eitherPinaforeLensMorphism
    , applyPinaforeLens
    , applyInversePinaforeLens
    , applyInversePinaforeLensSet
    , lensFunctionMorphism
    , lensInverseFunctionMorphism
    ) where

import Pinafore.Base.Know
import Shapes
import Truth.Core

type PinaforeLensValue baseupdate = EditLens baseupdate

data APinaforeFunctionMorphism baseupdate tt a b = MkAPinaforeFunctionMorphism
    { pfFuncRead :: forall m. MonadIO m => MutableRead m (UpdateReader baseupdate) -> a -> ApplyStack tt m b
    , pfUpdate :: forall m. MonadIO m => baseupdate -> MutableRead m (UpdateReader baseupdate) -> ApplyStack tt m Bool
    }

instance IsStack (MonadTransConstraint Monad) tt => Functor (APinaforeFunctionMorphism baseupdate tt a) where
    fmap ::
           forall p q.
           (p -> q)
        -> APinaforeFunctionMorphism baseupdate tt a p
        -> APinaforeFunctionMorphism baseupdate tt a q
    fmap pq (MkAPinaforeFunctionMorphism fr up) = let
        fr' :: forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> a
            -> ApplyStack tt m q
        fr' mr a =
            case transStackDict @Monad @tt @m of
                Dict -> fmap pq $ fr mr a
        in MkAPinaforeFunctionMorphism fr' up

type PinaforeFunctionMorphism baseupdate = Runnable2 (APinaforeFunctionMorphism baseupdate)

instance Functor (PinaforeFunctionMorphism baseupdate a) where
    fmap f fm = arr f . fm

instance Applicative (PinaforeFunctionMorphism baseupdate a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance RunnableMap (APinaforeFunctionMorphism baseupdate) where
    mapRunnable ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> KindFunction (APinaforeFunctionMorphism baseupdate tt1) (APinaforeFunctionMorphism baseupdate tt2)
    mapRunnable mapm =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkAPinaforeFunctionMorphism fr u :: APinaforeFunctionMorphism baseupdate tt1 a b) -> let
            pfFuncRead ::
                   forall m. MonadIO m
                => MutableRead m (UpdateReader baseupdate)
                -> a
                -> ApplyStack tt2 m b
            pfFuncRead mr a = tlfFunction mapm (Proxy @m) $ fr mr a
            pfUpdate ::
                   forall m. MonadIO m
                => baseupdate
                -> MutableRead m (UpdateReader baseupdate)
                -> ApplyStack tt2 m Bool
            pfUpdate e mr = tlfFunction mapm (Proxy @m) $ u e mr
            in MkAPinaforeFunctionMorphism {..}

instance RunnableCategory (APinaforeFunctionMorphism baseupdate) where
    ucId = let
        pfFuncRead _ = return
        pfUpdate _ _ = return False
        in MkAPinaforeFunctionMorphism {..}
    ucCompose ::
           forall tab tbc a b c. (MonadTransStackUnliftAll tab, MonadTransStackUnliftAll tbc)
        => APinaforeFunctionMorphism baseupdate tbc b c
        -> APinaforeFunctionMorphism baseupdate tab a b
        -> APinaforeFunctionMorphism baseupdate (Concat tbc tab) a c
    ucCompose (MkAPinaforeFunctionMorphism lbc ubc) (MkAPinaforeFunctionMorphism lab uab) = let
        pfFuncRead ::
               forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> a
            -> ApplyStack (Concat tbc tab) m c
        pfFuncRead =
            case concatMonadTransStackUnliftAllDict @tbc @tab of
                Dict ->
                    case transStackDict @MonadIO @(Concat tbc tab) @m of
                        Dict ->
                            \mr a -> do
                                b <- concatSndMFunction @tbc @tab @m $ lab mr a
                                concatFstMFunction @tbc @tab @m $ lbc mr b
        pfUpdate ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
            -> ApplyStack (Concat tbc tab) m Bool
        pfUpdate =
            case concatMonadTransStackUnliftAllDict @tbc @tab of
                Dict ->
                    case transStackDict @MonadIO @(Concat tbc tab) @m of
                        Dict ->
                            \update mr -> do
                                chab <- concatSndMFunction @tbc @tab @m $ uab update mr
                                chbc <- concatFstMFunction @tbc @tab @m $ ubc update mr
                                return $ chab || chbc
        in MkAPinaforeFunctionMorphism {..}

instance Arrow (PinaforeFunctionMorphism baseupdate) where
    arr ab =
        MkRunnable2
            cmEmpty
            MkAPinaforeFunctionMorphism {pfFuncRead = \_ a -> return $ ab a, pfUpdate = \_ _ -> return False}
    first :: forall b c d. PinaforeFunctionMorphism baseupdate b c -> PinaforeFunctionMorphism baseupdate (b, d) (c, d)
    first (MkRunnable2 (run@(MkTransStackRunner _) :: TransStackRunner tt) (MkAPinaforeFunctionMorphism bc pfUpdate)) =
        MkRunnable2 run $ let
            pfFuncRead ::
                   forall m. MonadIO m
                => MutableRead m (UpdateReader baseupdate)
                -> (b, d)
                -> ApplyStack tt m (c, d)
            pfFuncRead =
                case transStackDict @MonadIO @tt @m of
                    Dict ->
                        \mr (b, d) -> do
                            c <- bc mr b
                            return (c, d)
            in MkAPinaforeFunctionMorphism {..}
    second = cfmap

instance ArrowChoice (PinaforeFunctionMorphism baseupdate) where
    left (MkRunnable2 (run@(MkTransStackRunner _) :: TransStackRunner tt) (MkAPinaforeFunctionMorphism pfr pfUpdate)) =
        MkRunnable2 run $ let
            pfFuncRead ::
                   forall m. MonadIO m
                => MutableRead m (UpdateReader baseupdate)
                -> _
                -> ApplyStack tt m _
            pfFuncRead =
                case transStackDict @MonadIO @tt @m of
                    Dict ->
                        \mr ebd ->
                            case ebd of
                                Left b -> fmap Left (pfr mr b)
                                Right d -> return $ Right d
            in MkAPinaforeFunctionMorphism {..}
    right (MkRunnable2 (run@(MkTransStackRunner _) :: TransStackRunner tt) (MkAPinaforeFunctionMorphism pfr pfUpdate)) =
        MkRunnable2 run $ let
            pfFuncRead ::
                   forall m. MonadIO m
                => MutableRead m (UpdateReader baseupdate)
                -> _
                -> ApplyStack tt m _
            pfFuncRead =
                case transStackDict @MonadIO @tt @m of
                    Dict ->
                        \mr ebd ->
                            case ebd of
                                Left d -> return $ Left d
                                Right b -> fmap Right (pfr mr b)
            in MkAPinaforeFunctionMorphism {..}

instance Traversable f => CatFunctor (PinaforeFunctionMorphism baseupdate) (PinaforeFunctionMorphism baseupdate) f where
    cfmap :: forall a b. PinaforeFunctionMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate (f a) (f b)
    cfmap (MkRunnable2 (run@(MkTransStackRunner _) :: TransStackRunner tt) (MkAPinaforeFunctionMorphism f pfUpdate)) =
        MkRunnable2 run $ let
            pfFuncRead ::
                   forall m. MonadIO m
                => MutableRead m (UpdateReader baseupdate)
                -> f a
                -> ApplyStack tt m (f b)
            pfFuncRead =
                case transStackDict @MonadIO @tt @m of
                    Dict -> \mr fa -> for fa $ f mr
            in MkAPinaforeFunctionMorphism {..}

type PinaforeFunctionValue baseupdate t = UpdateFunction baseupdate (WholeUpdate t)

applyPinaforeFunction ::
       forall baseupdate a b.
       PinaforeFunctionMorphism baseupdate a b
    -> PinaforeFunctionValue baseupdate a
    -> PinaforeFunctionValue baseupdate b
applyPinaforeFunction = let
    call ::
           forall tt. MonadTransStackUnliftAll tt
        => APinaforeFunctionMorphism baseupdate tt a b
        -> AnUpdateFunction tt baseupdate (WholeUpdate a)
        -> AnUpdateFunction tt baseupdate (WholeUpdate b)
    call MkAPinaforeFunctionMorphism {..} MkAnUpdateFunction {..} = let
        getB ::
               forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> ApplyStack tt m b
        getB =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    \mr -> do
                        a <- ufGet mr ReadWhole
                        pfFuncRead mr a
        g :: ReadFunctionTT tt (UpdateReader baseupdate) (WholeReader b)
        g mr ReadWhole = getB mr
        u :: forall m. MonadIO m
          => baseupdate
          -> MutableRead m (UpdateReader baseupdate)
          -> ApplyStack tt m [WholeUpdate b]
        u =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    \pinedit mr -> do
                        ch <- pfUpdate pinedit mr
                        if ch
                            then do
                                b <- getB mr
                                return [MkWholeReaderUpdate b]
                            else do
                                edits <- ufUpdate pinedit mr
                                for edits $ \(MkWholeReaderUpdate a) -> fmap MkWholeReaderUpdate $ pfFuncRead mr a
        in MkAnUpdateFunction g u
    in joinRunnable2Maps call

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       UpdateFunction baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase ef morph = let
    call ::
           forall tt1 tt2.
           (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2, MonadTransStackUnliftAll (Concat tt1 tt2))
        => APinaforeFunctionMorphism baseA tt1 a b
        -> AnUpdateFunction tt2 baseB baseA
        -> APinaforeFunctionMorphism baseB (Concat tt1 tt2) a b
    call (MkAPinaforeFunctionMorphism frA updateA) aef = let
        readFunc :: ReadFunctionTT tt2 (UpdateReader baseB) (UpdateReader baseA)
        readFunc = ufGet aef
        frB :: forall m. MonadIO m
            => MutableRead m (UpdateReader baseB)
            -> a
            -> ApplyStack (Concat tt1 tt2) m b
        frB =
            case transStackConcatRefl @tt1 @tt2 @m of
                Refl ->
                    case transStackDict @MonadIO @tt2 @m of
                        Dict -> \mr a -> frA (readFunc mr) a
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (UpdateReader baseB)
            -> ApplyStack (Concat tt1 tt2) m Bool
        updateB =
            case transStackConcatRefl @tt1 @tt2 @m of
                Refl ->
                    case transStackDict @MonadIO @(Concat tt1 tt2) @m of
                        Dict ->
                            case transStackDict @MonadIO @tt2 @m of
                                Dict ->
                                    \beditB mr -> do
                                        beditAs <- concatSndMFunction @tt1 @tt2 @m $ ufUpdate aef beditB mr
                                        chs <- for beditAs $ \beditA -> updateA beditA $ readFunc mr
                                        return $ or chs
        in MkAPinaforeFunctionMorphism frB updateB
    in joinRunnable2s call morph ef

lensFunctionValue ::
       (IsEditUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => PinaforeLensValue baseupdate update
    -> PinaforeFunctionValue baseupdate (UpdateSubject update)
lensFunctionValue lens = convertUpdateFunction . editLensFunction lens

data APinaforeLensMorphism baseupdate t a b = MkAPinaforeLensMorphism
    { pmForward :: AnEditLens t (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    , pmInverse :: APinaforeFunctionMorphism baseupdate t b [a]
    }

instance RunnableMap (APinaforeLensMorphism baseupdate) where
    mapRunnable mapTM =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkAPinaforeLensMorphism fwd inv) ->
            MkAPinaforeLensMorphism
                ((unNestedMorphism $ unNestedMorphism $ mapRunnable mapTM) fwd)
                ((unNestedMorphism $ unNestedMorphism $ mapRunnable mapTM) inv)

type PinaforeLensMorphism baseupdate = Runnable2 (APinaforeLensMorphism baseupdate)

pairPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate a (b, c)
pairPinaforeLensMorphism = let
    call ::
           forall tt. MonadTransStackUnliftAll tt
        => APinaforeLensMorphism baseupdate tt a b
        -> APinaforeLensMorphism baseupdate tt a c
        -> APinaforeLensMorphism baseupdate tt a (b, c)
    call (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getB updateB) putEditsB) (MkAPinaforeFunctionMorphism frB updB)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getC updateC) putEditsC) (MkAPinaforeFunctionMorphism frC updC)) = let
        getBC ::
               forall m. MonadIO m
            => MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> MutableRead (ApplyStack tt m) (WholeReader (Know (b, c)))
        getBC =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    \mr ReadWhole -> do
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
            -> ApplyStack tt m [WholeUpdate (Know (b, c))]
        updateBC =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    \update mr -> do
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
            -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        putEditsBC =
            case transStackDict @MonadIO @tt @m of
                Dict ->
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
            -> ApplyStack tt m [a]
        frBC =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    \mr (b, c) -> do
                        aa1 <- frB mr b
                        aa2 <- frC mr c
                        return $ aa1 ++ aa2
        updBC ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
            -> ApplyStack tt m Bool
        updBC =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    \update mr -> do
                        eb <- updB update mr
                        case eb of
                            True -> return True
                            False -> updC update mr
        in MkAPinaforeLensMorphism
               (MkAnEditLens (MkAnUpdateFunction getBC updateBC) putEditsBC)
               (MkAPinaforeFunctionMorphism frBC updBC)
    in joinRunnable2Maps call

eitherPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate b c
    -> PinaforeLensMorphism baseupdate (Either a b) c
eitherPinaforeLensMorphism = let
    call ::
           forall tt. MonadTransStackUnliftAll tt
        => APinaforeLensMorphism baseupdate tt a c
        -> APinaforeLensMorphism baseupdate tt b c
        -> APinaforeLensMorphism baseupdate tt (Either a b) c
    call (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getA updateA) putEditsA) (MkAPinaforeFunctionMorphism frA updA)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getB updateB) putEditsB) (MkAPinaforeFunctionMorphism frB updB)) = let
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
        getAB ::
               ReadFunctionTT tt (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b)))) (WholeReader (Know c))
        getAB (mr :: MutableRead m _) ReadWhole =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    keab <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                    case keab of
                        Unknown -> return Unknown
                        Known (Left a) -> getA (getMRA mr a) ReadWhole
                        Known (Right b) -> getB (getMRB mr b) ReadWhole
        updateAB ::
               forall m. MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know (Either a b)))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
            -> ApplyStack tt m [WholeUpdate (Know c)]
        updateAB (MkTupleUpdate SelectContext update) mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    keab <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                    case keab of
                        Unknown -> return [MkWholeReaderUpdate Unknown]
                        Known (Left a) -> updateA (MkTupleUpdate SelectContext update) (getMRA mr a)
                        Known (Right b) -> updateB (MkTupleUpdate SelectContext update) (getMRB mr b)
        updateAB (MkTupleUpdate SelectContent (MkWholeReaderUpdate keab)) mr =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    case keab of
                        Unknown -> return [MkWholeReaderUpdate Unknown]
                        Known (Left a) ->
                            updateA (MkTupleUpdate SelectContent $ MkWholeReaderUpdate $ Known a) (getMRA mr a)
                        Known (Right b) ->
                            updateB (MkTupleUpdate SelectContent $ MkWholeReaderUpdate $ Known b) (getMRB mr b)
        putEditsAB ::
               forall m. MonadIO m
            => [WholeEdit (Know c)]
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
            -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know (Either a b)))])
        putEditsAB =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    wholePutEdits $ \kc mr -> do
                        keab <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent ReadWhole
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
            -> ApplyStack tt m [Either a b]
        frAB mr c =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    aa <- frA mr c
                    bb <- frB mr c
                    return $ fmap Left aa <> fmap Right bb
        updAB ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
            -> ApplyStack tt m Bool
        updAB update mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    u <- updA update mr
                    case u of
                        True -> return True
                        False -> updB update mr
        in MkAPinaforeLensMorphism
               (MkAnEditLens (MkAnUpdateFunction getAB updateAB) putEditsAB)
               (MkAPinaforeFunctionMorphism frAB updAB)
    in joinRunnable2Maps call

mapPinaforeLensMorphismBase ::
       forall baseA baseB a b. EditLens baseB baseA -> PinaforeLensMorphism baseA a b -> PinaforeLensMorphism baseB a b
mapPinaforeLensMorphismBase lens morph = let
    call ::
           forall tt1 tt2.
           (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2, MonadTransStackUnliftAll (Concat tt1 tt2))
        => APinaforeLensMorphism baseA tt1 a b
        -> AnEditLens tt2 baseB baseA
        -> APinaforeLensMorphism baseB (Concat tt1 tt2) a b
    call (MkAPinaforeLensMorphism fwdA (MkAPinaforeFunctionMorphism frA updateA)) alens = let
        readFunc :: ReadFunctionTT tt2 (UpdateReader baseB) (UpdateReader baseA)
        readFunc = ufGet $ elFunction alens
        fwdB :: AnEditLens (Concat tt1 tt2) (ContextUpdate baseB (WholeUpdate (Know a))) (WholeUpdate (Know b))
        fwdB = ucCompose fwdA $ liftContentAnEditLens alens
        frB :: forall m. MonadIO m
            => MutableRead m (UpdateReader baseB)
            -> b
            -> ApplyStack (Concat tt1 tt2) m [a]
        frB mr b =
            case transStackConcatRefl @tt1 @tt2 @m of
                Refl ->
                    case transStackDict @MonadIO @tt2 @m of
                        Dict -> frA (readFunc mr) b
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (UpdateReader baseB)
            -> ApplyStack (Concat tt1 tt2) m Bool
        updateB beditB mr =
            case transStackConcatRefl @tt1 @tt2 @m of
                Refl ->
                    case transStackDict @MonadIO @tt2 @m of
                        Dict ->
                            case transStackDict @MonadIO @(Concat tt1 tt2) @m of
                                Dict -> do
                                    beditAs <- concatSndMFunction @tt1 @tt2 @m $ ufUpdate (elFunction alens) beditB mr
                                    chs <- for beditAs $ \beditA -> updateA beditA $ readFunc mr
                                    return $ or chs
        invB :: APinaforeFunctionMorphism baseB (Concat tt1 tt2) b [a]
        invB = MkAPinaforeFunctionMorphism frB updateB
        plmB :: APinaforeLensMorphism baseB (Concat tt1 tt2) a b
        plmB = MkAPinaforeLensMorphism fwdB invB
        in plmB
    in joinRunnable2s call morph lens

bindReadContext ::
       MutableRead m (ContextUpdateReader updateX updateA)
    -> MutableRead m (UpdateReader updateB)
    -> MutableRead m (ContextUpdateReader updateX updateB)
bindReadContext mr _ (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
bindReadContext _ mr (MkTupleUpdateReader SelectContent rt) = mr rt

instance RunnableCategory (APinaforeLensMorphism baseupdate) where
    ucId = let
        ufGet :: ReadFunction (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know a))
        ufGet mr ReadWhole = mr $ MkTupleUpdateReader SelectContent ReadWhole
        ufUpdate ::
               MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know a)]
        ufUpdate (MkTupleUpdate SelectContext _) _ = return []
        ufUpdate (MkTupleUpdate SelectContent update) _ = return [update]
        elFunction :: AnUpdateFunction '[] (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        elFunction = MkAnUpdateFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit (Know a)]
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit SelectContent) edits
        pmForward :: AnEditLens '[] (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        pmForward = MkAnEditLens {..}
        pfFuncRead :: MonadIO m => MutableRead m (UpdateReader baseupdate) -> a -> m [a]
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: MonadIO m => baseupdate -> MutableRead m (UpdateReader baseupdate) -> m Bool
        pfUpdate _ _ = return False
        pmInverse :: APinaforeFunctionMorphism baseupdate '[] a [a]
        pmInverse = MkAPinaforeFunctionMorphism {..}
        in MkAPinaforeLensMorphism {..}
    ucCompose ::
           forall ttab ttbc a b c. (MonadTransStackUnliftAll ttab, MonadTransStackUnliftAll ttbc)
        => APinaforeLensMorphism baseupdate ttbc b c
        -> APinaforeLensMorphism baseupdate ttab a b
        -> APinaforeLensMorphism baseupdate (Concat ttbc ttab) a c
    ucCompose (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction bcGet bcUpdate) bcPutEdit) (MkAPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction abGet abUpdate) abPutEdit) (MkAPinaforeFunctionMorphism abInvFuncRead abInvUpdate)) =
        case concatMonadTransStackUnliftAllDict @ttbc @ttab of
            Dict -> let
                acGet ::
                       ReadFunctionTT (Concat ttbc ttab) (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know c))
                acGet (mra :: MutableRead m _) ReadWhole =
                    case transStackDict @MonadIO @(Concat ttbc ttab) @m of
                        Dict -> do
                            mb <- concatSndMFunction @ttbc @ttab @m $ abGet mra ReadWhole
                            concatFstMFunction @ttbc @ttab @m $
                                bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
                acUpdate ::
                       forall m. MonadIO m
                    => ContextUpdate baseupdate (WholeUpdate (Know a))
                    -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
                    -> ApplyStack (Concat ttbc ttab) m [WholeUpdate (Know c)]
                acUpdate (MkTupleUpdate SelectContext pinupdate) mra =
                    case transStackConcatRefl @ttbc @ttab @m of
                        Refl ->
                            case transStackDict @MonadIO @(Concat ttbc ttab) @m of
                                Dict ->
                                    case transStackDict @MonadIO @ttab @m of
                                        Dict -> do
                                            editbs <-
                                                concatSndMFunction @ttbc @ttab @m $
                                                abUpdate (MkTupleUpdate SelectContext pinupdate) mra
                                            editcs1 <-
                                                bcUpdate (MkTupleUpdate SelectContext pinupdate) $
                                                bindReadContext (remonadMutableRead (stackLift @ttab) mra) $ abGet mra
                                            editcss2 <-
                                                for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                                                    concatFstMFunction @ttbc @ttab @m $
                                                    bcUpdate (MkTupleUpdate SelectContent updateB) $
                                                    bindReadContext mra $ subjectToMutableRead mb
                                            return $ editcs1 ++ mconcat editcss2
                acUpdate (MkTupleUpdate SelectContent updateA) mra =
                    case transStackDict @MonadIO @(Concat ttbc ttab) @m of
                        Dict -> do
                            editbs <-
                                concatSndMFunction @ttbc @ttab @m $ abUpdate (MkTupleUpdate SelectContent updateA) mra
                            editcss <-
                                for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                                    concatFstMFunction @ttbc @ttab @m $
                                    bcUpdate (MkTupleUpdate SelectContent updateB) $
                                    bindReadContext mra $ subjectToMutableRead mb
                            return $ mconcat editcss
                acFunc ::
                       AnUpdateFunction (Concat ttbc ttab) (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
                acFunc = MkAnUpdateFunction acGet acUpdate
                acPutEdit ::
                       forall m. MonadIO m
                    => [WholeEdit (Know c)]
                    -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
                    -> ApplyStack (Concat ttbc ttab) m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
                acPutEdit editcs mra =
                    case transStackConcatRefl @ttbc @ttab @m of
                        Refl ->
                            case transStackDict @MonadIO @(Concat ttbc ttab) @m of
                                Dict ->
                                    case transStackDict @MonadIO @ttab @m of
                                        Dict ->
                                            getComposeM $ do
                                                editpbs <-
                                                    MkComposeM $
                                                    bcPutEdit editcs $
                                                    bindReadContext (remonadMutableRead (stackLift @ttab) mra) $
                                                    abGet mra
                                                case partitionContextEdits editpbs of
                                                    (pinedits, editbs) -> do
                                                        editpas2 <-
                                                            MkComposeM $
                                                            concatSndMFunction @ttbc @ttab @m $ abPutEdit editbs mra
                                                        return $
                                                            (fmap (MkTupleUpdateEdit SelectContext) pinedits) ++
                                                            editpas2
                acForward ::
                       AnEditLens (Concat ttbc ttab) (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
                acForward = MkAnEditLens acFunc acPutEdit
                acInvFuncRead ::
                       forall m. MonadIO m
                    => MutableRead m (UpdateReader baseupdate)
                    -> c
                    -> ApplyStack (Concat ttbc ttab) m [a]
                acInvFuncRead mr c =
                    case transStackDict @MonadIO @(Concat ttbc ttab) @m of
                        Dict ->
                            case transStackDict @MonadIO @ttab @m of
                                Dict -> do
                                    bset <- concatFstMFunction @ttbc @ttab @m $ bcInvFuncRead mr c
                                    asetset <- concatSndMFunction @ttbc @ttab @m $ for bset $ \b -> abInvFuncRead mr b
                                    return $ mconcat asetset
                acInvUpdate ::
                       forall m. MonadIO m
                    => baseupdate
                    -> MutableRead m (UpdateReader baseupdate)
                    -> ApplyStack (Concat ttbc ttab) m Bool
                acInvUpdate pedit mr =
                    case transStackDict @MonadIO @(Concat ttbc ttab) @m of
                        Dict -> do
                            chbc <- concatFstMFunction @ttbc @ttab @m $ bcInvUpdate pedit mr
                            chab <- concatSndMFunction @ttbc @ttab @m $ abInvUpdate pedit mr
                            return $ chbc || chab
                acInverse :: APinaforeFunctionMorphism baseupdate (Concat ttbc ttab) c [a]
                acInverse = MkAPinaforeFunctionMorphism acInvFuncRead acInvUpdate
                in MkAPinaforeLensMorphism acForward acInverse

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
    elFunction :: AnUpdateFunction '[] (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    elFunction = MkAnUpdateFunction {..}
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
    pmForward :: AnEditLens '[] (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    pmForward = MkAnEditLens {..}
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
    pmInverse :: APinaforeFunctionMorphism baseupdate '[] b [a]
    pmInverse = MkAPinaforeFunctionMorphism {..}
    in MkRunnable2 cmEmpty MkAPinaforeLensMorphism {..}

nullPinaforeLensMorphism :: forall baseupdate a b. PinaforeLensMorphism baseupdate a b
nullPinaforeLensMorphism = funcPinaforeLensMorphism (\_ -> Unknown) (\_ -> mempty) (\_ -> Nothing)

bijectionPinaforeLensMorphism :: Bijection a b -> PinaforeLensMorphism baseupdate a b
bijectionPinaforeLensMorphism (MkIsomorphism ab ba) =
    funcPinaforeLensMorphism (fmap ab) (\b -> opoint $ ba b) (\kb -> Just $ fmap ba kb)

instance IsoVariant (PinaforeLensMorphism baseupdate t) where
    isoMap ab ba m = bijectionPinaforeLensMorphism (MkIsomorphism ab ba) . m

instance IsoVariant' (PinaforeLensMorphism baseupdate) where
    isoMap' ab ba m = m . bijectionPinaforeLensMorphism (MkIsomorphism ba ab)

applyPinaforeLens ::
       PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (WholeUpdate (Know a))
    -> PinaforeLensValue baseupdate (WholeUpdate (Know b))
applyPinaforeLens (MkRunnable2 trun pm) val = (MkRunnable2 trun $ pmForward pm) . contextualiseEditLens val

lensFunctionMorphism ::
       forall baseupdate a b.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
lensFunctionMorphism (MkRunnable2 (trun :: TransStackRunner tt) MkAPinaforeLensMorphism {..}) = let
    funcRead ::
           forall m. MonadIO m
        => MutableRead m (UpdateReader baseupdate)
        -> Know a
        -> ApplyStack tt m (Know b)
    funcRead mr a = let
        mr' :: MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mr' (MkTupleUpdateReader SelectContext rt) = mr rt
        mr' (MkTupleUpdateReader SelectContent ReadWhole) = return a
        in ufGet (elFunction pmForward) mr' ReadWhole
    in MkRunnable2 trun $ MkAPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate b [a]
lensInverseFunctionMorphism (MkRunnable2 trun MkAPinaforeLensMorphism {..}) = MkRunnable2 trun pmInverse

pmInverseEditLens ::
       forall baseupdate a b. Eq a
    => PinaforeLensMorphism baseupdate a b
    -> EditLens (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
pmInverseEditLens (MkRunnable2 (trun@(MkTransStackRunner _) :: TransStackRunner tt) MkAPinaforeLensMorphism {..}) = let
    getFiniteSet ::
           forall m update. (MonadIO m, MonadIO (ApplyStack tt m))
        => Know b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> ApplyStack tt m (FiniteSet a)
    getFiniteSet (Known b) mr = fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getFiniteSet Unknown _ = return mempty
    fsetReadFunction ::
           ReadFunctionTT tt (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (WholeReader (FiniteSet a))
    fsetReadFunction (mr :: MutableRead m _) ReadWhole =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                kb <- stackLift @tt $ mr (MkTupleUpdateReader SelectContent ReadWhole)
                getFiniteSet kb mr
    ufGet :: ReadFunctionTT tt (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (FiniteSetReader a)
    ufGet (mr :: MutableRead m _) rt =
        case transStackDict @MonadIO @tt @m of
            Dict -> wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    ufUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know b))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> ApplyStack tt m [FiniteSetUpdate a]
    ufUpdate (MkTupleUpdate SelectContext pinupdate) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                ch <- pfUpdate pmInverse pinupdate $ tupleReadFunction SelectContext mr
                if ch
                    then do
                        kb <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                        aset <- getFiniteSet kb mr
                        aedits <- getReplaceEditsFromSubject aset
                        return $ fmap editUpdate aedits
                    else return []
    ufUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate kb)) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                aset <- getFiniteSet kb mr
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
    elFunction :: AnUpdateFunction tt (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
    elFunction = MkAnUpdateFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkAnEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (UpdateReader baseupdate)
        -> ApplyStack tt m (Maybe [UpdateEdit baseupdate])
    putEditAB a kb mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
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
        -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    elPutEdit (KeyEditItem _ update) _ = never update
    elPutEdit (KeyEditDelete a) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
                return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit (KeyEditInsertReplace a) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                kb <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                mpedits <- putEditAB a kb $ tupleReadFunction SelectContext mr
                return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit KeyEditClear mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                kb <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                aa <- getFiniteSet kb mr
                lmpedits <- for (toList aa) $ \a -> putEditAB a Unknown $ tupleReadFunction SelectContext mr
                return $
                    fmap (\lpedits -> fmap (MkTupleUpdateEdit SelectContext) $ mconcat lpedits) $ sequenceA lmpedits
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
        -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    elPutEdits [] _ =
        case transStackDict @MonadIO @tt @m of
            Dict -> getComposeM $ return []
    elPutEdits (e:ee) mr =
        case transStackDict @MonadIO @tt @m of
            Dict ->
                getComposeM $ do
                    ea <- MkComposeM $ elPutEdit e mr
                    eea <- MkComposeM $ elPutEdits ee $ applyEdits' ea mr
                    return $ ea ++ eea
    in MkRunnable2 trun $ MkAnEditLens {..}

applyInversePinaforeLens ::
       forall baseupdate a b. (Eq a, Eq b)
    => PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (WholeUpdate (Know b))
    -> PinaforeLensValue baseupdate (FiniteSetUpdate a)
applyInversePinaforeLens pm val = pmInverseEditLens pm . contextualiseEditLens val

pmInverseEditLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a b
    -> EditLens (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
pmInverseEditLensSet newb (MkRunnable2 (trun@(MkTransStackRunner _) :: TransStackRunner tt) MkAPinaforeLensMorphism {..}) = let
    getPointPreimage ::
           forall m update. (MonadIO m, MonadIO (ApplyStack tt m))
        => b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> ApplyStack tt m (FiniteSet a)
    getPointPreimage b mr = fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getSetPreimage ::
           forall m update. (MonadIO m, MonadIO (ApplyStack tt m))
        => FiniteSet b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> ApplyStack tt m (FiniteSet a)
    getSetPreimage bs mr = do
        as <- for bs $ \b -> getPointPreimage b mr
        return $ mconcat $ toList as
    getAB ::
           forall m update. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate update)
        -> a
        -> ApplyStack tt m (Know b)
    getAB mr a = let
        mra :: MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mra (MkTupleUpdateReader SelectContext rp) = mr $ MkTupleUpdateReader SelectContext rp
        mra (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known a
        in ufGet (elFunction pmForward) mra ReadWhole
    ufGet' :: ReadFunctionTT tt (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (FiniteSetReader a)
    ufGet' (mr :: MutableRead m _) KeyReadKeys =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                bs <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                getSetPreimage bs mr
    ufGet' (mr :: MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))) (KeyReadItem a ReadWhole) =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                kb <- getAB mr a
                case kb of
                    Known b -> do
                        mb <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent $ KeyReadItem b ReadWhole
                        case mb of
                            Just _ -> return $ Just a
                            Nothing -> return Nothing
                    Unknown -> return Nothing
    ufUpdate' ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (FiniteSetUpdate b)
        -> MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> ApplyStack tt m [FiniteSetUpdate a]
    ufUpdate' (MkTupleUpdate SelectContext pinaedit) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
                if ch
                    then do
                        bs <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                        aset <- getSetPreimage bs mr
                        aedits <- getReplaceEditsFromSubject aset
                        return $ fmap editUpdate aedits
                    else return []
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateItem _ update)) _ = never update
    ufUpdate' (MkTupleUpdate SelectContent KeyUpdateClear) _ =
        case transStackDict @MonadIO @tt @m of
            Dict -> return [KeyUpdateClear]
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateInsertReplace _)) _ =
        case transStackDict @MonadIO @tt @m of
            Dict -> return []
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateDelete b)) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                aset <- getPointPreimage b mr
                return $ fmap KeyUpdateDelete $ toList aset
    elFunction' :: AnUpdateFunction tt (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
    elFunction' = MkAnUpdateFunction ufGet' ufUpdate'
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
        -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkAnEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (UpdateReader baseupdate)
        -> ApplyStack tt m (Maybe [UpdateEdit baseupdate])
    putEditAB a b mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
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
        -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdit' (KeyEditItem _ update) _ = never update
    elPutEdit' (KeyEditDelete a) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
                return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit' (KeyEditInsertReplace a) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                b <- liftIO newb
                getComposeM $ do
                    pedits <- MkComposeM $ putEditAB a (Known b) $ tupleReadFunction SelectContext mr
                    return $
                        (MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace b) :
                        fmap (MkTupleUpdateEdit SelectContext) pedits
    elPutEdit' KeyEditClear mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                bs <- stackLift @tt $ mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                getComposeM $ do
                    lpedits <-
                        for (toList bs) $ \b -> do
                            aa <- lift $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
                            lpedits <-
                                for (toList aa) $ \a ->
                                    MkComposeM $ putEditAB a Unknown $ tupleReadFunction SelectContext mr
                            return $ mconcat lpedits
                    return $ fmap (MkTupleUpdateEdit SelectContext) $ mconcat lpedits
    elPutEdits' ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> ApplyStack tt m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdits' [] _ =
        case transStackDict @MonadIO @tt @m of
            Dict -> getComposeM $ return []
    elPutEdits' (e:ee) mr =
        case transStackDict @MonadIO @tt @m of
            Dict ->
                getComposeM $ do
                    ea <- MkComposeM $ elPutEdit' @m e mr
                    eea <- MkComposeM $ elPutEdits' ee $ applyEdits' ea mr
                    return $ ea ++ eea
    in MkRunnable2 trun $ MkAnEditLens elFunction' elPutEdits'

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (FiniteSetUpdate b)
    -> PinaforeLensValue baseupdate (FiniteSetUpdate a)
applyInversePinaforeLensSet newb pm val = pmInverseEditLensSet newb pm . contextualiseEditLens val
