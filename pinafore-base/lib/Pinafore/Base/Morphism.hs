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

data APinaforeFunctionMorphism baseupdate t a b = MkAPinaforeFunctionMorphism
    { pfFuncRead :: forall m. MonadIO m => MutableRead m (UpdateReader baseupdate) -> a -> t m b
    , pfUpdate :: forall m. MonadIO m => baseupdate -> MutableRead m (UpdateReader baseupdate) -> t m Bool
    }

instance MonadTransConstraint MonadIO t => Functor (APinaforeFunctionMorphism baseupdate t a) where
    fmap ab (MkAPinaforeFunctionMorphism fr up) =
        MkAPinaforeFunctionMorphism (\mr a -> withTransConstraintTM @MonadIO $ fmap ab $ fr mr a) up

type PinaforeFunctionMorphism baseupdate = CloseUnlift (APinaforeFunctionMorphism baseupdate)

instance Functor (PinaforeFunctionMorphism baseupdate a) where
    fmap f fm = arr f . fm

instance Applicative (PinaforeFunctionMorphism baseupdate a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance Unliftable (APinaforeFunctionMorphism baseupdate) where
    fmapUnliftable mapm (MkAPinaforeFunctionMorphism fr u) =
        MkAPinaforeFunctionMorphism (\mr a -> mapm $ fr mr a) (\e mr -> mapm $ u e mr)

instance UnliftCategory (APinaforeFunctionMorphism baseupdate) where
    ucId = let
        pfFuncRead _ = return
        pfUpdate _ _ = return False
        in MkAPinaforeFunctionMorphism {..}
    ucCompose (MkAPinaforeFunctionMorphism lbc ubc) (MkAPinaforeFunctionMorphism lab uab) =
        MkAPinaforeFunctionMorphism
            { pfFuncRead =
                  \mr a ->
                      withTransConstraintTM @MonadIO $ do
                          b <- lift2ComposeT'' $ lab mr a
                          lift1ComposeT $ lbc mr b
            , pfUpdate =
                  \update mr ->
                      withTransConstraintTM @MonadIO $ do
                          chab <- lift2ComposeT'' $ uab update mr
                          chbc <- lift1ComposeT $ ubc update mr
                          return $ chab || chbc
            }

instance Arrow (PinaforeFunctionMorphism baseupdate) where
    arr ab =
        MkCloseUnlift
            wUnIdentityT
            MkAPinaforeFunctionMorphism {pfFuncRead = \_ a -> return $ ab a, pfUpdate = \_ _ -> return False}
    first (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism bc u)) =
        MkCloseUnlift unlift $
        MkAPinaforeFunctionMorphism
            (\mr (b, d) ->
                 withTransConstraintTM @MonadIO $ do
                     c <- bc mr b
                     return (c, d))
            u
    second = cfmap

instance ArrowChoice (PinaforeFunctionMorphism baseupdate) where
    left (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism pfr pfu)) =
        MkCloseUnlift unlift $
        MkAPinaforeFunctionMorphism
            (\mr ebd ->
                 withTransConstraintTM @Monad $
                 case ebd of
                     Left b -> fmap Left (pfr mr b)
                     Right d -> return $ Right d)
            pfu
    right (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism pfr pfu)) =
        MkCloseUnlift unlift $
        MkAPinaforeFunctionMorphism
            (\mr ebd ->
                 withTransConstraintTM @Monad $
                 case ebd of
                     Left d -> return $ Left d
                     Right b -> fmap Right (pfr mr b))
            pfu

instance Traversable f => CatFunctor (PinaforeFunctionMorphism baseupdate) (PinaforeFunctionMorphism baseupdate) f where
    cfmap (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism f u)) =
        MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism (\mr fa -> withTransConstraintTM @MonadIO $ for fa $ f mr) u

type PinaforeFunctionValue baseupdate t = UpdateFunction baseupdate (WholeUpdate t)

applyPinaforeFunction ::
       forall baseupdate a b.
       PinaforeFunctionMorphism baseupdate a b
    -> PinaforeFunctionValue baseupdate a
    -> PinaforeFunctionValue baseupdate b
applyPinaforeFunction =
    joinUnliftables $ \MkAPinaforeFunctionMorphism {..} (MkAnUpdateFunction {..} :: AnUpdateFunction t baseupdate (WholeUpdate a)) -> let
        getB ::
               forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> t m b
        getB mr =
            withTransConstraintTM @MonadIO $ do
                a <- ufGet mr ReadWhole
                pfFuncRead mr a
        g :: ReadFunctionT t (UpdateReader baseupdate) (WholeReader b)
        g mr ReadWhole = getB mr
        u :: forall m. MonadIO m
          => baseupdate
          -> MutableRead m (UpdateReader baseupdate)
          -> t m [WholeUpdate b]
        u pinedit mr =
            withTransConstraintTM @MonadIO $ do
                ch <- pfUpdate pinedit mr
                if ch
                    then do
                        b <- getB mr
                        return [MkWholeReaderUpdate b]
                    else do
                        edits <- ufUpdate pinedit mr
                        for edits $ \(MkWholeReaderUpdate a) -> fmap MkWholeReaderUpdate $ pfFuncRead mr a
        in MkAnUpdateFunction g u

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       UpdateFunction baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase ef morph = let
    call ::
           forall t1 t2. (MonadTransUntrans t1, MonadTransUntrans t2)
        => APinaforeFunctionMorphism baseA t1 a b
        -> AnUpdateFunction t2 baseB baseA
        -> APinaforeFunctionMorphism baseB (ComposeT t1 t2) a b
    call (MkAPinaforeFunctionMorphism frA updateA) aef = let
        readFunc :: ReadFunctionT t2 (UpdateReader baseB) (UpdateReader baseA)
        readFunc = ufGet aef
        frB :: forall m. MonadIO m
            => MutableRead m (UpdateReader baseB)
            -> a
            -> ComposeT t1 t2 m b
        frB mr a = mkComposeT $ frA (readFunc mr) a
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (UpdateReader baseB)
            -> ComposeT t1 t2 m Bool
        updateB beditB mr =
            withTransConstraintTM @MonadIO $ do
                beditAs <- lift2ComposeT' $ ufUpdate aef beditB mr
                chs <- for beditAs $ \beditA -> mkComposeT $ updateA beditA $ readFunc mr
                return $ or chs
        in MkAPinaforeFunctionMorphism frB updateB
    in joinUnlifts call morph ef

lensFunctionValue ::
       (IsEditUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => PinaforeLensValue baseupdate update
    -> PinaforeFunctionValue baseupdate (UpdateSubject update)
lensFunctionValue lens = convertUpdateFunction . editLensFunction lens

data APinaforeLensMorphism baseupdate t a b = MkAPinaforeLensMorphism
    { pmForward :: AnEditLens t (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    , pmInverse :: APinaforeFunctionMorphism baseupdate t b [a]
    }

instance Unliftable (APinaforeLensMorphism baseupdate) where
    fmapUnliftable mapTM (MkAPinaforeLensMorphism fwd inv) =
        MkAPinaforeLensMorphism (fmapUnliftable mapTM fwd) (fmapUnliftable mapTM inv)

type PinaforeLensMorphism baseupdate = CloseUnlift (APinaforeLensMorphism baseupdate)

mkComposeT ::
       forall t1 t2 m a. (MonadTransConstraint MonadIO t2, MonadIO m)
    => (MonadIO (t2 m) => t1 (t2 m) a)
    -> ComposeT t1 t2 m a
mkComposeT =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> MkComposeT

pairPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate a (b, c)
pairPinaforeLensMorphism = let
    call ::
           forall t1 t2. (MonadTransUntrans t1, MonadTransUntrans t2)
        => APinaforeLensMorphism baseupdate t1 a b
        -> APinaforeLensMorphism baseupdate t2 a c
        -> APinaforeLensMorphism baseupdate (ComposeT t1 t2) a (b, c)
    call (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getB updateB) putEditsB) (MkAPinaforeFunctionMorphism frB updB)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getC updateC) putEditsC) (MkAPinaforeFunctionMorphism frC updC)) = let
        getBC ::
               ReadFunctionT (ComposeT t1 t2) (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know ( b
                                                                                                                         , c)))
        getBC mr ReadWhole =
            withTransConstraintTM @MonadIO $ do
                kb <- lift1ComposeT $ getB mr ReadWhole
                case kb of
                    Unknown -> return Unknown
                    Known b -> do
                        kc <- lift2ComposeT' $ getC mr ReadWhole
                        return $ fmap (\c -> (b, c)) kc
        updateBC ::
               forall m. MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> ComposeT t1 t2 m [WholeUpdate (Know (b, c))]
        updateBC update mr =
            withTransConstraintTM @MonadIO $ do
                ebs <- lift1ComposeT $ updateB update mr
                ecs <- lift2ComposeT' $ updateC update mr
                case (lastM ebs, lastM ecs) of
                    (Nothing, Nothing) -> return []
                    (Just (MkWholeReaderUpdate Unknown), _) -> return [MkWholeReaderUpdate Unknown]
                    (_, Just (MkWholeReaderUpdate Unknown)) -> return [MkWholeReaderUpdate Unknown]
                    (Just (MkWholeReaderUpdate (Known b)), Just (MkWholeReaderUpdate (Known c))) ->
                        return [MkWholeReaderUpdate $ Known (b, c)]
                    (Just (MkWholeReaderUpdate (Known b)), Nothing) -> do
                        kc <- lift2ComposeT' $ getC mr ReadWhole
                        return [MkWholeReaderUpdate $ fmap (\c -> (b, c)) kc]
                    (Nothing, Just (MkWholeReaderUpdate (Known c))) -> do
                        kb <- lift1ComposeT $ getB mr ReadWhole
                        return [MkWholeReaderUpdate $ fmap (\b -> (b, c)) kb]
        putEditsBC ::
               forall m. MonadIO m
            => (Know (b, c))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> ComposeT t1 t2 m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        putEditsBC kbc mr =
            withTransConstraintTM @MonadIO $
            case kbc of
                Unknown -> return Nothing -- can't delete
                (Known (b, c)) ->
                    getComposeM $ do
                        aa1 <- MkComposeM $ lift1ComposeT $ putEditsB [MkWholeReaderEdit $ Known b] mr
                        aa2 <- MkComposeM $ lift2ComposeT' $ putEditsC [MkWholeReaderEdit $ Known c] mr
                        return $ aa1 ++ aa2
        frBC ::
               forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> (b, c)
            -> ComposeT t1 t2 m [a]
        frBC mr (b, c) =
            withTransConstraintTM @MonadIO $ do
                aa1 <- lift1ComposeT $ frB mr b
                aa2 <- lift2ComposeT' $ frC mr c
                return $ aa1 ++ aa2
        updBC ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
            -> ComposeT t1 t2 m Bool
        updBC update mr =
            withTransConstraintTM @MonadIO $ do
                eb <- lift1ComposeT $ updB update mr
                case eb of
                    True -> return True
                    False -> lift2ComposeT' $ updC update mr
        in MkAPinaforeLensMorphism
               (MkAnEditLens (MkAnUpdateFunction getBC updateBC) $ wholePutEdits putEditsBC)
               (MkAPinaforeFunctionMorphism frBC updBC)
    in joinUnlifts call

eitherPinaforeLensMorphism ::
       forall baseupdate a b c.
       PinaforeLensMorphism baseupdate a c
    -> PinaforeLensMorphism baseupdate b c
    -> PinaforeLensMorphism baseupdate (Either a b) c
eitherPinaforeLensMorphism = let
    call ::
           forall t. MonadTransUntrans t
        => APinaforeLensMorphism baseupdate t a c
        -> APinaforeLensMorphism baseupdate t b c
        -> APinaforeLensMorphism baseupdate t (Either a b) c
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
               ReadFunctionT t (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b)))) (WholeReader (Know c))
        getAB (mr :: MutableRead m _) ReadWhole =
            withTransConstraintTM @MonadIO $ do
                keab <- lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                case keab of
                    Unknown -> return Unknown
                    Known (Left a) -> getA (getMRA mr a) ReadWhole
                    Known (Right b) -> getB (getMRB mr b) ReadWhole
        updateAB ::
               forall m. MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know (Either a b)))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
            -> t m [WholeUpdate (Know c)]
        updateAB (MkTupleUpdate SelectContext update) mr =
            withTransConstraintTM @MonadIO $ do
                keab <- lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                case keab of
                    Unknown -> return [MkWholeReaderUpdate Unknown]
                    Known (Left a) -> updateA (MkTupleUpdate SelectContext update) (getMRA mr a)
                    Known (Right b) -> updateB (MkTupleUpdate SelectContext update) (getMRB mr b)
        updateAB (MkTupleUpdate SelectContent (MkWholeReaderUpdate keab)) mr =
            withTransConstraintTM @MonadIO $
            case keab of
                Unknown -> return [MkWholeReaderUpdate Unknown]
                Known (Left a) -> updateA (MkTupleUpdate SelectContent $ MkWholeReaderUpdate $ Known a) (getMRA mr a)
                Known (Right b) -> updateB (MkTupleUpdate SelectContent $ MkWholeReaderUpdate $ Known b) (getMRB mr b)
        putEditsAB ::
               forall m. MonadIO m
            => Know c
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know (Either a b))))
            -> t m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know (Either a b)))])
        putEditsAB kc mr =
            withTransConstraintTM @MonadIO $ do
                keab <- lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
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
            -> t m [Either a b]
        frAB mr c =
            withTransConstraintTM @MonadIO $ do
                aa <- frA mr c
                bb <- frB mr c
                return $ fmap Left aa <> fmap Right bb
        updAB ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
            -> t m Bool
        updAB update mr =
            withTransConstraintTM @MonadIO $ do
                u <- updA update mr
                case u of
                    True -> return True
                    False -> updB update mr
        in MkAPinaforeLensMorphism
               (MkAnEditLens (MkAnUpdateFunction getAB updateAB) $ wholePutEdits putEditsAB)
               (MkAPinaforeFunctionMorphism frAB updAB)
    in joinUnliftables call

mapPinaforeLensMorphismBase ::
       forall baseA baseB a b. EditLens baseB baseA -> PinaforeLensMorphism baseA a b -> PinaforeLensMorphism baseB a b
mapPinaforeLensMorphismBase lens morph = let
    call ::
           forall t1 t2. (MonadTransUntrans t1, MonadTransUntrans t2)
        => APinaforeLensMorphism baseA t1 a b
        -> AnEditLens t2 baseB baseA
        -> APinaforeLensMorphism baseB (ComposeT t1 t2) a b
    call (MkAPinaforeLensMorphism fwdA (MkAPinaforeFunctionMorphism frA updateA)) alens = let
        readFunc :: ReadFunctionT t2 (UpdateReader baseB) (UpdateReader baseA)
        readFunc = ufGet $ elFunction alens
        fwdB :: AnEditLens (ComposeT t1 t2) (ContextUpdate baseB (WholeUpdate (Know a))) (WholeUpdate (Know b))
        fwdB = ucCompose fwdA $ liftContentAnEditLens alens
        frB :: forall m. MonadIO m
            => MutableRead m (UpdateReader baseB)
            -> b
            -> ComposeT t1 t2 m [a]
        frB mr b = mkComposeT $ frA (readFunc mr) b
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (UpdateReader baseB)
            -> ComposeT t1 t2 m Bool
        updateB beditB mr =
            withTransConstraintTM @MonadIO $ do
                beditAs <- lift2ComposeT' $ ufUpdate (elFunction alens) beditB mr
                chs <- for beditAs $ \beditA -> mkComposeT $ updateA beditA $ readFunc mr
                return $ or chs
        invB :: APinaforeFunctionMorphism baseB (ComposeT t1 t2) b [a]
        invB = MkAPinaforeFunctionMorphism frB updateB
        plmB :: APinaforeLensMorphism baseB (ComposeT t1 t2) a b
        plmB = MkAPinaforeLensMorphism fwdB invB
        in plmB
    in joinUnlifts call morph lens

bindReadContext ::
       MutableRead m (ContextUpdateReader updateX updateA)
    -> MutableRead m (UpdateReader updateB)
    -> MutableRead m (ContextUpdateReader updateX updateB)
bindReadContext mr _ (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
bindReadContext _ mr (MkTupleUpdateReader SelectContent rt) = mr rt

instance UnliftCategory (APinaforeLensMorphism baseupdate) where
    ucId = let
        ufGet :: ReadFunctionT IdentityT (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know a))
        ufGet mr ReadWhole = lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
        ufUpdate ::
               MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> IdentityT m [WholeUpdate (Know a)]
        ufUpdate (MkTupleUpdate SelectContext _) _ = return []
        ufUpdate (MkTupleUpdate SelectContent update) _ = return [update]
        elFunction ::
               AnUpdateFunction IdentityT (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        elFunction = MkAnUpdateFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit (Know a)]
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> IdentityT m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit SelectContent) edits
        pmForward :: AnEditLens IdentityT (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know a))
        pmForward = MkAnEditLens {..}
        pfFuncRead :: MonadIO m => MutableRead m (UpdateReader baseupdate) -> a -> IdentityT m [a]
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: MonadIO m => baseupdate -> MutableRead m (UpdateReader baseupdate) -> IdentityT m Bool
        pfUpdate _ _ = return False
        pmInverse :: APinaforeFunctionMorphism baseupdate IdentityT a [a]
        pmInverse = MkAPinaforeFunctionMorphism {..}
        in MkAPinaforeLensMorphism {..}
    ucCompose ::
           forall tab tbc a b c. (MonadTransUntrans tab, MonadTransUntrans tbc)
        => APinaforeLensMorphism baseupdate tbc b c
        -> APinaforeLensMorphism baseupdate tab a b
        -> APinaforeLensMorphism baseupdate (ComposeT tbc tab) a c
    ucCompose (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction bcGet bcUpdate) bcPutEdit) (MkAPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction abGet abUpdate) abPutEdit) (MkAPinaforeFunctionMorphism abInvFuncRead abInvUpdate)) = let
        acGet ::
               ReadFunctionT (ComposeT tbc tab) (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know c))
        acGet mra ReadWhole =
            withTransConstraintTM @MonadIO $ do
                mb <- lift2ComposeT'' $ abGet mra ReadWhole
                lift1ComposeT $ bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextUpdate baseupdate (WholeUpdate (Know a))
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> ComposeT tbc tab m [WholeUpdate (Know c)]
        acUpdate (MkTupleUpdate SelectContext pinupdate) mra =
            withTransConstraintTM @MonadIO $ do
                editbs <- lift2ComposeT'' $ abUpdate (MkTupleUpdate SelectContext pinupdate) mra
                editcs1 <-
                    MkComposeT $
                    withTransConstraintTM' @MonadIO $
                    bcUpdate (MkTupleUpdate SelectContext pinupdate) $
                    bindReadContext (remonadMutableRead lift mra) $ abGet mra
                editcss2 <-
                    for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                        lift1ComposeT $
                        bcUpdate (MkTupleUpdate SelectContent updateB) $ bindReadContext mra $ subjectToMutableRead mb
                return $ editcs1 ++ mconcat editcss2
        acUpdate (MkTupleUpdate SelectContent updateA) mra =
            withTransConstraintTM @MonadIO $ do
                editbs <- lift2ComposeT'' $ abUpdate (MkTupleUpdate SelectContent updateA) mra
                editcss <-
                    for editbs $ \updateB@(MkWholeReaderUpdate mb) ->
                        lift1ComposeT $
                        bcUpdate (MkTupleUpdate SelectContent updateB) $ bindReadContext mra $ subjectToMutableRead mb
                return $ mconcat editcss
        acFunc ::
               AnUpdateFunction (ComposeT tbc tab) (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
        acFunc = MkAnUpdateFunction acGet acUpdate
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit (Know c)]
            -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
            -> ComposeT tbc tab m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
        acPutEdit editcs mra =
            withTransConstraintTM @MonadIO $
            getComposeM $ do
                editpbs <-
                    MkComposeM $
                    MkComposeT $
                    withTransConstraintTM' @MonadIO $
                    bcPutEdit editcs $ bindReadContext (remonadMutableRead lift mra) $ abGet mra
                case partitionContextEdits editpbs of
                    (pinedits, editbs) -> do
                        editpas2 <- MkComposeM $ lift2ComposeT'' $ abPutEdit editbs mra
                        return $ (fmap (MkTupleUpdateEdit SelectContext) pinedits) ++ editpas2
        acForward ::
               AnEditLens (ComposeT tbc tab) (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know c))
        acForward = MkAnEditLens acFunc acPutEdit
        acInvFuncRead ::
               forall m. MonadIO m
            => MutableRead m (UpdateReader baseupdate)
            -> c
            -> ComposeT tbc tab m [a]
        acInvFuncRead mr c =
            withTransConstraintTM @MonadIO $ do
                bset <- lift1ComposeT $ bcInvFuncRead mr c
                asetset <- lift2ComposeT'' $ withTransConstraintTM @MonadIO $ for bset $ \b -> abInvFuncRead mr b
                return $ mconcat asetset
        acInvUpdate ::
               forall m. MonadIO m
            => baseupdate
            -> MutableRead m (UpdateReader baseupdate)
            -> ComposeT tbc tab m Bool
        acInvUpdate pedit mr =
            withTransConstraintTM @MonadIO $ do
                chbc <- lift1ComposeT $ bcInvUpdate pedit mr
                chab <- lift2ComposeT'' $ abInvUpdate pedit mr
                return $ chbc || chab
        acInverse :: APinaforeFunctionMorphism baseupdate (ComposeT tbc tab) c [a]
        acInverse = MkAPinaforeFunctionMorphism acInvFuncRead acInvUpdate
        in MkAPinaforeLensMorphism acForward acInverse

funcPinaforeLensMorphism ::
       forall baseupdate a b.
       (Know a -> Know b)
    -> (b -> [a])
    -> (Know b -> Maybe (Know a))
    -> PinaforeLensMorphism baseupdate a b
funcPinaforeLensMorphism ab bsa bma = let
    ufGet :: ReadFunctionT IdentityT (ContextUpdateReader baseupdate (WholeUpdate (Know a))) (WholeReader (Know b))
    ufGet mr ReadWhole = lift $ fmap ab $ mr $ MkTupleUpdateReader SelectContent ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know a))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> IdentityT m [WholeUpdate (Know b)]
    ufUpdate (MkTupleUpdate SelectContext _) _ = return []
    ufUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate a)) _ = return [MkWholeReaderUpdate $ ab a]
    elFunction :: AnUpdateFunction IdentityT (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> IdentityT m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    elPutEdits edits _ =
        return $
        case lastWholeEdit edits of
            Nothing -> Just []
            Just kb -> fmap (pure . MkTupleUpdateEdit SelectContent . MkWholeReaderEdit) (bma kb)
    pmForward :: AnEditLens IdentityT (ContextUpdate baseupdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
    pmForward = MkAnEditLens {..}
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m (UpdateReader baseupdate)
        -> b
        -> IdentityT m [a]
    pfFuncRead _ b = return $ bsa b
    pfUpdate ::
           forall m. MonadIO m
        => baseupdate
        -> MutableRead m (UpdateReader baseupdate)
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    pmInverse :: APinaforeFunctionMorphism baseupdate IdentityT b [a]
    pmInverse = MkAPinaforeFunctionMorphism {..}
    in MkCloseUnlift wUnIdentityT MkAPinaforeLensMorphism {..}

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
applyPinaforeLens (MkCloseUnlift unlift pm) val = (MkCloseUnlift unlift $ pmForward pm) . contextualiseEditLens val

lensFunctionMorphism ::
       forall baseupdate a b.
       PinaforeLensMorphism baseupdate a b
    -> PinaforeFunctionMorphism baseupdate (Know a) (Know b)
lensFunctionMorphism (MkCloseUnlift (unlift :: WUntransFunction t) MkAPinaforeLensMorphism {..}) = let
    funcRead ::
           forall m. MonadIO m
        => MutableRead m (UpdateReader baseupdate)
        -> Know a
        -> t m (Know b)
    funcRead mr a = let
        mr' :: MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mr' (MkTupleUpdateReader SelectContext rt) = mr rt
        mr' (MkTupleUpdateReader SelectContent ReadWhole) = return a
        in ufGet (elFunction pmForward) mr' ReadWhole
    in MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseupdate a b -> PinaforeFunctionMorphism baseupdate b [a]
lensInverseFunctionMorphism (MkCloseUnlift unlift MkAPinaforeLensMorphism {..}) = MkCloseUnlift unlift pmInverse

pmInverseEditLens ::
       forall baseupdate a b. Eq a
    => PinaforeLensMorphism baseupdate a b
    -> EditLens (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
pmInverseEditLens (MkCloseUnlift (unlift :: WUntransFunction t) MkAPinaforeLensMorphism {..}) = let
    getFiniteSet ::
           forall m update. MonadIO m
        => Know b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> t m (FiniteSet a)
    getFiniteSet (Known b) mr =
        withTransConstraintTM @MonadIO $ fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getFiniteSet Unknown _ = withTransConstraintTM @MonadIO $ return mempty
    fsetReadFunction ::
           ReadFunctionT t (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (WholeReader (FiniteSet a))
    fsetReadFunction mr ReadWhole =
        withTransConstraintTM @MonadIO $ do
            kb <- lift $ mr (MkTupleUpdateReader SelectContent ReadWhole)
            getFiniteSet kb mr
    ufGet :: ReadFunctionT t (ContextUpdateReader baseupdate (WholeUpdate (Know b))) (FiniteSetReader a)
    ufGet mr rt = withTransConstraintTM @MonadIO $ wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    ufUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (WholeUpdate (Know b))
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know b)))
        -> t m [FiniteSetUpdate a]
    ufUpdate (MkTupleUpdate SelectContext pinupdate) mr =
        withTransConstraintTM @MonadIO $ do
            ch <- pfUpdate pmInverse pinupdate $ tupleReadFunction SelectContext mr
            if ch
                then do
                    kb <- lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
                    aset <- getFiniteSet kb mr
                    aedits <- getReplaceEditsFromSubject aset
                    return $ fmap editUpdate aedits
                else return []
    ufUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate kb)) mr =
        withTransConstraintTM @MonadIO $ do
            aset <- getFiniteSet kb mr
            aedits <- getReplaceEditsFromSubject aset
            return $ fmap editUpdate aedits
    elFunction :: AnUpdateFunction t (ContextUpdate baseupdate (WholeUpdate (Know b))) (FiniteSetUpdate a)
    elFunction = MkAnUpdateFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        -> t m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkAnEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (UpdateReader baseupdate)
        -> t m (Maybe [UpdateEdit baseupdate])
    putEditAB a kb mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeReaderEdit kb] $ \case
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
        -> t m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    elPutEdit (KeyEditItem _ update) _ = never update
    elPutEdit (KeyEditDelete a) mr =
        withTransConstraintTM @MonadIO $ do
            mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit (KeyEditInsertReplace a) mr =
        withTransConstraintTM @MonadIO $ do
            kb <- lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
            mpedits <- putEditAB a kb $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit KeyEditClear mr =
        withTransConstraintTM @MonadIO $ do
            kb <- lift $ mr $ MkTupleUpdateReader SelectContent ReadWhole
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
        -> t m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know b))])
    elPutEdits [] _ = withTransConstraintTM @MonadIO $ getComposeM $ return []
    elPutEdits (e:ee) mr =
        withTransConstraintTM @MonadIO $
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit e mr
            eea <- MkComposeM $ elPutEdits ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkCloseUnlift unlift $ MkAnEditLens {..}

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
pmInverseEditLensSet newb (MkCloseUnlift (unlift :: WUntransFunction t) MkAPinaforeLensMorphism {..}) = let
    getPointPreimage ::
           forall m update. MonadIO m
        => b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> t m (FiniteSet a)
    getPointPreimage b mr =
        withTransConstraintTM @MonadIO $ fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getSetPreimage ::
           forall m update. MonadIO m
        => FiniteSet b
        -> MutableRead m (ContextUpdateReader baseupdate update)
        -> t m (FiniteSet a)
    getSetPreimage bs mr =
        withTransConstraintTM @MonadIO $ do
            as <- for bs $ \b -> getPointPreimage b mr
            return $ mconcat $ toList as
    getAB ::
           forall m update. MonadIO m
        => MutableRead m (ContextUpdateReader baseupdate update)
        -> a
        -> t m (Know b)
    getAB mr a = let
        mra :: MutableRead m (ContextUpdateReader baseupdate (WholeUpdate (Know a)))
        mra (MkTupleUpdateReader SelectContext rp) = mr $ MkTupleUpdateReader SelectContext rp
        mra (MkTupleUpdateReader SelectContent ReadWhole) = return $ Known a
        in ufGet (elFunction pmForward) mra ReadWhole
    ufGet' :: ReadFunctionT t (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (FiniteSetReader a)
    ufGet' mr KeyReadKeys =
        withTransConstraintTM @MonadIO $ do
            bs <- lift $ mr $ MkTupleUpdateReader SelectContent KeyReadKeys
            getSetPreimage bs mr
    ufGet' (mr :: MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))) (KeyReadItem a ReadWhole) =
        withTransConstraintTM @MonadIO $ do
            kb <- getAB mr a
            case kb of
                Known b -> do
                    mb <- lift $ mr $ MkTupleUpdateReader SelectContent $ KeyReadItem b ReadWhole
                    case mb of
                        Just _ -> return $ Just a
                        Nothing -> return Nothing
                Unknown -> return Nothing
    ufUpdate' ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (FiniteSetUpdate b)
        -> MutableRead m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> t m [FiniteSetUpdate a]
    ufUpdate' (MkTupleUpdate SelectContext pinaedit) mr =
        withTransConstraintTM @MonadIO $ do
            ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
            if ch
                then do
                    bs <- lift $ mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                    aset <- getSetPreimage bs mr
                    aedits <- getReplaceEditsFromSubject aset
                    return $ fmap editUpdate aedits
                else return []
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateItem _ update)) _ = never update
    ufUpdate' (MkTupleUpdate SelectContent KeyUpdateClear) _ = lift $ return [KeyUpdateClear]
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateInsertReplace _)) _ = lift $ return []
    ufUpdate' (MkTupleUpdate SelectContent (KeyUpdateDelete b)) mr =
        withTransConstraintTM @MonadIO $ do
            aset <- getPointPreimage b mr
            return $ fmap KeyUpdateDelete $ toList aset
    elFunction' :: AnUpdateFunction t (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
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
        -> t m (Maybe [ContextUpdateEdit baseupdate (WholeUpdate (Know a))])
    MkAnEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (UpdateReader baseupdate)
        -> t m (Maybe [UpdateEdit baseupdate])
    putEditAB a b mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeReaderEdit b] $ \case
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
        -> t m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdit' (KeyEditItem _ update) _ = never update
    elPutEdit' (KeyEditDelete a) mr =
        withTransConstraintTM @MonadIO $ do
            mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit' (KeyEditInsertReplace a) mr =
        withTransConstraintTM @MonadIO $ do
            b <- liftIO newb
            getComposeM $ do
                pedits <- MkComposeM $ putEditAB a (Known b) $ tupleReadFunction SelectContext mr
                return $
                    (MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace b) :
                    fmap (MkTupleUpdateEdit SelectContext) pedits
    elPutEdit' KeyEditClear mr =
        withTransConstraintTM @MonadIO $ do
            bs <- lift $ mr $ MkTupleUpdateReader SelectContent KeyReadKeys
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
        -> t m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdits' [] _ = withTransConstraintTM @MonadIO $ getComposeM $ return []
    elPutEdits' (e:ee) mr =
        withTransConstraintTM @MonadIO $
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit' e mr
            eea <- MkComposeM $ elPutEdits' ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkCloseUnlift unlift $ MkAnEditLens elFunction' elPutEdits'

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (FiniteSetUpdate b)
    -> PinaforeLensValue baseupdate (FiniteSetUpdate a)
applyInversePinaforeLensSet newb pm val = pmInverseEditLensSet newb pm . contextualiseEditLens val
