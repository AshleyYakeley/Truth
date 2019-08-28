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

type PinaforeLensValue baseedit = EditLens baseedit

data APinaforeFunctionMorphism baseedit t a b = MkAPinaforeFunctionMorphism
    { pfFuncRead :: forall m. MonadIO m => MutableRead m (EditReader baseedit) -> a -> t m b
    , pfUpdate :: forall m. MonadIO m => baseedit -> MutableRead m (EditReader baseedit) -> t m Bool
    }

instance MonadTransConstraint MonadIO t => Functor (APinaforeFunctionMorphism baseedit t a) where
    fmap ab (MkAPinaforeFunctionMorphism fr up) =
        MkAPinaforeFunctionMorphism (\mr a -> withTransConstraintTM @MonadIO $ fmap ab $ fr mr a) up

type PinaforeFunctionMorphism baseedit = CloseUnlift (APinaforeFunctionMorphism baseedit)

instance Functor (PinaforeFunctionMorphism baseedit a) where
    fmap f fm = arr f . fm

instance Applicative (PinaforeFunctionMorphism baseedit a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance Unliftable (APinaforeFunctionMorphism baseedit) where
    fmapUnliftable mapm (MkAPinaforeFunctionMorphism fr u) =
        MkAPinaforeFunctionMorphism (\mr a -> mapm $ fr mr a) (\e mr -> mapm $ u e mr)

instance UnliftCategory (APinaforeFunctionMorphism baseedit) where
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
                  \edit mr ->
                      withTransConstraintTM @MonadIO $ do
                          chab <- lift2ComposeT'' $ uab edit mr
                          chbc <- lift1ComposeT $ ubc edit mr
                          return $ chab || chbc
            }

instance Arrow (PinaforeFunctionMorphism baseedit) where
    arr ab =
        MkCloseUnlift
            identityUnlift
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

instance ArrowChoice (PinaforeFunctionMorphism baseedit) where
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

instance Traversable f => CatFunctor (PinaforeFunctionMorphism baseedit) (PinaforeFunctionMorphism baseedit) f where
    cfmap (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism f u)) =
        MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism (\mr fa -> withTransConstraintTM @MonadIO $ for fa $ f mr) u

type PinaforeFunctionValue baseedit t = UpdateFunction baseedit (WholeEdit t)

applyPinaforeFunction ::
       forall baseedit a b.
       PinaforeFunctionMorphism baseedit a b
    -> PinaforeFunctionValue baseedit a
    -> PinaforeFunctionValue baseedit b
applyPinaforeFunction =
    joinUnliftables $ \MkAPinaforeFunctionMorphism {..} (MkAnUpdateFunction {..} :: AnUpdateFunction t baseedit (WholeEdit a)) -> let
        getB ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> t m b
        getB mr =
            withTransConstraintTM @MonadIO $ do
                a <- ufGet mr ReadWhole
                pfFuncRead mr a
        g :: ReadFunctionT t (EditReader baseedit) (WholeReader b)
        g mr ReadWhole = getB mr
        u :: forall m. MonadIO m
          => baseedit
          -> MutableRead m (EditReader baseedit)
          -> t m [WholeEdit b]
        u pinedit mr =
            withTransConstraintTM @MonadIO $ do
                ch <- pfUpdate pinedit mr
                if ch
                    then do
                        b <- getB mr
                        return [MkWholeEdit b]
                    else do
                        edits <- ufUpdate pinedit mr
                        for edits $ \(MkWholeEdit a) -> fmap MkWholeEdit $ pfFuncRead mr a
        in MkAnUpdateFunction g u

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       UpdateFunction baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase ef morph = let
    call ::
           forall t1 t2. (MonadTransUnlift t1, MonadTransUnlift t2)
        => APinaforeFunctionMorphism baseA t1 a b
        -> AnUpdateFunction t2 baseB baseA
        -> APinaforeFunctionMorphism baseB (ComposeT t1 t2) a b
    call (MkAPinaforeFunctionMorphism frA updateA) aef = let
        readFunc :: ReadFunctionT t2 (EditReader baseB) (EditReader baseA)
        readFunc = ufGet aef
        frB :: forall m. MonadIO m
            => MutableRead m (EditReader baseB)
            -> a
            -> ComposeT t1 t2 m b
        frB mr a = mkComposeT $ frA (readFunc mr) a
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (EditReader baseB)
            -> ComposeT t1 t2 m Bool
        updateB beditB mr =
            withTransConstraintTM @MonadIO $ do
                beditAs <- lift2ComposeT' $ ufUpdate aef beditB mr
                chs <- for beditAs $ \beditA -> mkComposeT $ updateA beditA $ readFunc mr
                return $ or chs
        in MkAPinaforeFunctionMorphism frB updateB
    in joinUnlifts call morph ef

lensFunctionValue ::
       (FullSubjectReader (EditReader edit), ApplicableEdit edit)
    => PinaforeLensValue baseedit edit
    -> PinaforeFunctionValue baseedit (EditSubject edit)
lensFunctionValue lens = convertUpdateFunction . editLensFunction lens

data APinaforeLensMorphism baseedit t a b = MkAPinaforeLensMorphism
    { pmForward :: AnEditLens t (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know b))
    , pmInverse :: APinaforeFunctionMorphism baseedit t b [a]
    }

instance Unliftable (APinaforeLensMorphism baseedit) where
    fmapUnliftable mapTM (MkAPinaforeLensMorphism fwd inv) =
        MkAPinaforeLensMorphism (fmapUnliftable mapTM fwd) (fmapUnliftable mapTM inv)

type PinaforeLensMorphism baseedit = CloseUnlift (APinaforeLensMorphism baseedit)

mkComposeT ::
       forall t1 t2 m a. (MonadTransConstraint MonadIO t2, MonadIO m)
    => (MonadIO (t2 m) => t1 (t2 m) a)
    -> ComposeT t1 t2 m a
mkComposeT =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> MkComposeT

pairPinaforeLensMorphism ::
       forall baseedit a b c.
       PinaforeLensMorphism baseedit a b
    -> PinaforeLensMorphism baseedit a c
    -> PinaforeLensMorphism baseedit a (b, c)
pairPinaforeLensMorphism = let
    call ::
           forall t1 t2. (MonadTransUnlift t1, MonadTransUnlift t2)
        => APinaforeLensMorphism baseedit t1 a b
        -> APinaforeLensMorphism baseedit t2 a c
        -> APinaforeLensMorphism baseedit (ComposeT t1 t2) a (b, c)
    call (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getB updateB) putEditsB) (MkAPinaforeFunctionMorphism frB updB)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getC updateC) putEditsC) (MkAPinaforeFunctionMorphism frC updC)) = let
        getBC ::
               ReadFunctionT (ComposeT t1 t2) (ContextEditReader baseedit (WholeEdit (Know a))) (WholeReader (Know ( b
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
            => ContextEdit baseedit (WholeEdit (Know a))
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
            -> ComposeT t1 t2 m [WholeEdit (Know (b, c))]
        updateBC edit mr =
            withTransConstraintTM @MonadIO $ do
                ebs <- lift1ComposeT $ updateB edit mr
                ecs <- lift2ComposeT' $ updateC edit mr
                case (lastM ebs, lastM ecs) of
                    (Nothing, Nothing) -> return []
                    (Just (MkWholeEdit Unknown), _) -> return [MkWholeEdit Unknown]
                    (_, Just (MkWholeEdit Unknown)) -> return [MkWholeEdit Unknown]
                    (Just (MkWholeEdit (Known b)), Just (MkWholeEdit (Known c))) -> return [MkWholeEdit $ Known (b, c)]
                    (Just (MkWholeEdit (Known b)), Nothing) -> do
                        kc <- lift2ComposeT' $ getC mr ReadWhole
                        return [MkWholeEdit $ fmap (\c -> (b, c)) kc]
                    (Nothing, Just (MkWholeEdit (Known c))) -> do
                        kb <- lift1ComposeT $ getB mr ReadWhole
                        return [MkWholeEdit $ fmap (\b -> (b, c)) kb]
        putEditsBC ::
               forall m. MonadIO m
            => (Know (b, c))
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
            -> ComposeT t1 t2 m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
        putEditsBC kbc mr =
            withTransConstraintTM @MonadIO $
            case kbc of
                Unknown -> return Nothing -- can't delete
                (Known (b, c)) ->
                    getComposeM $ do
                        aa1 <- MkComposeM $ lift1ComposeT $ putEditsB [MkWholeEdit $ Known b] mr
                        aa2 <- MkComposeM $ lift2ComposeT' $ putEditsC [MkWholeEdit $ Known c] mr
                        return $ aa1 ++ aa2
        frBC ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> (b, c)
            -> ComposeT t1 t2 m [a]
        frBC mr (b, c) =
            withTransConstraintTM @MonadIO $ do
                aa1 <- lift1ComposeT $ frB mr b
                aa2 <- lift2ComposeT' $ frC mr c
                return $ aa1 ++ aa2
        updBC ::
               forall m. MonadIO m
            => baseedit
            -> MutableRead m (EditReader baseedit)
            -> ComposeT t1 t2 m Bool
        updBC edit mr =
            withTransConstraintTM @MonadIO $ do
                eb <- lift1ComposeT $ updB edit mr
                case eb of
                    True -> return True
                    False -> lift2ComposeT' $ updC edit mr
        in MkAPinaforeLensMorphism
               (MkAnEditLens (MkAnUpdateFunction getBC updateBC) $ wholePutEdits putEditsBC)
               (MkAPinaforeFunctionMorphism frBC updBC)
    in joinUnlifts call

eitherPinaforeLensMorphism ::
       forall baseedit a b c.
       PinaforeLensMorphism baseedit a c
    -> PinaforeLensMorphism baseedit b c
    -> PinaforeLensMorphism baseedit (Either a b) c
eitherPinaforeLensMorphism = let
    call ::
           forall t. MonadTransUnlift t
        => APinaforeLensMorphism baseedit t a c
        -> APinaforeLensMorphism baseedit t b c
        -> APinaforeLensMorphism baseedit t (Either a b) c
    call (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getA updateA) putEditsA) (MkAPinaforeFunctionMorphism frA updA)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction getB updateB) putEditsB) (MkAPinaforeFunctionMorphism frB updB)) = let
        getMRA ::
               forall m. MonadIO m
            => MutableRead m (ContextEditReader baseedit (WholeEdit (Know (Either a b))))
            -> a
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
        getMRA mr _ (MkTupleEditReader SelectContext r) = mr $ MkTupleEditReader SelectContext r
        getMRA _ a (MkTupleEditReader SelectContent ReadWhole) = return $ Known a
        getMRB ::
               forall m. MonadIO m
            => MutableRead m (ContextEditReader baseedit (WholeEdit (Know (Either a b))))
            -> b
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know b)))
        getMRB mr _ (MkTupleEditReader SelectContext r) = mr $ MkTupleEditReader SelectContext r
        getMRB _ b (MkTupleEditReader SelectContent ReadWhole) = return $ Known b
        getAB :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit (Know (Either a b)))) (WholeReader (Know c))
        getAB (mr :: MutableRead m _) ReadWhole =
            withTransConstraintTM @MonadIO $ do
                keab <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                case keab of
                    Unknown -> return Unknown
                    Known (Left a) -> getA (getMRA mr a) ReadWhole
                    Known (Right b) -> getB (getMRB mr b) ReadWhole
        updateAB ::
               forall m. MonadIO m
            => ContextEdit baseedit (WholeEdit (Know (Either a b)))
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know (Either a b))))
            -> t m [WholeEdit (Know c)]
        updateAB (MkTupleEdit SelectContext edit) mr =
            withTransConstraintTM @MonadIO $ do
                keab <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                case keab of
                    Unknown -> return [MkWholeEdit Unknown]
                    Known (Left a) -> updateA (MkTupleEdit SelectContext edit) (getMRA mr a)
                    Known (Right b) -> updateB (MkTupleEdit SelectContext edit) (getMRB mr b)
        updateAB (MkTupleEdit SelectContent (MkWholeEdit keab)) mr =
            withTransConstraintTM @MonadIO $
            case keab of
                Unknown -> return [MkWholeEdit Unknown]
                Known (Left a) -> updateA (MkTupleEdit SelectContent $ MkWholeEdit $ Known a) (getMRA mr a)
                Known (Right b) -> updateB (MkTupleEdit SelectContent $ MkWholeEdit $ Known b) (getMRB mr b)
        putEditsAB ::
               forall m. MonadIO m
            => Know c
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know (Either a b))))
            -> t m (Maybe [ContextEdit baseedit (WholeEdit (Know (Either a b)))])
        putEditsAB kc mr =
            withTransConstraintTM @MonadIO $ do
                keab <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                case keab of
                    Unknown ->
                        case kc of
                            Unknown -> return $ Just []
                            Known _ -> return Nothing
                    Known (Left a) -> do
                        mea <- putEditsA [MkWholeEdit kc] (getMRA mr a)
                        return $ (fmap $ fmap $ mapContextEdit $ mapWholeEdit $ fmap Left) mea
                    Known (Right b) -> do
                        meb <- putEditsB [MkWholeEdit kc] (getMRB mr b)
                        return $ (fmap $ fmap $ mapContextEdit $ mapWholeEdit $ fmap Right) meb
        frAB ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> c
            -> t m [Either a b]
        frAB mr c =
            withTransConstraintTM @MonadIO $ do
                aa <- frA mr c
                bb <- frB mr c
                return $ fmap Left aa <> fmap Right bb
        updAB ::
               forall m. MonadIO m
            => baseedit
            -> MutableRead m (EditReader baseedit)
            -> t m Bool
        updAB edit mr =
            withTransConstraintTM @MonadIO $ do
                u <- updA edit mr
                case u of
                    True -> return True
                    False -> updB edit mr
        in MkAPinaforeLensMorphism
               (MkAnEditLens (MkAnUpdateFunction getAB updateAB) $ wholePutEdits putEditsAB)
               (MkAPinaforeFunctionMorphism frAB updAB)
    in joinUnliftables call

mapPinaforeLensMorphismBase ::
       forall baseA baseB a b. EditLens baseB baseA -> PinaforeLensMorphism baseA a b -> PinaforeLensMorphism baseB a b
mapPinaforeLensMorphismBase lens morph = let
    call ::
           forall t1 t2. (MonadTransUnlift t1, MonadTransUnlift t2)
        => APinaforeLensMorphism baseA t1 a b
        -> AnEditLens t2 baseB baseA
        -> APinaforeLensMorphism baseB (ComposeT t1 t2) a b
    call (MkAPinaforeLensMorphism fwdA (MkAPinaforeFunctionMorphism frA updateA)) alens = let
        readFunc :: ReadFunctionT t2 (EditReader baseB) (EditReader baseA)
        readFunc = ufGet $ elFunction alens
        fwdB :: AnEditLens (ComposeT t1 t2) (ContextEdit baseB (WholeEdit (Know a))) (WholeEdit (Know b))
        fwdB = ucCompose fwdA $ liftContentAnEditLens alens
        frB :: forall m. MonadIO m
            => MutableRead m (EditReader baseB)
            -> b
            -> ComposeT t1 t2 m [a]
        frB mr b = mkComposeT $ frA (readFunc mr) b
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (EditReader baseB)
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
       MutableRead m (ContextEditReader editx edita)
    -> MutableRead m (EditReader editb)
    -> MutableRead m (ContextEditReader editx editb)
bindReadContext mr _ (MkTupleEditReader SelectContext rt) = mr $ MkTupleEditReader SelectContext rt
bindReadContext _ mr (MkTupleEditReader SelectContent rt) = mr rt

instance UnliftCategory (APinaforeLensMorphism baseedit) where
    ucId = let
        ufGet :: ReadFunctionT IdentityT (ContextEditReader baseedit (WholeEdit (Know a))) (WholeReader (Know a))
        ufGet mr ReadWhole = lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        ufUpdate ::
               MonadIO m
            => ContextEdit baseedit (WholeEdit (Know a))
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
            -> IdentityT m [WholeEdit (Know a)]
        ufUpdate (MkTupleEdit SelectContext _) _ = return []
        ufUpdate (MkTupleEdit SelectContent edit) _ = return [edit]
        elFunction :: AnUpdateFunction IdentityT (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know a))
        elFunction = MkAnUpdateFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit (Know a)]
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
            -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleEdit SelectContent) edits
        pmForward :: AnEditLens IdentityT (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know a))
        pmForward = MkAnEditLens {..}
        pfFuncRead :: MonadIO m => MutableRead m (EditReader baseedit) -> a -> IdentityT m [a]
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: MonadIO m => baseedit -> MutableRead m (EditReader baseedit) -> IdentityT m Bool
        pfUpdate _ _ = return False
        pmInverse :: APinaforeFunctionMorphism baseedit IdentityT a [a]
        pmInverse = MkAPinaforeFunctionMorphism {..}
        in MkAPinaforeLensMorphism {..}
    ucCompose ::
           forall tab tbc a b c. (MonadTransUnlift tab, MonadTransUnlift tbc)
        => APinaforeLensMorphism baseedit tbc b c
        -> APinaforeLensMorphism baseedit tab a b
        -> APinaforeLensMorphism baseedit (ComposeT tbc tab) a c
    ucCompose (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction bcGet bcUpdate) bcPutEdit) (MkAPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnUpdateFunction abGet abUpdate) abPutEdit) (MkAPinaforeFunctionMorphism abInvFuncRead abInvUpdate)) = let
        acGet ::
               ReadFunctionT (ComposeT tbc tab) (ContextEditReader baseedit (WholeEdit (Know a))) (WholeReader (Know c))
        acGet mra ReadWhole =
            withTransConstraintTM @MonadIO $ do
                mb <- lift2ComposeT'' $ abGet mra ReadWhole
                lift1ComposeT $ bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextEdit baseedit (WholeEdit (Know a))
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
            -> ComposeT tbc tab m [WholeEdit (Know c)]
        acUpdate (MkTupleEdit SelectContext pinedit) mra =
            withTransConstraintTM @MonadIO $ do
                editbs <- lift2ComposeT'' $ abUpdate (MkTupleEdit SelectContext pinedit) mra
                editcs1 <-
                    MkComposeT $
                    withTransConstraintTM' @MonadIO $
                    bcUpdate (MkTupleEdit SelectContext pinedit) $
                    bindReadContext (remonadMutableRead lift mra) $ abGet mra
                editcss2 <-
                    for editbs $ \editb@(MkWholeEdit mb) ->
                        lift1ComposeT $
                        bcUpdate (MkTupleEdit SelectContent editb) $ bindReadContext mra $ subjectToMutableRead mb
                return $ editcs1 ++ mconcat editcss2
        acUpdate (MkTupleEdit SelectContent edita) mra =
            withTransConstraintTM @MonadIO $ do
                editbs <- lift2ComposeT'' $ abUpdate (MkTupleEdit SelectContent edita) mra
                editcss <-
                    for editbs $ \editb@(MkWholeEdit mb) ->
                        lift1ComposeT $
                        bcUpdate (MkTupleEdit SelectContent editb) $ bindReadContext mra $ subjectToMutableRead mb
                return $ mconcat editcss
        acFunc :: AnUpdateFunction (ComposeT tbc tab) (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know c))
        acFunc = MkAnUpdateFunction acGet acUpdate
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit (Know c)]
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
            -> ComposeT tbc tab m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
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
                        return $ (fmap (MkTupleEdit SelectContext) pinedits) ++ editpas2
        acForward :: AnEditLens (ComposeT tbc tab) (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know c))
        acForward = MkAnEditLens acFunc acPutEdit
        acInvFuncRead ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> c
            -> ComposeT tbc tab m [a]
        acInvFuncRead mr c =
            withTransConstraintTM @MonadIO $ do
                bset <- lift1ComposeT $ bcInvFuncRead mr c
                asetset <- lift2ComposeT'' $ withTransConstraintTM @MonadIO $ for bset $ \b -> abInvFuncRead mr b
                return $ mconcat asetset
        acInvUpdate ::
               forall m. MonadIO m
            => baseedit
            -> MutableRead m (EditReader baseedit)
            -> ComposeT tbc tab m Bool
        acInvUpdate pedit mr =
            withTransConstraintTM @MonadIO $ do
                chbc <- lift1ComposeT $ bcInvUpdate pedit mr
                chab <- lift2ComposeT'' $ abInvUpdate pedit mr
                return $ chbc || chab
        acInverse :: APinaforeFunctionMorphism baseedit (ComposeT tbc tab) c [a]
        acInverse = MkAPinaforeFunctionMorphism acInvFuncRead acInvUpdate
        in MkAPinaforeLensMorphism acForward acInverse

funcPinaforeLensMorphism ::
       forall baseedit a b.
       (Know a -> Know b)
    -> (b -> [a])
    -> (Know b -> Maybe (Know a))
    -> PinaforeLensMorphism baseedit a b
funcPinaforeLensMorphism ab bsa bma = let
    ufGet :: ReadFunctionT IdentityT (ContextEditReader baseedit (WholeEdit (Know a))) (WholeReader (Know b))
    ufGet mr ReadWhole = lift $ fmap ab $ mr $ MkTupleEditReader SelectContent ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => ContextEdit baseedit (WholeEdit (Know a))
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
        -> IdentityT m [WholeEdit (Know b)]
    ufUpdate (MkTupleEdit SelectContext _) _ = return []
    ufUpdate (MkTupleEdit SelectContent (MkWholeEdit a)) _ = return [MkWholeEdit $ ab a]
    elFunction :: AnUpdateFunction IdentityT (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know b))
    elFunction = MkAnUpdateFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know b)
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
        -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
    elPutEdit (MkWholeEdit kb) _ = return $ fmap (pure . MkTupleEdit SelectContent . MkWholeEdit) (bma kb)
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know a))))
        -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    pmForward :: AnEditLens IdentityT (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know b))
    pmForward = MkAnEditLens {..}
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m (EditReader baseedit)
        -> b
        -> IdentityT m [a]
    pfFuncRead _ b = return $ bsa b
    pfUpdate ::
           forall m. MonadIO m
        => baseedit
        -> MutableRead m (EditReader baseedit)
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    pmInverse :: APinaforeFunctionMorphism baseedit IdentityT b [a]
    pmInverse = MkAPinaforeFunctionMorphism {..}
    in MkCloseUnlift identityUnlift MkAPinaforeLensMorphism {..}

nullPinaforeLensMorphism :: forall baseedit a b. PinaforeLensMorphism baseedit a b
nullPinaforeLensMorphism = funcPinaforeLensMorphism (\_ -> Unknown) (\_ -> mempty) (\_ -> Nothing)

bijectionPinaforeLensMorphism :: Bijection a b -> PinaforeLensMorphism baseedit a b
bijectionPinaforeLensMorphism (MkIsomorphism ab ba) =
    funcPinaforeLensMorphism (fmap ab) (\b -> opoint $ ba b) (\kb -> Just $ fmap ba kb)

instance IsoVariant (PinaforeLensMorphism baseedit t) where
    isoMap ab ba m = bijectionPinaforeLensMorphism (MkIsomorphism ab ba) . m

instance IsoVariant' (PinaforeLensMorphism baseedit) where
    isoMap' ab ba m = m . bijectionPinaforeLensMorphism (MkIsomorphism ba ab)

applyPinaforeLens ::
       PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (WholeEdit (Know a))
    -> PinaforeLensValue baseedit (WholeEdit (Know b))
applyPinaforeLens (MkCloseUnlift unlift pm) val = (MkCloseUnlift unlift $ pmForward pm) . contextualiseEditLens val

lensFunctionMorphism ::
       forall baseedit a b. PinaforeLensMorphism baseedit a b -> PinaforeFunctionMorphism baseedit (Know a) (Know b)
lensFunctionMorphism (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    funcRead ::
           forall m. MonadIO m
        => MutableRead m (EditReader baseedit)
        -> Know a
        -> t m (Know b)
    funcRead mr a = let
        mr' :: MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
        mr' (MkTupleEditReader SelectContext rt) = mr rt
        mr' (MkTupleEditReader SelectContent ReadWhole) = return a
        in ufGet (elFunction pmForward) mr' ReadWhole
    in MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseedit a b -> PinaforeFunctionMorphism baseedit b [a]
lensInverseFunctionMorphism (MkCloseUnlift unlift MkAPinaforeLensMorphism {..}) = MkCloseUnlift unlift pmInverse

pmInverseEditLens ::
       forall baseedit a b. Eq a
    => PinaforeLensMorphism baseedit a b
    -> EditLens (ContextEdit baseedit (WholeEdit (Know b))) (FiniteSetEdit a)
pmInverseEditLens (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    getFiniteSet ::
           forall m edit. MonadIO m
        => Know b
        -> MutableRead m (ContextEditReader baseedit edit)
        -> t m (FiniteSet a)
    getFiniteSet (Known b) mr =
        withTransConstraintTM @MonadIO $ fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getFiniteSet Unknown _ = withTransConstraintTM @MonadIO $ return mempty
    fsetReadFunction :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit (Know b))) (WholeReader (FiniteSet a))
    fsetReadFunction mr ReadWhole =
        withTransConstraintTM @MonadIO $ do
            kb <- lift $ mr (MkTupleEditReader SelectContent ReadWhole)
            getFiniteSet kb mr
    ufGet :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit (Know b))) (FiniteSetReader a)
    ufGet mr rt = withTransConstraintTM @MonadIO $ wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    ufUpdate ::
           forall m. MonadIO m
        => ContextEdit baseedit (WholeEdit (Know b))
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know b))))
        -> t m [FiniteSetEdit a]
    ufUpdate (MkTupleEdit SelectContext pinaedit) mr =
        withTransConstraintTM @MonadIO $ do
            ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
            if ch
                then do
                    kb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                    aset <- getFiniteSet kb mr
                    aedits <- getReplaceEditsFromSubject aset
                    return aedits
                else return []
    ufUpdate (MkTupleEdit SelectContent (MkWholeEdit kb)) mr =
        withTransConstraintTM @MonadIO $ do
            aset <- getFiniteSet kb mr
            aedits <- getReplaceEditsFromSubject aset
            return aedits
    elFunction :: AnUpdateFunction t (ContextEdit baseedit (WholeEdit (Know b))) (FiniteSetEdit a)
    elFunction = MkAnUpdateFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
    MkAnEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (EditReader baseedit)
        -> t m (Maybe [baseedit])
    putEditAB a kb mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeEdit kb] $ \case
                    MkTupleEditReader SelectContext rt -> mr rt
                    MkTupleEditReader SelectContent ReadWhole -> return $ Known a
            return $
                fmap
                    (\edits ->
                         mapMaybe
                             (\case
                                  MkTupleEdit SelectContext edit -> Just edit
                                  MkTupleEdit SelectContent _ -> Nothing)
                             edits)
                    medits
    elPutEdit ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Know b))))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Know b))])
    elPutEdit (KeyEditItem _ edit) _ = never edit
    elPutEdit (KeyDeleteItem a) mr =
        withTransConstraintTM @MonadIO $ do
            mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
    elPutEdit (KeyInsertReplaceItem a) mr =
        withTransConstraintTM @MonadIO $ do
            kb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            mpedits <- putEditAB a kb $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
    elPutEdit KeyClear mr =
        withTransConstraintTM @MonadIO $ do
            kb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            aa <- getFiniteSet kb mr
            lmpedits <- for (toList aa) $ \a -> putEditAB a Unknown $ tupleReadFunction SelectContext mr
            return $ fmap (\lpedits -> fmap (MkTupleEdit SelectContext) $ mconcat lpedits) $ sequenceA lmpedits
    applyEdit' ::
           ContextEdit baseedit (WholeEdit (Know b))
        -> ReadFunction (ContextEditReader baseedit (WholeEdit (Know b))) (ContextEditReader baseedit (WholeEdit (Know b)))
    -- removed line to avoid (ApplicableEdit baseedit) constraint, possibly kinda hacky.
    -- applyEdit' (MkTupleEdit SelectContext edit) mr (MkTupleEditReader SelectContext rt) = applyEdit edit (mr . MkTupleEditReader SelectContext) rt
    applyEdit' (MkTupleEdit SelectContent edit) mr (MkTupleEditReader SelectContent rt) =
        applyEdit edit (mr . MkTupleEditReader SelectContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextEdit baseedit (WholeEdit (Know b))]
        -> ReadFunction (ContextEditReader baseedit (WholeEdit (Know b))) (ContextEditReader baseedit (WholeEdit (Know b)))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know b)))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Know b))])
    elPutEdits [] _ = withTransConstraintTM @MonadIO $ getComposeM $ return []
    elPutEdits (e:ee) mr =
        withTransConstraintTM @MonadIO $
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit e mr
            eea <- MkComposeM $ elPutEdits ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkCloseUnlift unlift $ MkAnEditLens {..}

applyInversePinaforeLens ::
       forall baseedit a b. (Eq a, Eq b)
    => PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (WholeEdit (Know b))
    -> PinaforeLensValue baseedit (FiniteSetEdit a)
applyInversePinaforeLens pm val = pmInverseEditLens pm . contextualiseEditLens val

pmInverseEditLensSet ::
       forall baseedit a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseedit a b
    -> EditLens (ContextEdit baseedit (FiniteSetEdit b)) (FiniteSetEdit a)
pmInverseEditLensSet newb (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    getPointPreimage ::
           forall m edit. MonadIO m
        => b
        -> MutableRead m (ContextEditReader baseedit edit)
        -> t m (FiniteSet a)
    getPointPreimage b mr =
        withTransConstraintTM @MonadIO $ fmap setFromList $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    getSetPreimage ::
           forall m edit. MonadIO m
        => FiniteSet b
        -> MutableRead m (ContextEditReader baseedit edit)
        -> t m (FiniteSet a)
    getSetPreimage bs mr =
        withTransConstraintTM @MonadIO $ do
            as <- for bs $ \b -> getPointPreimage b mr
            return $ mconcat $ toList as
    getAB ::
           forall m edit. MonadIO m
        => MutableRead m (ContextEditReader baseedit edit)
        -> a
        -> t m (Know b)
    getAB mr a = let
        mra :: MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
        mra (MkTupleEditReader SelectContext rp) = mr $ MkTupleEditReader SelectContext rp
        mra (MkTupleEditReader SelectContent ReadWhole) = return $ Known a
        in ufGet (elFunction pmForward) mra ReadWhole
    ufGet' :: ReadFunctionT t (ContextEditReader baseedit (FiniteSetEdit b)) (FiniteSetReader a)
    ufGet' mr KeyReadKeys =
        withTransConstraintTM @MonadIO $ do
            bs <- lift $ mr $ MkTupleEditReader SelectContent KeyReadKeys
            getSetPreimage bs mr
    ufGet' (mr :: MutableRead m (ContextEditReader baseedit (FiniteSetEdit b))) (KeyReadItem a ReadWhole) =
        withTransConstraintTM @MonadIO $ do
            kb <- getAB mr a
            case kb of
                Known b -> do
                    mb <- lift $ mr $ MkTupleEditReader SelectContent $ KeyReadItem b ReadWhole
                    case mb of
                        Just _ -> return $ Just a
                        Nothing -> return Nothing
                Unknown -> return Nothing
    ufUpdate' ::
           forall m. MonadIO m
        => ContextEdit baseedit (FiniteSetEdit b)
        -> MutableRead m (ContextEditReader baseedit (FiniteSetEdit b))
        -> t m [FiniteSetEdit a]
    ufUpdate' (MkTupleEdit SelectContext pinaedit) mr =
        withTransConstraintTM @MonadIO $ do
            ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
            if ch
                then do
                    bs <- lift $ mr $ MkTupleEditReader SelectContent KeyReadKeys
                    aset <- getSetPreimage bs mr
                    aedits <- getReplaceEditsFromSubject aset
                    return aedits
                else return []
    ufUpdate' (MkTupleEdit SelectContent (KeyEditItem _ edit)) _ = never edit
    ufUpdate' (MkTupleEdit SelectContent KeyClear) _ = lift $ return [KeyClear]
    ufUpdate' (MkTupleEdit SelectContent (KeyInsertReplaceItem _)) _ = lift $ return []
    ufUpdate' (MkTupleEdit SelectContent (KeyDeleteItem b)) mr =
        withTransConstraintTM @MonadIO $ do
            aset <- getPointPreimage b mr
            return $ fmap KeyDeleteItem $ toList aset
    elFunction' :: AnUpdateFunction t (ContextEdit baseedit (FiniteSetEdit b)) (FiniteSetEdit a)
    elFunction' = MkAnUpdateFunction ufGet' ufUpdate'
    applyEdit' ::
           ContextEdit baseedit (FiniteSetEdit b)
        -> ReadFunction (ContextEditReader baseedit (FiniteSetEdit b)) (ContextEditReader baseedit (FiniteSetEdit b))
    applyEdit' (MkTupleEdit SelectContent edit) mr (MkTupleEditReader SelectContent rt) =
        applyEdit edit (mr . MkTupleEditReader SelectContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextEdit baseedit (FiniteSetEdit b)]
        -> ReadFunction (ContextEditReader baseedit (FiniteSetEdit b)) (ContextEditReader baseedit (FiniteSetEdit b))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
    MkAnEditLens _ putEditBA = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Know b
        -> MutableRead m (EditReader baseedit)
        -> t m (Maybe [baseedit])
    putEditAB a b mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeEdit b] $ \case
                    MkTupleEditReader SelectContext rt -> mr rt
                    MkTupleEditReader SelectContent ReadWhole -> return $ Known a
            return $
                fmap
                    (\edits ->
                         mapMaybe
                             (\case
                                  MkTupleEdit SelectContext edit -> Just edit
                                  MkTupleEdit SelectContent _ -> Nothing)
                             edits)
                    medits
    elPutEdit' ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> MutableRead m (ContextEditReader baseedit (FiniteSetEdit b))
        -> t m (Maybe [ContextEdit baseedit (FiniteSetEdit b)])
    elPutEdit' (KeyEditItem _ edit) _ = never edit
    elPutEdit' (KeyDeleteItem a) mr =
        withTransConstraintTM @MonadIO $ do
            mpedits <- putEditAB a Unknown $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
    elPutEdit' (KeyInsertReplaceItem a) mr =
        withTransConstraintTM @MonadIO $ do
            b <- liftIO newb
            getComposeM $ do
                pedits <- MkComposeM $ putEditAB a (Known b) $ tupleReadFunction SelectContext mr
                return $ (MkTupleEdit SelectContent $ KeyInsertReplaceItem b) : fmap (MkTupleEdit SelectContext) pedits
    elPutEdit' KeyClear mr =
        withTransConstraintTM @MonadIO $ do
            bs <- lift $ mr $ MkTupleEditReader SelectContent KeyReadKeys
            getComposeM $ do
                lpedits <-
                    for (toList bs) $ \b -> do
                        aa <- lift $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
                        lpedits <-
                            for (toList aa) $ \a ->
                                MkComposeM $ putEditAB a Unknown $ tupleReadFunction SelectContext mr
                        return $ mconcat lpedits
                return $ fmap (MkTupleEdit SelectContext) $ mconcat lpedits
    elPutEdits' ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (ContextEditReader baseedit (FiniteSetEdit b))
        -> t m (Maybe [ContextEdit baseedit (FiniteSetEdit b)])
    elPutEdits' [] _ = withTransConstraintTM @MonadIO $ getComposeM $ return []
    elPutEdits' (e:ee) mr =
        withTransConstraintTM @MonadIO $
        getComposeM $ do
            ea <- MkComposeM $ elPutEdit' e mr
            eea <- MkComposeM $ elPutEdits' ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkCloseUnlift unlift $ MkAnEditLens elFunction' elPutEdits'

applyInversePinaforeLensSet ::
       forall baseedit a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (FiniteSetEdit b)
    -> PinaforeLensValue baseedit (FiniteSetEdit a)
applyInversePinaforeLensSet newb pm val = pmInverseEditLensSet newb pm . contextualiseEditLens val
