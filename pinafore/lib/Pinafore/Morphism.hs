module Pinafore.Morphism
    ( APinaforeFunctionMorphism(..)
    , APinaforeLensMorphism(..)
    , PinaforeFunctionValue
    , PinaforeFunctionMorphism
    , applyPinaforeFunction
    , PinaforeLensValue
    , lensFunctionValue
    , PinaforeLensMorphism
    , mapPinaforeLensMorphismBase
    , funcPinaforeLensMorphism
    , nullPinaforeLensMorphism
    , applyPinaforeLens
    , applyInversePinaforeLens
    , lensFunctionMorphism
    , lensInverseFunctionMorphism
    ) where

import Shapes
import Truth.Core
import Truth.Debug.Object

type PinaforeLensValue baseedit = EditLens baseedit

data APinaforeFunctionMorphism baseedit t a b = MkAPinaforeFunctionMorphism
    { pfFuncRead :: forall m. MonadIO m =>
                                  MutableRead m (EditReader baseedit) -> a -> t m b
    , pfUpdate :: forall m. MonadIO m =>
                                baseedit -> MutableRead m (EditReader baseedit) -> t m Bool
    }

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

instance Traversable f => CatFunctor (PinaforeFunctionMorphism baseedit) f where
    cfmap (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism f u)) =
        MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism (\mr fa -> withTransConstraintTM @MonadIO $ for fa $ f mr) u

type PinaforeFunctionValue baseedit t = EditFunction baseedit (WholeEdit t)

applyPinaforeFunction ::
       forall baseedit a b.
       PinaforeFunctionMorphism baseedit a b
    -> PinaforeFunctionValue baseedit a
    -> PinaforeFunctionValue baseedit b
applyPinaforeFunction =
    joinUnliftables $ \MkAPinaforeFunctionMorphism {..} (MkAnEditFunction {..} :: AnEditFunction t baseedit (WholeEdit a)) -> let
        getB ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> t m b
        getB mr =
            withTransConstraintTM @MonadIO $ do
                a <- efGet mr ReadWhole
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
                    else return []
        in MkAnEditFunction g u

lensFunctionValue ::
       (FullSubjectReader (EditReader edit), ApplicableEdit edit)
    => PinaforeLensValue baseedit edit
    -> PinaforeFunctionValue baseedit (EditSubject edit)
lensFunctionValue lens = convertEditFunction . editLensFunction lens

data APinaforeLensMorphism baseedit t a b = MkAPinaforeLensMorphism
    { pmForward :: AnEditLens t (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
    , pmInverse :: APinaforeFunctionMorphism baseedit t b (FiniteSet a)
    }

instance Unliftable (APinaforeLensMorphism baseedit) where
    fmapUnliftable mapTM (MkAPinaforeLensMorphism fwd inv) =
        MkAPinaforeLensMorphism (fmapUnliftable mapTM fwd) (fmapUnliftable mapTM inv)

type PinaforeLensMorphism baseedit = CloseUnlift (APinaforeLensMorphism baseedit)

mkComposeT ::
       forall t1 t2 m a. (MonadTransConstraint MonadIO t2, MonadIO m)
    => (MonadIO (t2 m) =>
            t1 (t2 m) a)
    -> ComposeT t1 t2 m a
mkComposeT =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> MkComposeT

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
        readFunc = efGet $ elFunction alens
        fwdB :: AnEditLens (ComposeT t1 t2) (ContextEdit baseB (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
        fwdB = ucCompose fwdA $ liftContentAnEditLens alens
        frB :: forall m. MonadIO m
            => MutableRead m (EditReader baseB)
            -> b
            -> ComposeT t1 t2 m (FiniteSet a)
        frB mr b = mkComposeT $ frA (readFunc mr) b
        updateB ::
               forall m. MonadIO m
            => baseB
            -> MutableRead m (EditReader baseB)
            -> ComposeT t1 t2 m Bool
        updateB beditB mr =
            withTransConstraintTM @MonadIO $ do
                beditAs <- lift2ComposeT' $ efUpdate (elFunction alens) beditB mr
                chs <- for beditAs $ \beditA -> mkComposeT $ updateA beditA $ readFunc mr
                return $ or chs
        invB :: APinaforeFunctionMorphism baseB (ComposeT t1 t2) b (FiniteSet a)
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
        efGet :: ReadFunctionT IdentityT (ContextEditReader baseedit (WholeEdit (Maybe a))) (WholeReader (Maybe a))
        efGet mr ReadWhole = lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        efUpdate ::
               MonadIO m
            => ContextEdit baseedit (WholeEdit (Maybe a))
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe a))))
            -> IdentityT m [WholeEdit (Maybe a)]
        efUpdate (MkTupleEdit SelectContext _) _ = return []
        efUpdate (MkTupleEdit SelectContent edit) _ = return [edit]
        elFunction :: AnEditFunction IdentityT (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe a))
        elFunction = MkAnEditFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit (Maybe a)]
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe a))))
            -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit (Maybe a))])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleEdit SelectContent) edits
        pmForward :: AnEditLens IdentityT (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe a))
        pmForward = MkAnEditLens {..}
        pfFuncRead :: MonadIO m => MutableRead m (EditReader baseedit) -> a -> IdentityT m (FiniteSet a)
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: MonadIO m => baseedit -> MutableRead m (EditReader baseedit) -> IdentityT m Bool
        pfUpdate _ _ = return False
        pmInverse :: APinaforeFunctionMorphism baseedit IdentityT a (FiniteSet a)
        pmInverse = MkAPinaforeFunctionMorphism {..}
        in MkAPinaforeLensMorphism {..}
    ucCompose ::
           forall tab tbc a b c. (MonadTransUnlift tab, MonadTransUnlift tbc)
        => APinaforeLensMorphism baseedit tbc b c
        -> APinaforeLensMorphism baseedit tab a b
        -> APinaforeLensMorphism baseedit (ComposeT tbc tab) a c
    ucCompose (MkAPinaforeLensMorphism (MkAnEditLens (MkAnEditFunction bcGet bcUpdate) bcPutEdit) (MkAPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnEditFunction abGet abUpdate) abPutEdit) (MkAPinaforeFunctionMorphism abInvFuncRead abInvUpdate)) = let
        acGet ::
               ReadFunctionT (ComposeT tbc tab) (ContextEditReader baseedit (WholeEdit (Maybe a))) (WholeReader (Maybe c))
        acGet mra ReadWhole =
            withTransConstraintTM @MonadIO $ do
                mb <- lift2ComposeT'' $ abGet mra ReadWhole
                lift1ComposeT $ bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextEdit baseedit (WholeEdit (Maybe a))
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Maybe a)))
            -> ComposeT tbc tab m [WholeEdit (Maybe c)]
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
        acFunc :: AnEditFunction (ComposeT tbc tab) (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe c))
        acFunc = MkAnEditFunction acGet acUpdate
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit (Maybe c)]
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe a))))
            -> ComposeT tbc tab m (Maybe [ContextEdit baseedit (WholeEdit (Maybe a))])
        acPutEdit editcs mra =
            withTransConstraintTM @MonadIO $
            getCompose $ do
                editpbs <-
                    Compose $
                    MkComposeT $
                    withTransConstraintTM' @MonadIO $
                    bcPutEdit editcs $ bindReadContext (remonadMutableRead lift mra) $ abGet mra
                case partitionContextEdits editpbs of
                    (pinedits, editbs) -> do
                        editpas2 <- Compose $ lift2ComposeT'' $ abPutEdit editbs mra
                        return $ (fmap (MkTupleEdit SelectContext) pinedits) ++ editpas2
        acForward :: AnEditLens (ComposeT tbc tab) (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe c))
        acForward = traceAThing "(.)" $ MkAnEditLens acFunc acPutEdit
        acInvFuncRead ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> c
            -> ComposeT tbc tab m (FiniteSet a)
        acInvFuncRead mr c =
            withTransConstraintTM @MonadIO $ do
                bset <- lift1ComposeT $ bcInvFuncRead mr c
                asetset <- lift2ComposeT'' $ withTransConstraintTM @MonadIO $ for bset $ \b -> abInvFuncRead mr b
                return $ mconcat $ unFiniteSet asetset
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
        acInverse :: APinaforeFunctionMorphism baseedit (ComposeT tbc tab) c (FiniteSet a)
        acInverse = MkAPinaforeFunctionMorphism acInvFuncRead acInvUpdate
        in MkAPinaforeLensMorphism acForward acInverse

funcPinaforeLensMorphism ::
       forall baseedit a b. (a -> Maybe b) -> (b -> FiniteSet a) -> PinaforeLensMorphism baseedit a b
funcPinaforeLensMorphism amb bsa = let
    efGet :: ReadFunctionT IdentityT (ContextEditReader baseedit (WholeEdit (Maybe a))) (WholeReader (Maybe b))
    efGet mr ReadWhole = lift $ fmap (\ma -> ma >>= amb) $ mr $ MkTupleEditReader SelectContent ReadWhole
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit baseedit (WholeEdit (Maybe a))
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe a))))
        -> IdentityT m [WholeEdit (Maybe b)]
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit ma)) _ = return [MkWholeEdit $ ma >>= amb]
    elFunction :: AnEditFunction IdentityT (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe b)]
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe a))))
        -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit (Maybe a))])
    elPutEdits _ _ = return Nothing
    pmForward :: AnEditLens IdentityT (ContextEdit baseedit (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
    pmForward = traceAThing "funcPinaforeLensMorphism" $ MkAnEditLens {..}
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m (EditReader baseedit)
        -> b
        -> IdentityT m (FiniteSet a)
    pfFuncRead _ b = return $ bsa b
    pfUpdate ::
           forall m. MonadIO m
        => baseedit
        -> MutableRead m (EditReader baseedit)
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    pmInverse :: APinaforeFunctionMorphism baseedit IdentityT b (FiniteSet a)
    pmInverse = MkAPinaforeFunctionMorphism {..}
    in MkCloseUnlift identityUnlift MkAPinaforeLensMorphism {..}

nullPinaforeLensMorphism :: forall baseedit a b. PinaforeLensMorphism baseedit a b
nullPinaforeLensMorphism = funcPinaforeLensMorphism (\_ -> Nothing) (\_ -> mempty)

applyPinaforeLens ::
       PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (WholeEdit (Maybe a))
    -> PinaforeLensValue baseedit (WholeEdit (Maybe b))
applyPinaforeLens (MkCloseUnlift unlift pm) val = traceThing "applyPinaforeLens" $ (MkCloseUnlift unlift $ pmForward pm) . contextualiseEditLens val

lensFunctionMorphism ::
       forall baseedit a b. PinaforeLensMorphism baseedit a b -> PinaforeFunctionMorphism baseedit a (Maybe b)
lensFunctionMorphism (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    funcRead ::
           forall m. MonadIO m
        => MutableRead m (EditReader baseedit)
        -> a
        -> t m (Maybe b)
    funcRead mr a = let
        mr' :: MutableRead m (ContextEditReader baseedit (WholeEdit (Maybe a)))
        mr' (MkTupleEditReader SelectContext rt) = mr rt
        mr' (MkTupleEditReader SelectContent ReadWhole) = return $ Just a
        in efGet (elFunction pmForward) mr' ReadWhole
    in MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseedit a b -> PinaforeFunctionMorphism baseedit b (FiniteSet a)
lensInverseFunctionMorphism (MkCloseUnlift unlift MkAPinaforeLensMorphism {..}) = MkCloseUnlift unlift pmInverse

pmInverseEditLens ::
       forall baseedit a b. (Eq a)
    => PinaforeLensMorphism baseedit a b
    -> EditLens (ContextEdit baseedit (WholeEdit (Maybe b))) (FiniteSetEdit a)
pmInverseEditLens (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    getFiniteSet ::
           forall m edit. MonadIO m
        => Maybe b
        -> MutableRead m (ContextEditReader baseedit edit)
        -> t m (FiniteSet a)
    getFiniteSet mb mr =
        withTransConstraintTM @MonadIO $
        case mb of
            Nothing -> return mempty
            Just b -> pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    fsetReadFunction :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit (Maybe b))) (WholeReader (FiniteSet a))
    fsetReadFunction mr ReadWhole =
        withTransConstraintTM @MonadIO $ do
            mb <- lift $ mr (MkTupleEditReader SelectContent ReadWhole)
            case mb of
                Nothing -> return mempty
                Just b -> pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    efGet :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit (Maybe b))) (FiniteSetReader a)
    efGet mr rt = withTransConstraintTM @MonadIO $ wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit baseedit (WholeEdit (Maybe b))
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe b))))
        -> t m [FiniteSetEdit a]
    efUpdate (MkTupleEdit SelectContext pinaedit) mr =
        withTransConstraintTM @MonadIO $ do
            ch <- pfUpdate pmInverse pinaedit $ tupleReadFunction SelectContext mr
            if ch
                then do
                    mb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                    aset <- getFiniteSet mb mr
                    aedits <- getReplaceEditsFromSubject aset
                    return aedits
                else return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit mb)) mr =
        withTransConstraintTM @MonadIO $ do
            aset <- getFiniteSet mb mr
            aedits <- getReplaceEditsFromSubject aset
            return aedits
    elFunction :: AnEditFunction t (ContextEdit baseedit (WholeEdit (Maybe b))) (FiniteSetEdit a)
    elFunction = MkAnEditFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Maybe b)]
        -> MutableRead m (ContextEditReader baseedit (WholeEdit (Maybe a)))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Maybe a))])
    (MkAnEditLens _ putEditBA) = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Maybe b
        -> MutableRead m (EditReader baseedit)
        -> t m (Maybe [baseedit])
    putEditAB a mb mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeEdit mb] $ \case
                    MkTupleEditReader SelectContext rt -> mr rt
                    MkTupleEditReader SelectContent ReadWhole -> return $ Just a
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
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe b))))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Maybe b))])
    elPutEdit (KeyEditItem _ edit) _ = never edit
    elPutEdit (KeyDeleteItem a) mr =
        withTransConstraintTM @MonadIO $ do
            mpedits <- putEditAB a Nothing $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
    elPutEdit (KeyInsertReplaceItem a) mr =
        withTransConstraintTM @MonadIO $ do
            mb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            case mb of
                Just _ -> do
                    mpedits <- putEditAB a mb $ tupleReadFunction SelectContext mr
                    return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
                Nothing -> return Nothing
    elPutEdit KeyClear mr =
        withTransConstraintTM @MonadIO $ do
            mb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            case mb of
                Just b -> do
                    aa <- pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
                    lmpedits <- for (toList aa) $ \a -> putEditAB a Nothing $ tupleReadFunction SelectContext mr
                    return $ fmap (\lpedits -> fmap (MkTupleEdit SelectContext) $ mconcat lpedits) $ sequenceA lmpedits
                Nothing -> return Nothing
    applyEdit' ::
           ContextEdit baseedit (WholeEdit (Maybe b))
        -> ReadFunction (ContextEditReader baseedit (WholeEdit (Maybe b))) (ContextEditReader baseedit (WholeEdit (Maybe b)))
    -- removed line to avoid (ApplicableEdit baseedit) constraint, possibly kinda hacky.
    -- applyEdit' (MkTupleEdit SelectContext edit) mr (MkTupleEditReader SelectContext rt) = applyEdit edit (mr . MkTupleEditReader SelectContext) rt
    applyEdit' (MkTupleEdit SelectContent edit) mr (MkTupleEditReader SelectContent rt) =
        applyEdit edit (mr . MkTupleEditReader SelectContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextEdit baseedit (WholeEdit (Maybe b))]
        -> ReadFunction (ContextEditReader baseedit (WholeEdit (Maybe b))) (ContextEditReader baseedit (WholeEdit (Maybe b)))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit (Maybe b))))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit (Maybe b))])
    elPutEdits [] _ = withTransConstraintTM @MonadIO $ getCompose $ return []
    elPutEdits (e:ee) mr =
        withTransConstraintTM @MonadIO $
        getCompose $ do
            ea <- Compose $ elPutEdit e mr
            eea <- Compose $ elPutEdits ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkCloseUnlift unlift $ MkAnEditLens {..}

applyInversePinaforeLens ::
       forall baseedit a b. (Eq a, Eq b)
    => PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (WholeEdit (Maybe b))
    -> PinaforeLensValue baseedit (FiniteSetEdit a)
applyInversePinaforeLens pm val = pmInverseEditLens pm . contextualiseEditLens val
