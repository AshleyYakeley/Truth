module Pinafore.Morphism
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

mapPinaforeFunctionMorphismBase ::
       forall baseA baseB a b.
       EditFunction baseB baseA
    -> PinaforeFunctionMorphism baseA a b
    -> PinaforeFunctionMorphism baseB a b
mapPinaforeFunctionMorphismBase ef morph = let
    call ::
           forall t1 t2. (MonadTransUnlift t1, MonadTransUnlift t2)
        => APinaforeFunctionMorphism baseA t1 a b
        -> AnEditFunction t2 baseB baseA
        -> APinaforeFunctionMorphism baseB (ComposeT t1 t2) a b
    call (MkAPinaforeFunctionMorphism frA updateA) aef = let
        readFunc :: ReadFunctionT t2 (EditReader baseB) (EditReader baseA)
        readFunc = efGet aef
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
                beditAs <- lift2ComposeT' $ efUpdate aef beditB mr
                chs <- for beditAs $ \beditA -> mkComposeT $ updateA beditA $ readFunc mr
                return $ or chs
        in MkAPinaforeFunctionMorphism frB updateB
    in joinUnlifts call morph ef

lensFunctionValue ::
       (FullSubjectReader (EditReader edit), ApplicableEdit edit)
    => PinaforeLensValue baseedit edit
    -> PinaforeFunctionValue baseedit (EditSubject edit)
lensFunctionValue lens = convertEditFunction . editLensFunction lens

data APinaforeLensMorphism baseedit t a b = MkAPinaforeLensMorphism
    { pmForward :: AnEditLens t (ContextEdit baseedit (WholeEdit a)) (WholeEdit b)
    , pmInverse :: APinaforeFunctionMorphism baseedit t b (FiniteSet a)
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
        fwdB :: AnEditLens (ComposeT t1 t2) (ContextEdit baseB (WholeEdit a)) (WholeEdit b)
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
        efGet :: ReadFunctionT IdentityT (ContextEditReader baseedit (WholeEdit a)) (WholeReader a)
        efGet mr ReadWhole = lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        efUpdate ::
               MonadIO m
            => ContextEdit baseedit (WholeEdit a)
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit a)))
            -> IdentityT m [WholeEdit a]
        efUpdate (MkTupleEdit SelectContext _) _ = return []
        efUpdate (MkTupleEdit SelectContent edit) _ = return [edit]
        elFunction :: AnEditFunction IdentityT (ContextEdit baseedit (WholeEdit a)) (WholeEdit a)
        elFunction = MkAnEditFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit a]
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit a)))
            -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit a)])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleEdit SelectContent) edits
        pmForward :: AnEditLens IdentityT (ContextEdit baseedit (WholeEdit a)) (WholeEdit a)
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
        acGet :: ReadFunctionT (ComposeT tbc tab) (ContextEditReader baseedit (WholeEdit a)) (WholeReader c)
        acGet mra ReadWhole =
            withTransConstraintTM @MonadIO $ do
                mb <- lift2ComposeT'' $ abGet mra ReadWhole
                lift1ComposeT $ bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextEdit baseedit (WholeEdit a)
            -> MutableRead m (ContextEditReader baseedit (WholeEdit a))
            -> ComposeT tbc tab m [WholeEdit c]
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
        acFunc :: AnEditFunction (ComposeT tbc tab) (ContextEdit baseedit (WholeEdit a)) (WholeEdit c)
        acFunc = MkAnEditFunction acGet acUpdate
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit c]
            -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit a)))
            -> ComposeT tbc tab m (Maybe [ContextEdit baseedit (WholeEdit a)])
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
        acForward :: AnEditLens (ComposeT tbc tab) (ContextEdit baseedit (WholeEdit a)) (WholeEdit c)
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

funcPinaforeLensMorphism :: forall baseedit a b. (a -> b) -> (b -> FiniteSet a) -> PinaforeLensMorphism baseedit a b
funcPinaforeLensMorphism ab bsa = let
    efGet :: ReadFunctionT IdentityT (ContextEditReader baseedit (WholeEdit a)) (WholeReader b)
    efGet mr ReadWhole = lift $ fmap ab $ mr $ MkTupleEditReader SelectContent ReadWhole
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit baseedit (WholeEdit a)
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit a)))
        -> IdentityT m [WholeEdit b]
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit a)) _ = return [MkWholeEdit $ ab a]
    elFunction :: AnEditFunction IdentityT (ContextEdit baseedit (WholeEdit a)) (WholeEdit b)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit b]
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit a)))
        -> IdentityT m (Maybe [ContextEdit baseedit (WholeEdit a)])
    elPutEdits _ _ = return Nothing
    pmForward :: AnEditLens IdentityT (ContextEdit baseedit (WholeEdit a)) (WholeEdit b)
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

nullPinaforeLensMorphism :: forall baseedit a b. PinaforeLensMorphism baseedit a (Maybe b)
nullPinaforeLensMorphism = funcPinaforeLensMorphism (\_ -> Nothing) (\_ -> mempty)

applyPinaforeLens ::
       PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (WholeEdit a)
    -> PinaforeLensValue baseedit (WholeEdit b)
applyPinaforeLens (MkCloseUnlift unlift pm) val = traceThing "applyPinaforeLens" $ (MkCloseUnlift unlift $ pmForward pm) . contextualiseEditLens val

lensFunctionMorphism :: forall baseedit a b. PinaforeLensMorphism baseedit a b -> PinaforeFunctionMorphism baseedit a b
lensFunctionMorphism (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    funcRead ::
           forall m. MonadIO m
        => MutableRead m (EditReader baseedit)
        -> a
        -> t m b
    funcRead mr a = let
        mr' :: MutableRead m (ContextEditReader baseedit (WholeEdit a))
        mr' (MkTupleEditReader SelectContext rt) = mr rt
        mr' (MkTupleEditReader SelectContent ReadWhole) = return a
        in efGet (elFunction pmForward) mr' ReadWhole
    in MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism funcRead (pfUpdate pmInverse)

lensInverseFunctionMorphism :: PinaforeLensMorphism baseedit a b -> PinaforeFunctionMorphism baseedit b (FiniteSet a)
lensInverseFunctionMorphism (MkCloseUnlift unlift MkAPinaforeLensMorphism {..}) = MkCloseUnlift unlift pmInverse

pmInverseEditLens ::
       forall baseedit a b. Eq a
    => IO b
    -> PinaforeLensMorphism baseedit a b
    -> EditLens (ContextEdit baseedit (WholeEdit b)) (FiniteSetEdit a)
pmInverseEditLens getNullValue (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    getFiniteSet ::
           forall m edit. MonadIO m
        => b
        -> MutableRead m (ContextEditReader baseedit edit)
        -> t m (FiniteSet a)
    getFiniteSet b mr = withTransConstraintTM @MonadIO $ pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    fsetReadFunction :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit b)) (WholeReader (FiniteSet a))
    fsetReadFunction mr ReadWhole =
        withTransConstraintTM @MonadIO $ do
            b <- lift $ mr (MkTupleEditReader SelectContent ReadWhole)
            pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
    efGet :: ReadFunctionT t (ContextEditReader baseedit (WholeEdit b)) (FiniteSetReader a)
    efGet mr rt = withTransConstraintTM @MonadIO $ wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit baseedit (WholeEdit b)
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit b)))
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
    elFunction :: AnEditFunction t (ContextEdit baseedit (WholeEdit b)) (FiniteSetEdit a)
    elFunction = MkAnEditFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit b]
        -> MutableRead m (ContextEditReader baseedit (WholeEdit a))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit a)])
    (MkAnEditLens _ putEditBA) = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> b
        -> MutableRead m (EditReader baseedit)
        -> t m (Maybe [baseedit])
    putEditAB a b mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeEdit b] $ \case
                    MkTupleEditReader SelectContext rt -> mr rt
                    MkTupleEditReader SelectContent ReadWhole -> return a
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
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit b)))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit b)])
    elPutEdit (KeyEditItem _ edit) _ = never edit
    elPutEdit (KeyDeleteItem a) mr =
        withTransConstraintTM @MonadIO $ do
            nullValue <- liftIO getNullValue
            mpedits <- putEditAB a nullValue $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
    elPutEdit (KeyInsertReplaceItem a) mr =
        withTransConstraintTM @MonadIO $ do
            b <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            mpedits <- putEditAB a b $ tupleReadFunction SelectContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit SelectContext) pedits) mpedits
    elPutEdit KeyClear mr =
        withTransConstraintTM @MonadIO $ do
            b <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            aa <- pfFuncRead pmInverse (tupleReadFunction SelectContext mr) b
            lmpedits <-
                for (toList aa) $ \a -> do
                    nullValue <- liftIO getNullValue
                    putEditAB a nullValue $ tupleReadFunction SelectContext mr
            return $ fmap (\lpedits -> fmap (MkTupleEdit SelectContext) $ mconcat lpedits) $ sequenceA lmpedits
    applyEdit' ::
           ContextEdit baseedit (WholeEdit b)
        -> ReadFunction (ContextEditReader baseedit (WholeEdit b)) (ContextEditReader baseedit (WholeEdit b))
    -- removed line to avoid (ApplicableEdit baseedit) constraint, possibly kinda hacky.
    -- applyEdit' (MkTupleEdit SelectContext edit) mr (MkTupleEditReader SelectContext rt) = applyEdit edit (mr . MkTupleEditReader SelectContext) rt
    applyEdit' (MkTupleEdit SelectContent edit) mr (MkTupleEditReader SelectContent rt) =
        applyEdit edit (mr . MkTupleEditReader SelectContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextEdit baseedit (WholeEdit b)]
        -> ReadFunction (ContextEditReader baseedit (WholeEdit b)) (ContextEditReader baseedit (WholeEdit b))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (EditReader (ContextEdit baseedit (WholeEdit b)))
        -> t m (Maybe [ContextEdit baseedit (WholeEdit b)])
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
    => IO b
    -> PinaforeLensMorphism baseedit a b
    -> PinaforeLensValue baseedit (WholeEdit b)
    -> PinaforeLensValue baseedit (FiniteSetEdit a)
applyInversePinaforeLens getNullValue pm val = pmInverseEditLens getNullValue pm . contextualiseEditLens val
