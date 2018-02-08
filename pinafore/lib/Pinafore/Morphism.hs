module Pinafore.Morphism
    ( PinaforeFunctionValue
    , PinaforeFunctionMorphism
    , applyPinaforeFunction
    , PinaforeLensValue
    , lensFunctionValue
    , PinaforeLensMorphism
    , funcPinaforeLensMorphism
    , applyPinaforeLens
    , applyInversePinaforeLens
    , lensFunctionMorphism
    , lensInverseFunctionMorphism
    , literalPinaforeLensMorphism
    , predicatePinaforeLensMorphism
    ) where

import Pinafore.AsText
import Pinafore.Edit
import Shapes
import Truth.Core
import Truth.Debug

type PinaforeLensValue = EditLens PinaforeEdit

data APinaforeFunctionMorphism t a b = MkAPinaforeFunctionMorphism
    { pfFuncRead :: forall m. MonadIO m =>
                                  MutableRead m PinaforeRead -> a -> t m b
    , pfUpdate :: PinaforeEdit -> Bool
    }

type PinaforeFunctionMorphism = CloseUnlift APinaforeFunctionMorphism

instance Functor (PinaforeFunctionMorphism a) where
    fmap f fm = arr f . fm

instance Applicative (PinaforeFunctionMorphism a) where
    pure b = arr $ \_ -> b
    fmxy <*> fmx =
        proc a -> do
            xy <- fmxy -< a
            x <- fmx -< a
            returnA -< xy x

instance Unliftable APinaforeFunctionMorphism where
    fmapUnliftable mapm (MkAPinaforeFunctionMorphism fr u) = MkAPinaforeFunctionMorphism (\mr a -> mapm $ fr mr a) u

instance UnliftCategory APinaforeFunctionMorphism where
    type UnliftCategoryConstraint APinaforeFunctionMorphism a = ()
    ucId = let
        pfFuncRead _ = return
        pfUpdate _ = False
        in MkAPinaforeFunctionMorphism {..}
    ucCompose (MkAPinaforeFunctionMorphism lbc ubc) (MkAPinaforeFunctionMorphism lab uab) =
        MkAPinaforeFunctionMorphism
        { pfFuncRead =
              \mr a ->
                  withTransConstraintTM @MonadIO $ do
                      b <- lift2ComposeT'' $ lab mr a
                      lift1ComposeT $ lbc mr b
        , pfUpdate = \edit -> uab edit || ubc edit
        }

instance Category PinaforeFunctionMorphism where
    id = cid
    (.) = (<.>)

instance Arrow PinaforeFunctionMorphism where
    arr ab =
        MkCloseUnlift
            identityUnlift
            MkAPinaforeFunctionMorphism {pfFuncRead = \_ a -> return $ ab a, pfUpdate = \_ -> False}
    first (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism bc u)) =
        MkCloseUnlift unlift $
        MkAPinaforeFunctionMorphism
            (\mr (b, d) ->
                 withTransConstraintTM @MonadIO $ do
                     c <- bc mr b
                     return (c, d))
            u
    second = cfmap

instance Traversable f => CatFunctor PinaforeFunctionMorphism f where
    cfmap (MkCloseUnlift unlift (MkAPinaforeFunctionMorphism f u)) =
        MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism (\mr fa -> withTransConstraintTM @MonadIO $ for fa $ f mr) u

type PinaforeFunctionValue t = EditFunction PinaforeEdit (WholeEdit t)

applyPinaforeFunction :: forall a b. PinaforeFunctionMorphism a b -> PinaforeFunctionValue a -> PinaforeFunctionValue b
applyPinaforeFunction =
    joinUnlifts $ \(unlift :: Unlift t) MkAPinaforeFunctionMorphism {..} MkAnEditFunction {..} ->
        MkCloseUnlift unlift $ let
            getB ::
                   forall m. MonadIO m
                => MutableRead m PinaforeRead
                -> t m b
            getB mr =
                withTransConstraintTM @MonadIO $ do
                    a <- efGet mr ReadWhole
                    pfFuncRead mr a
            g :: ReadFunctionT t (EditReader PinaforeEdit) (WholeReader b)
            g mr ReadWhole = getB mr
            u :: forall m. MonadIO m
              => PinaforeEdit
              -> MutableRead m (EditReader PinaforeEdit)
              -> t m [WholeEdit b]
            u pinedit mr
                | pfUpdate pinedit =
                    withTransConstraintTM @MonadIO $ do
                        b <- getB mr
                        return [MkWholeEdit b]
            u _ _ = withTransConstraintTM @MonadIO $ return []
            in MkAnEditFunction g u

lensFunctionValue ::
       (FullSubjectReader (EditReader edit), Edit edit)
    => PinaforeLensValue edit
    -> PinaforeFunctionValue (EditSubject edit)
lensFunctionValue lens = convertEditFunction <.> editLensFunction lens

data APinaforeLensMorphism t a b = MkAPinaforeLensMorphism
    { pmForward :: AnEditLens t (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
    , pmInverse :: APinaforeFunctionMorphism t b (FiniteSet a)
    }

type PinaforeLensMorphism = CloseUnlift APinaforeLensMorphism

bindReadContext ::
       MutableRead m (ContextEditReader editx edita)
    -> MutableRead m (EditReader editb)
    -> MutableRead m (ContextEditReader editx editb)
bindReadContext mr _ (MkTupleEditReader EditContext rt) = mr $ MkTupleEditReader EditContext rt
bindReadContext _ mr (MkTupleEditReader EditContent rt) = mr rt

instance UnliftCategory APinaforeLensMorphism where
    type UnliftCategoryConstraint APinaforeLensMorphism a = ()
    ucId = let
        efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforeEdit (WholeEdit (Maybe a))) (WholeReader (Maybe a))
        efGet mr ReadWhole = lift $ mr $ MkTupleEditReader EditContent ReadWhole
        efUpdate ::
               MonadIO m
            => ContextEdit PinaforeEdit (WholeEdit (Maybe a))
            -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe a))))
            -> IdentityT m [WholeEdit (Maybe a)]
        efUpdate (MkTupleEdit EditContext _) _ = return []
        efUpdate (MkTupleEdit EditContent edit) _ = return [edit]
        elFunction :: AnEditFunction IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe a))
        elFunction = MkAnEditFunction {..}
        elPutEdits ::
               MonadIO m
            => [WholeEdit (Maybe a)]
            -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe a))))
            -> IdentityT m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe a))])
        elPutEdits edits _ = return $ Just $ fmap (MkTupleEdit EditContent) edits
        pmForward :: AnEditLens IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe a))
        pmForward = MkAnEditLens {..}
        pfFuncRead :: MonadIO m => MutableRead m PinaforeRead -> a -> IdentityT m (FiniteSet a)
        pfFuncRead _ a = return $ opoint a
        pfUpdate :: PinaforeEdit -> Bool
        pfUpdate _ = False
        pmInverse :: APinaforeFunctionMorphism IdentityT a (FiniteSet a)
        pmInverse = MkAPinaforeFunctionMorphism {..}
        in MkAPinaforeLensMorphism {..}
    ucCompose ::
           forall tab tbc a b c. (MonadTransUnlift tab, MonadTransUnlift tbc)
        => APinaforeLensMorphism tbc b c
        -> APinaforeLensMorphism tab a b
        -> APinaforeLensMorphism (ComposeT tbc tab) a c
    ucCompose (MkAPinaforeLensMorphism (MkAnEditLens (MkAnEditFunction bcGet bcUpdate) bcPutEdit) (MkAPinaforeFunctionMorphism bcInvFuncRead bcInvUpdate)) (MkAPinaforeLensMorphism (MkAnEditLens (MkAnEditFunction abGet abUpdate) abPutEdit) (MkAPinaforeFunctionMorphism abInvFuncRead abInvUpdate)) = let
        acGet ::
               ReadFunctionT (ComposeT tbc tab) (ContextEditReader PinaforeEdit (WholeEdit (Maybe a))) (WholeReader (Maybe c))
        acGet mra ReadWhole =
            withTransConstraintTM @MonadIO $ do
                mb <- lift2ComposeT'' $ abGet mra ReadWhole
                lift1ComposeT $ bcGet (bindReadContext mra $ subjectToMutableRead mb) ReadWhole
        acUpdate ::
               forall m. MonadIO m
            => ContextEdit PinaforeEdit (WholeEdit (Maybe a))
            -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe a)))
            -> ComposeT tbc tab m [WholeEdit (Maybe c)]
        acUpdate (MkTupleEdit EditContext pinedit) mra =
            withTransConstraintTM @MonadIO $ do
                editbs <- lift2ComposeT'' $ abUpdate (MkTupleEdit EditContext pinedit) mra
                editcs1 <-
                    MkComposeT $
                    withTransConstraintTM' @MonadIO $
                    bcUpdate (MkTupleEdit EditContext pinedit) $
                    bindReadContext (remonadMutableRead lift mra) $ abGet mra
                editcss2 <-
                    for editbs $ \editb@(MkWholeEdit mb) ->
                        lift1ComposeT $
                        bcUpdate (MkTupleEdit EditContent editb) $ bindReadContext mra $ subjectToMutableRead mb
                return $ editcs1 ++ mconcat editcss2
        acUpdate (MkTupleEdit EditContent edita) mra =
            withTransConstraintTM @MonadIO $ do
                editbs <- lift2ComposeT'' $ abUpdate (MkTupleEdit EditContent edita) mra
                editcss <-
                    for editbs $ \editb@(MkWholeEdit mb) ->
                        lift1ComposeT $
                        bcUpdate (MkTupleEdit EditContent editb) $ bindReadContext mra $ subjectToMutableRead mb
                return $ mconcat editcss
        acFunc ::
               AnEditFunction (ComposeT tbc tab) (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe c))
        acFunc = MkAnEditFunction acGet acUpdate
        acPutEdit ::
               forall m. MonadIO m
            => [WholeEdit (Maybe c)]
            -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe a))))
            -> ComposeT tbc tab m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe a))])
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
                        return $ (fmap (MkTupleEdit EditContext) pinedits) ++ editpas2
        acForward ::
               AnEditLens (ComposeT tbc tab) (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe c))
        acForward = MkAnEditLens acFunc acPutEdit
        acInvFuncRead ::
               forall m. MonadIO m
            => MutableRead m PinaforeRead
            -> c
            -> ComposeT tbc tab m (FiniteSet a)
        acInvFuncRead mr c =
            withTransConstraintTM @MonadIO $ do
                bset <- lift1ComposeT $ bcInvFuncRead mr c
                asetset <- lift2ComposeT'' $ withTransConstraintTM @MonadIO $ for bset $ \b -> abInvFuncRead mr b
                return $ mconcat $ unFiniteSet asetset
        acInvUpdate :: PinaforeEdit -> Bool
        acInvUpdate pedit = bcInvUpdate pedit || abInvUpdate pedit
        acInverse :: APinaforeFunctionMorphism (ComposeT tbc tab) c (FiniteSet a)
        acInverse = MkAPinaforeFunctionMorphism acInvFuncRead acInvUpdate
        in MkAPinaforeLensMorphism acForward acInverse

instance Category PinaforeLensMorphism where
    id = cid
    (.) = (<.>)

funcPinaforeLensMorphism :: forall a b. (a -> Maybe b) -> (b -> FiniteSet a) -> PinaforeLensMorphism a b
funcPinaforeLensMorphism amb bsa = let
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforeEdit (WholeEdit (Maybe a))) (WholeReader (Maybe b))
    efGet mr ReadWhole = lift $ fmap (\ma -> ma >>= amb) $ mr $ MkTupleEditReader EditContent ReadWhole
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEdit (WholeEdit (Maybe a))
        -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe a))))
        -> IdentityT m [WholeEdit (Maybe b)]
    efUpdate (MkTupleEdit EditContext _) _ = return []
    efUpdate (MkTupleEdit EditContent (MkWholeEdit ma)) _ = return [MkWholeEdit $ ma >>= amb]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe b)]
        -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe a))))
        -> IdentityT m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe a))])
    elPutEdits _ _ = return Nothing
    pmForward :: AnEditLens IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe a))) (WholeEdit (Maybe b))
    pmForward = MkAnEditLens {..}
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeRead
        -> b
        -> IdentityT m (FiniteSet a)
    pfFuncRead _ b = return $ bsa b
    pmInverse :: APinaforeFunctionMorphism IdentityT b (FiniteSet a)
    pmInverse = MkAPinaforeFunctionMorphism {..}
    pfUpdate _ = False
    in MkCloseUnlift identityUnlift MkAPinaforeLensMorphism {..}

applyPinaforeLens ::
       PinaforeLensMorphism a b -> PinaforeLensValue (WholeEdit (Maybe a)) -> PinaforeLensValue (WholeEdit (Maybe b))
applyPinaforeLens (MkCloseUnlift unlift pm) val = (MkCloseUnlift unlift $ pmForward pm) <.> contextualiseEditLens val

lensFunctionMorphism :: forall a b. PinaforeLensMorphism a b -> PinaforeFunctionMorphism a (Maybe b)
lensFunctionMorphism (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    funcRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeRead
        -> a
        -> t m (Maybe b)
    funcRead mr a = let
        mr' :: MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe a)))
        mr' (MkTupleEditReader EditContext rt) = mr rt
        mr' (MkTupleEditReader EditContent ReadWhole) = return $ Just a
        in efGet (elFunction pmForward) mr' ReadWhole
    update :: PinaforeEdit -> Bool
    update = pfUpdate pmInverse
    in MkCloseUnlift unlift $ MkAPinaforeFunctionMorphism funcRead update

lensInverseFunctionMorphism :: PinaforeLensMorphism a b -> PinaforeFunctionMorphism b (FiniteSet a)
lensInverseFunctionMorphism (MkCloseUnlift unlift MkAPinaforeLensMorphism {..}) = MkCloseUnlift unlift pmInverse

pmInverseEditLens ::
       forall a b. (Eq a)
    => PinaforeLensMorphism a b
    -> EditLens (ContextEdit PinaforeEdit (WholeEdit (Maybe b))) (FiniteSetEdit a)
pmInverseEditLens (MkCloseUnlift (unlift :: Unlift t) MkAPinaforeLensMorphism {..}) = let
    getFiniteSet ::
           forall m edit. MonadIO m
        => Maybe b
        -> MutableRead m (ContextEditReader PinaforeEdit edit)
        -> t m (FiniteSet a)
    getFiniteSet mb mr =
        withTransConstraintTM @MonadIO $
        case mb of
            Nothing -> return mempty
            Just b -> pfFuncRead pmInverse (tupleReadFunction EditContext mr) b
    fsetReadFunction ::
           ReadFunctionT t (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) (WholeReader (FiniteSet a))
    fsetReadFunction mr ReadWhole =
        withTransConstraintTM @MonadIO $ do
            mb <- lift $ mr (MkTupleEditReader EditContent ReadWhole)
            case mb of
                Nothing -> return mempty
                Just b -> pfFuncRead pmInverse (tupleReadFunction EditContext mr) b
    efGet :: ReadFunctionT t (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) (FiniteSetReader a)
    efGet mr rt = withTransConstraintTM @MonadIO $ wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEdit (WholeEdit (Maybe b))
        -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe b))))
        -> t m [FiniteSetEdit a]
    efUpdate (MkTupleEdit EditContext pinaedit) mr
        | pfUpdate pmInverse pinaedit =
            withTransConstraintTM @MonadIO $ do
                mb <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
                aset <- getFiniteSet mb mr
                aedits <- getReplaceEditsFromSubject aset
                return aedits
    efUpdate (MkTupleEdit EditContent (MkWholeEdit mb)) mr =
        withTransConstraintTM @MonadIO $ do
            aset <- getFiniteSet mb mr
            aedits <- getReplaceEditsFromSubject aset
            return aedits
    efUpdate (MkTupleEdit EditContext _) _ = withTransConstraintTM @MonadIO $ return []
    elFunction :: AnEditFunction t (ContextEdit PinaforeEdit (WholeEdit (Maybe b))) (FiniteSetEdit a)
    elFunction = MkAnEditFunction {..}
    putEditBA ::
           forall m. MonadIO m
        => [WholeEdit (Maybe b)]
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe a)))
        -> t m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe a))])
    (MkAnEditLens _ putEditBA) = pmForward
    putEditAB ::
           forall m. MonadIO m
        => a
        -> Maybe b
        -> MutableRead m PinaforeRead
        -> t m (Maybe [PinaforeEdit])
    putEditAB a mb mr =
        withTransConstraintTM @MonadIO $ do
            medits <-
                putEditBA [MkWholeEdit mb] $ \case
                    MkTupleEditReader EditContext rt -> mr rt
                    MkTupleEditReader EditContent ReadWhole -> return $ Just a
            return $
                fmap
                    (\edits ->
                         mapMaybe
                             (\case
                                  MkTupleEdit EditContext edit -> Just edit
                                  MkTupleEdit EditContent _ -> Nothing)
                             edits)
                    medits
    elPutEdit ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe b))))
        -> t m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe b))])
    elPutEdit (KeyEditItem _ edit) _ = never edit
    elPutEdit (KeyDeleteItem a) mr =
        withTransConstraintTM @MonadIO $ do
            mpedits <- putEditAB a Nothing $ tupleReadFunction EditContext mr
            return $ fmap (\pedits -> fmap (MkTupleEdit EditContext) pedits) mpedits
    elPutEdit (KeyInsertReplaceItem a) mr =
        withTransConstraintTM @MonadIO $ do
            mb <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
            case mb of
                Just _ -> do
                    mpedits <- putEditAB a mb $ tupleReadFunction EditContext mr
                    return $ fmap (\pedits -> fmap (MkTupleEdit EditContext) pedits) mpedits
                Nothing -> return Nothing
    elPutEdit KeyClear mr =
        withTransConstraintTM @MonadIO $ do
            mb <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
            case mb of
                Just b -> do
                    aa <- pfFuncRead pmInverse (tupleReadFunction EditContext mr) b
                    lmpedits <- for (toList aa) $ \a -> putEditAB a Nothing $ tupleReadFunction EditContext mr
                    return $ fmap (\lpedits -> fmap (MkTupleEdit EditContext) $ mconcat lpedits) $ sequenceA lmpedits
                Nothing -> return Nothing
    applyEdit' ::
           ContextEdit PinaforeEdit (WholeEdit (Maybe b))
        -> ReadFunction (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) (ContextEditReader PinaforeEdit (WholeEdit (Maybe b)))
    -- removed line to avoid (Edit PinaforeEdit) constraint, possibly kinda hacky.
    -- applyEdit' (MkTupleEdit EditContext edit) mr (MkTupleEditReader EditContext rt) = applyEdit edit (mr . MkTupleEditReader EditContext) rt
    applyEdit' (MkTupleEdit EditContent edit) mr (MkTupleEditReader EditContent rt) =
        applyEdit edit (mr . MkTupleEditReader EditContent) rt
    applyEdit' _ mr rt = mr rt
    applyEdits' ::
           [ContextEdit PinaforeEdit (WholeEdit (Maybe b))]
        -> ReadFunction (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) (ContextEditReader PinaforeEdit (WholeEdit (Maybe b)))
    applyEdits' [] mr = mr
    applyEdits' (e:es) mr = applyEdits' es $ applyEdit' e mr
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> MutableRead m (EditReader (ContextEdit PinaforeEdit (WholeEdit (Maybe b))))
        -> t m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe b))])
    elPutEdits [] _ = withTransConstraintTM @MonadIO $ getCompose $ return []
    elPutEdits (e:ee) mr =
        withTransConstraintTM @MonadIO $
        getCompose $ do
            ea <- Compose $ elPutEdit e mr
            eea <- Compose $ elPutEdits ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkCloseUnlift unlift $ MkAnEditLens {..}

applyInversePinaforeLens ::
       forall a b. (Eq a, Eq b)
    => PinaforeLensMorphism a b
    -> PinaforeLensValue (WholeEdit (Maybe b))
    -> PinaforeLensValue (FiniteSetEdit a)
applyInversePinaforeLens pm val = pmInverseEditLens pm <.> contextualiseEditLens val

literalPinaforeMap ::
       forall val. AsText val
    => AnEditLens IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe val))
literalPinaforeMap = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (WholeReader (Maybe val))
    efGet mr ReadWhole =
        lift $ do
            msubj <- mr $ MkTupleEditReader EditContent ReadWhole
            case msubj of
                Just subj -> do
                    mbs <- mr $ MkTupleEditReader EditContext $ PinaforeReadGetLiteral subj
                    return $ mbs >>= fromText
                Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEdit (WholeEdit (Maybe Point))
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point)))
        -> IdentityT m [WholeEdit (Maybe val)]
    efUpdate (MkTupleEdit EditContext (PinaforeEditSetLiteral s mbs)) mr = do
        msubj <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
        return $
            if Just s == msubj
                then [MkWholeEdit $ mbs >>= fromText]
                else []
    efUpdate (MkTupleEdit EditContext _) _ = return []
    efUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) _ = return [MkWholeEdit Nothing]
    efUpdate (MkTupleEdit EditContent (MkWholeEdit (Just subj))) mr = do
        mbs <- lift $ mr $ MkTupleEditReader EditContext $ PinaforeReadGetLiteral subj
        return $ [MkWholeEdit $ mbs >>= fromText]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe val))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe val)
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))])
    elPutEdit (MkWholeEdit (fmap toText -> mbs)) mr = do
        msubj <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
        case msubj of
            Just subj -> return $ Just [MkTupleEdit EditContext $ PinaforeEditSetLiteral subj mbs]
            Nothing -> do
                subj <- liftIO randomIO
                return $
                    Just
                        [ MkTupleEdit EditContent $ MkWholeEdit $ Just subj
                        , MkTupleEdit EditContext $ PinaforeEditSetLiteral subj mbs
                        ]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe val)]
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall val. AsText val
    => APinaforeFunctionMorphism IdentityT val (FiniteSet Point)
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeRead
        -> val
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeReadLookupLiteral $ toText val
    pfUpdate :: PinaforeEdit -> Bool
    pfUpdate (PinaforeEditSetLiteral _ _) = True
    pfUpdate _ = False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeLensMorphism ::
       forall val. AsText val
    => PinaforeLensMorphism Point val
literalPinaforeLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap literalInverseFunction

predicatePinaforeMap ::
       Predicate -> AnEditLens IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe Point))
predicatePinaforeMap prd = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (WholeReader (Maybe Point))
    efGet mr ReadWhole =
        traceBracket ("editGet " ++ show prd) $
        lift $ do
            msubj <- mr $ MkTupleEditReader EditContent ReadWhole
            case msubj of
                Just subj -> mr $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj
                Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEdit (WholeEdit (Maybe Point))
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point)))
        -> IdentityT m [WholeEdit (Maybe Point)]
    efUpdate (MkTupleEdit EditContext (PinaforeEditSetValue p s mv)) mr
        | p == prd = do
            msubj <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
            return $
                if Just s == msubj
                    then [MkWholeEdit mv]
                    else []
    efUpdate (MkTupleEdit EditContext _) _ = return []
    efUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) _ = return [MkWholeEdit Nothing]
    efUpdate (MkTupleEdit EditContent (MkWholeEdit (Just subj))) mr = do
        mval <- lift $ mr $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj
        return [MkWholeEdit mval]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe Point))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe Point)
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))])
    elPutEdit (MkWholeEdit mv) mr = do
        msubj <- lift $ mr $ MkTupleEditReader EditContent ReadWhole
        case msubj of
            Just subj -> return $ Just [MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv]
            Nothing -> do
                subj <- liftIO randomIO
                return $
                    Just
                        [ MkTupleEdit EditContent $ MkWholeEdit $ Just subj
                        , MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv
                        ]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe Point)]
        -> MutableRead m (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction :: Predicate -> APinaforeFunctionMorphism IdentityT Point (FiniteSet Point)
predicateInverseFunction prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeRead
        -> Point
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeReadLookupValue prd val
    pfUpdate :: PinaforeEdit -> Bool
    pfUpdate (PinaforeEditSetValue p _ _)
        | p == prd = True
    pfUpdate _ = False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeLensMorphism :: Predicate -> PinaforeLensMorphism Point Point
predicatePinaforeLensMorphism prd =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism (predicatePinaforeMap prd) (predicateInverseFunction prd)
