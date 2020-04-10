module Pinafore.Base.Morphism
    ( PinaforeLensMorphism(..)
    , identityPinaforeLensMorphism
    , composePinaforeLensMorphism
    , mapPinaforeLensMorphismBase
    , funcPinaforeLensMorphism
    , nullPinaforeLensMorphism
    , bijectionPinaforeLensMorphism
    , pairPinaforeLensMorphism
    , eitherPinaforeLensMorphism
    , lensFunctionMorphism
    , lensInverseFunctionMorphism
    , pinaforeLensMorphismChangeLens
    , pinaforeLensMorphismInverseChangeLens
    , pinaforeLensMorphismInverseChangeLensSet
    ) where

import Pinafore.Base.FunctionMorphism
import Pinafore.Base.Know
import Shapes
import Truth.Core

data PinaforeLensMorphism baseupdate ap aq bp bq = MkPinaforeLensMorphism
    { pmGet :: ap -> ReadM (UpdateReader baseupdate) (Know bq)
    , pmBaseUpdate :: baseupdate -> ReadM (UpdateReader baseupdate) Bool
    , pmPut :: Know ap -> Know bp -> ReadM (UpdateReader baseupdate) (Maybe ([UpdateEdit baseupdate], Maybe (Know aq)))
    , pmInv :: bp -> ReadM (UpdateReader baseupdate) [aq] -- not guaranteed to be unique
    }

instance Functor (PinaforeLensMorphism baseupdate ap aq bp) where
    fmap f (MkPinaforeLensMorphism g bu p i) = MkPinaforeLensMorphism ((fmap $ fmap $ fmap f) g) bu p i

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (PinaforeLensMorphism baseupdate ap aq) where
    cfmap f =
        MkNestedMorphism $ \(MkPinaforeLensMorphism g bu p i) -> let
            p' = (fmap $ cfmap1 $ endocfmap f) p
            i' = (cfmap1 f) i
            in MkPinaforeLensMorphism g bu p' i'

instance CatFunctor (->) (NestedMorphism (->)) (PinaforeLensMorphism baseupdate ap) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkPinaforeLensMorphism g bu p i) -> let
            p' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap $ fmap f) p
            i' = (fmap $ fmap $ fmap f) i
            in MkPinaforeLensMorphism g bu p' i'

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (PinaforeLensMorphism baseupdate) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $ \(MkPinaforeLensMorphism g bu p i) -> let
            g' = (cfmap1 f) g
            p' = (cfmap1 $ endocfmap f) p
            in MkPinaforeLensMorphism g' bu p' i

pmKGet :: PinaforeLensMorphism baseupdate ap aq bp bq -> Know ap -> ReadM (UpdateReader baseupdate) (Know bq)
pmKGet plm (Known a) = pmGet plm a
pmKGet _ Unknown = return Unknown

pmGetPointPreimage ::
       Eq aq => PinaforeLensMorphism baseupdate ap aq bp bq -> bp -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
pmGetPointPreimage MkPinaforeLensMorphism {..} b = fmap setFromList $ pmInv b

pmGetKnowPointPreimage ::
       Eq aq => PinaforeLensMorphism baseupdate ap aq bp bq -> Know bp -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
pmGetKnowPointPreimage plm (Known b) = pmGetPointPreimage plm b
pmGetKnowPointPreimage _ Unknown = return mempty

pmGetSetPreimage ::
       Eq aq
    => PinaforeLensMorphism baseupdate ap aq bp bq
    -> FiniteSet bp
    -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
pmGetSetPreimage plm bs = do
    as <- for bs $ pmGetPointPreimage plm
    return $ mconcat $ toList as

identityPinaforeLensMorphism :: PinaforeLensMorphism baseupdate x y y x
identityPinaforeLensMorphism = let
    pmGet a = return $ Known a
    pmBaseUpdate _ = return False
    pmPut _ kb = return $ Just ([], Just kb)
    pmInv b = return $ pure b
    in MkPinaforeLensMorphism {..}

composePinaforeLensMorphism ::
       PinaforeLensMorphism baseupdate bx by cp cq
    -> PinaforeLensMorphism baseupdate ap aq by bx
    -> PinaforeLensMorphism baseupdate ap aq cp cq
composePinaforeLensMorphism (MkPinaforeLensMorphism getBC buBC putBC invBC) ab@(MkPinaforeLensMorphism getAB buAB putAB invAB) = let
    pmGet a =
        getComposeM $ do
            b <- MkComposeM $ getAB a
            MkComposeM $ getBC b
    pmBaseUpdate update = do
        ub <- buAB update
        if ub
            then return True
            else buBC update
    pmPut koldA kC =
        getComposeM $ do
            koldB <- liftOuter $ pmKGet ab koldA
            (edits1, mknewB) <- MkComposeM $ putBC koldB kC
            case mknewB of
                Nothing -> return (edits1, Nothing)
                Just knewB -> do
                    (edits2, mknewA) <- MkComposeM $ putAB koldA knewB
                    return (edits1 <> edits2, mknewA)
    pmInv c = do
        bb <- invBC c
        aaa <- for bb invAB
        return $ mconcat aaa
    in MkPinaforeLensMorphism {..}

pairPinaforeLensMorphism ::
       forall baseupdate ap aq bp bq cp cq. Eq aq
    => PinaforeLensMorphism baseupdate ap aq bp bq
    -> PinaforeLensMorphism baseupdate ap aq cp cq
    -> PinaforeLensMorphism baseupdate ap aq (bp, cp) (bq, cq)
pairPinaforeLensMorphism (MkPinaforeLensMorphism getB buB putB invB) (MkPinaforeLensMorphism getC buC putC invC) = let
    pmGet a =
        getComposeM $ do
            b <- MkComposeM $ getB a
            c <- MkComposeM $ getC a
            return (b, c)
    pmBaseUpdate update = do
        ub <- buB update
        if ub
            then return True
            else buC update
    pmPut _ Unknown = return Nothing -- can't delete
    pmPut ka (Known (b, c)) =
        getComposeM $ do
            (updb, bmka) <- MkComposeM $ putB ka $ Known b
            (updc, cmka) <- MkComposeM $ putC ka $ Known c
            return (updb <> updc, bmka <|> cmka)
    pmInv (b, c) = do
        ba <- invB b
        ca <- invC c
        return $ unFiniteSet $ intersection (MkFiniteSet ba) (MkFiniteSet ca)
    in MkPinaforeLensMorphism {..}

eitherPinaforeLensMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       PinaforeLensMorphism baseupdate ap aq cp cq
    -> PinaforeLensMorphism baseupdate bp bq cp cq
    -> PinaforeLensMorphism baseupdate (Either ap bp) (Either aq bq) cp cq
eitherPinaforeLensMorphism (MkPinaforeLensMorphism getA buA putA invA) (MkPinaforeLensMorphism getB buB putB invB) = let
    pmGet (Left a) = getA a
    pmGet (Right b) = getB b
    pmBaseUpdate update = do
        ub <- buA update
        if ub
            then return True
            else buB update
    pmPut (Known (Left a)) kc =
        getComposeM $ do
            (bu, mka) <- MkComposeM $ putA (Known a) kc
            return $ (bu, fmap (fmap Left) mka)
    pmPut (Known (Right b)) kc =
        getComposeM $ do
            (bu, mkb) <- MkComposeM $ putB (Known b) kc
            return $ (bu, fmap (fmap Right) mkb)
    pmPut Unknown _ = return Nothing
    pmInv c = do
        aa <- invA c
        bb <- invB c
        return $ fmap Left aa <> fmap Right bb
    in MkPinaforeLensMorphism {..}

shortOr :: Monad m => (a -> m Bool) -> [a] -> m Bool
shortOr _ [] = return False
shortOr amb (a:aa) = do
    b <- amb a
    if b
        then return True
        else shortOr amb aa

mapPinaforeLensMorphismBase ::
       forall baseA baseB ap aq bp bq.
       ChangeLens baseB baseA
    -> PinaforeLensMorphism baseA ap aq bp bq
    -> PinaforeLensMorphism baseB ap aq bp bq
mapPinaforeLensMorphismBase MkChangeLens {..} (MkPinaforeLensMorphism getA buA putA invA) = let
    mapRead :: forall t. ReadM (UpdateReader baseA) t -> ReadM (UpdateReader baseB) t
    mapRead = mapReadM clRead
    pmGet a = mapRead $ getA a
    pmPut ka kb =
        getComposeM $ do
            (editsA, mka) <- MkComposeM $ mapRead $ putA ka kb
            editsB <- MkComposeM $ MkReadM $ clPutEdits editsA
            return $ (editsB, mka)
    pmBaseUpdate updateB = do
        updateAs <- MkReadM $ clUpdate updateB
        mapRead $ shortOr buA updateAs
    pmInv b = mapRead $ invA b
    in MkPinaforeLensMorphism {..}

funcPinaforeLensMorphism ::
       forall baseupdate ap aq bp bq.
       (ap -> Know bq)
    -> (bp -> [aq])
    -> (Know bp -> Maybe (Know aq))
    -> PinaforeLensMorphism baseupdate ap aq bp bq
funcPinaforeLensMorphism ab bsa bma = let
    pmGet :: ap -> ReadM (UpdateReader baseupdate) (Know bq)
    pmGet a = return $ ab a
    pmBaseUpdate :: baseupdate -> ReadM (UpdateReader baseupdate) Bool
    pmBaseUpdate _ = return False
    pmPut :: Know ap -> Know bp -> ReadM (UpdateReader baseupdate) (Maybe ([UpdateEdit baseupdate], Maybe (Know aq)))
    pmPut _ kb =
        return $ do
            ka <- bma kb
            return ([], Just ka)
    pmInv :: bp -> ReadM (UpdateReader baseupdate) [aq] -- not guaranteed to be unique
    pmInv b = return $ bsa b
    in MkPinaforeLensMorphism {..}

nullPinaforeLensMorphism :: forall baseupdate ap aq bp bq. PinaforeLensMorphism baseupdate ap aq bp bq
nullPinaforeLensMorphism = funcPinaforeLensMorphism (\_ -> Unknown) (\_ -> mempty) (\_ -> Nothing)

bijectionPinaforeLensMorphism :: Bijection a b -> PinaforeLensMorphism baseupdate a a b b
bijectionPinaforeLensMorphism (MkIsomorphism ab ba) =
    funcPinaforeLensMorphism (Known . ab) (\b -> opoint $ ba b) (\kb -> Just $ fmap ba kb)

{-
instance IsoVariant (PinaforeLensMorphism baseupdate t) where
    isoMap ab ba m = bijectionPinaforeLensMorphism (MkIsomorphism ab ba) . m

instance IsoVariant' (PinaforeLensMorphism baseupdate) where
    isoMap' ab ba m = m . bijectionPinaforeLensMorphism (MkIsomorphism ba ab)
-}
lensFunctionMorphism ::
       forall baseupdate ap aq bp bq.
       PinaforeLensMorphism baseupdate ap aq bp bq
    -> PinaforeFunctionMorphism baseupdate (Know ap) (Know bq)
lensFunctionMorphism plm = let
    pfFuncRead = pmKGet plm
    pfUpdate = pmBaseUpdate plm
    in MkPinaforeFunctionMorphism {..}

lensInverseFunctionMorphism ::
       PinaforeLensMorphism baseupdate ap aq bp bq -> PinaforeFunctionMorphism baseupdate bp [aq]
lensInverseFunctionMorphism MkPinaforeLensMorphism {..} = let
    pfFuncRead = pmInv
    pfUpdate = pmBaseUpdate
    in MkPinaforeFunctionMorphism {..}

runContextReadM ::
       forall m baseupdate update t. MonadIO m
    => Readable m (ContextUpdateReader baseupdate update)
    -> ReadM (UpdateReader baseupdate) t
    -> m t
runContextReadM rd rmt = unReadM rmt $ tupleReadFunction SelectContext rd

putEditBA ::
       forall m baseupdate ap aq bp bq. MonadIO m
    => PinaforeLensMorphism baseupdate ap aq bp bq
    -> [BiWholeEdit (Know bp) (Know bq)]
    -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap)))
    -> m (Maybe [ContextUpdateEdit baseupdate (BiWholeUpdate (Know aq) (Know ap))])
putEditBA plm editsB mr =
    case lastM editsB of
        Nothing -> return $ Just []
        Just (MkBiWholeEdit kb) -> do
            ka <- mr $ MkTupleUpdateReader SelectContent ReadWhole
            medits <- runContextReadM mr $ pmPut plm ka kb
            let
                convertEdits ::
                       forall .
                       ([UpdateEdit baseupdate], Maybe (Know aq))
                    -> [ContextUpdateEdit baseupdate (BiWholeUpdate (Know aq) (Know ap))]
                convertEdits (pinedits, mka) =
                    fmap (MkTupleUpdateEdit SelectContext) pinedits <>
                    case mka of
                        Nothing -> []
                        Just knewa -> [MkTupleUpdateEdit SelectContent $ MkBiWholeEdit knewa]
            return $ fmap convertEdits medits

putEditAB ::
       forall m baseupdate ap aq bp bq. MonadIO m
    => PinaforeLensMorphism baseupdate ap aq bp bq
    -> ap
    -> Know bp
    -> Readable m (UpdateReader baseupdate)
    -> m (Maybe [UpdateEdit baseupdate])
putEditAB plm a kb mr = do
    medits <-
        putEditBA @m plm [MkBiWholeEdit kb] $ \case
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

pinaforeLensMorphismChangeLens ::
       forall baseupdate ap aq bp bq.
       PinaforeLensMorphism baseupdate ap aq bp bq
    -> ChangeLens (ContextUpdate baseupdate (BiWholeUpdate (Know aq) (Know ap))) (BiWholeUpdate (Know bp) (Know bq))
pinaforeLensMorphismChangeLens plm = let
    clRead ::
           forall .
           ReadFunction (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap))) (WholeReader (Know bq))
    clRead mr ReadWhole = do
        ka <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        runContextReadM mr $ pmKGet plm ka
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (BiWholeUpdate (Know aq) (Know ap))
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap)))
        -> m [BiWholeUpdate (Know bp) (Know bq)]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- runContextReadM mr $ pmBaseUpdate plm pinupdate
        if ch
            then do
                ka <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                kb <- runContextReadM mr $ pmKGet plm ka
                return $ pure $ MkBiWholeUpdate kb
            else return []
    clUpdate (MkTupleUpdate SelectContent (MkBiWholeUpdate ka)) mr = do
        kb <- runContextReadM mr $ pmKGet plm ka
        return $ pure $ MkBiWholeUpdate kb
    clPutEdits ::
           forall m. MonadIO m
        => [BiWholeEdit (Know bp) (Know bq)]
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap)))
        -> m (Maybe [ContextUpdateEdit baseupdate (BiWholeUpdate (Know aq) (Know ap))])
    clPutEdits = putEditBA plm
    in MkChangeLens {..}

pinaforeLensMorphismInverseChangeLens ::
       forall baseupdate a bp bq. Eq a
    => PinaforeLensMorphism baseupdate a a bq bp
    -> ChangeLens (ContextUpdate baseupdate (BiWholeUpdate (Know bp) (Know bq))) (FiniteSetUpdate a)
pinaforeLensMorphismInverseChangeLens plm@MkPinaforeLensMorphism {..} = let
    fsetReadFunction ::
           ReadFunction (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq))) (WholeReader (FiniteSet a))
    fsetReadFunction (mr :: Readable m _) ReadWhole = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        runContextReadM mr $ pmGetKnowPointPreimage plm kb
    clRead :: ReadFunction (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq))) (FiniteSetReader a)
    clRead mr rt = wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (BiWholeUpdate (Know bp) (Know bq))
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq)))
        -> m [FiniteSetUpdate a]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        ch <- runContextReadM mr $ pmBaseUpdate pinupdate
        if ch
            then do
                kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                aset <- runContextReadM mr $ pmGetKnowPointPreimage plm kb
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
            else return []
    clUpdate (MkTupleUpdate SelectContent (MkBiWholeUpdate kb)) mr = do
        aset <- runContextReadM mr $ pmGetKnowPointPreimage plm kb
        aedits <- getReplaceEditsFromSubject aset
        return $ fmap editUpdate aedits
    putEdit ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq)))
        -> m (Maybe [UpdateEdit baseupdate])
    putEdit (KeyEditItem _ update) _ = never update
    putEdit (KeyEditDelete a) mr = putEditAB plm a Unknown $ tupleReadFunction SelectContext mr
    putEdit (KeyEditInsertReplace a) mr = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        putEditAB plm a kb $ tupleReadFunction SelectContext mr
    putEdit KeyEditClear mr = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        aa <- runContextReadM mr $ pmGetKnowPointPreimage plm kb
        lmpedits <- for (toList aa) $ \a -> putEditAB plm a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap mconcat $ sequenceA lmpedits
    clPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq)))
        -> m (Maybe [ContextUpdateEdit baseupdate (BiWholeUpdate (Know bp) (Know bq))])
    clPutEdits fsedits mr =
        getComposeM $ do
            baseedits <- for fsedits $ \fsedit -> MkComposeM $ putEdit fsedit mr
            return $ fmap (MkTupleUpdateEdit SelectContext) $ mconcat baseedits
    in MkChangeLens {..}

pinaforeLensMorphismInverseChangeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a a b b
    -> ChangeLens (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
pinaforeLensMorphismInverseChangeLensSet newb plm@MkPinaforeLensMorphism {..} = let
    clRead' :: ReadFunction (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (FiniteSetReader a)
    clRead' (mr :: Readable m _) KeyReadKeys = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        runContextReadM mr $ pmGetSetPreimage plm bs
    clRead' (mr :: Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))) (KeyReadItem a ReadWhole) = do
        kb <- runContextReadM mr $ pmGet a
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
        ch <- runContextReadM mr $ pmBaseUpdate pinaedit
        if ch
            then do
                bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                aset <- runContextReadM mr $ pmGetSetPreimage plm bs
                aedits <- getReplaceEditsFromSubject aset
                return $ fmap editUpdate aedits
            else return []
    clUpdate' (MkTupleUpdate SelectContent (KeyUpdateItem _ update)) _ = never update
    clUpdate' (MkTupleUpdate SelectContent KeyUpdateClear) _ = return [KeyUpdateClear]
    clUpdate' (MkTupleUpdate SelectContent (KeyUpdateInsertReplace _)) _ = return []
    clUpdate' (MkTupleUpdate SelectContent (KeyUpdateDelete b)) mr = do
        aset <- runContextReadM mr $ pmGetPointPreimage plm b
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
    elPutEdit' ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    elPutEdit' (KeyEditItem _ update) _ = never update
    elPutEdit' (KeyEditDelete a) mr = do
        mpedits <- putEditAB plm a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    elPutEdit' (KeyEditInsertReplace a) mr = do
        b <- liftIO newb
        getComposeM $ do
            pedits <- MkComposeM $ putEditAB plm a (Known b) $ tupleReadFunction SelectContext mr
            return $
                (MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace b) :
                fmap (MkTupleUpdateEdit SelectContext) pedits
    elPutEdit' KeyEditClear mr = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        getComposeM $ do
            lpedits <-
                for (toList bs) $ \b -> do
                    aa <- lift $ runContextReadM mr $ pmInv b
                    lpedits <-
                        for (toList aa) $ \a ->
                            MkComposeM $ putEditAB plm a Unknown $ tupleReadFunction SelectContext mr
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
