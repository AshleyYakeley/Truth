module Pinafore.Base.Morphism
    ( PinaforeLensMorphism(..)
    , identityPinaforeLensMorphism
    , composePinaforeLensMorphism
    , funcPinaforeLensMorphism
    , nullPinaforeLensMorphism
    , bijectionPinaforeLensMorphism
    , pairPinaforeLensMorphism
    , eitherPinaforeLensMorphism
    , lensFunctionMorphism
    , pinaforeLensMorphismChangeLens
    , pinaforeLensMorphismInverseChangeLens
    , pinaforeLensMorphismInverseChangeLensSet
    ) where

import qualified Data.List as List
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.Know
import Shapes
import Truth.Core

data PinaforeLensMorphism baseupdate ap aq bp bq = MkPinaforeLensMorphism
    { pmGet :: ap -> ReadM (UpdateReader baseupdate) (Know bq)
    , pmBaseUpdate :: baseupdate -> Maybe (ap -> ReadM (UpdateReader baseupdate) (Maybe (Know bq)))
    , pmPut :: Know ap -> Know bp -> ReadM (UpdateReader baseupdate) (Maybe ([UpdateEdit baseupdate], Maybe (Know aq)))
    , pmInvGet :: bp -> ReadM (UpdateReader baseupdate) [aq] -- not guaranteed to be unique
    , pmInvBaseUpdate :: baseupdate -> Maybe (bp -> ReadM (UpdateReader baseupdate) [(Bool, aq)])
    }

instance Functor (PinaforeLensMorphism baseupdate ap aq bp) where
    fmap f (MkPinaforeLensMorphism g bu p i ibu) = let
        g' = (fmap $ fmap $ fmap f) g
        bu' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap f) bu
        in MkPinaforeLensMorphism g' bu' p i ibu

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (PinaforeLensMorphism baseupdate ap aq) where
    cfmap f =
        MkNestedMorphism $ \(MkPinaforeLensMorphism g bu p i ibu) -> let
            p' = (fmap $ cfmap1 $ endocfmap f) p
            i' = (cfmap1 f) i
            ibu' = (fmap $ fmap $ cfmap1 f) ibu
            in MkPinaforeLensMorphism g bu p' i' ibu'

instance CatFunctor (->) (NestedMorphism (->)) (PinaforeLensMorphism baseupdate ap) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkPinaforeLensMorphism g bu p i ibu) -> let
            p' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap $ fmap f) p
            i' = (fmap $ fmap $ fmap f) i
            ibu' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap f) ibu
            in MkPinaforeLensMorphism g bu p' i' ibu'

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (PinaforeLensMorphism baseupdate) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $ \(MkPinaforeLensMorphism g bu p i ibu) -> let
            g' = (cfmap1 f) g
            bu' = (fmap $ fmap $ cfmap1 f) bu
            p' = (cfmap1 $ endocfmap f) p
            in MkPinaforeLensMorphism g' bu' p' i ibu

pmKGet :: PinaforeLensMorphism baseupdate ap aq bp bq -> Know ap -> ReadM (UpdateReader baseupdate) (Know bq)
pmKGet plm (Known a) = pmGet plm a
pmKGet _ Unknown = return Unknown

pmGetPointPreimage ::
       Eq aq => PinaforeLensMorphism baseupdate ap aq bp bq -> bp -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
pmGetPointPreimage MkPinaforeLensMorphism {..} b = fmap setFromList $ pmInvGet b

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

identityPinaforeLensMorphism :: forall baseupdate x y. PinaforeLensMorphism baseupdate x y y x
identityPinaforeLensMorphism = let
    pmGet a = return $ Known a
    pmBaseUpdate :: baseupdate -> Maybe (x -> ReadM (UpdateReader baseupdate) (Maybe (Know x)))
    pmBaseUpdate _ = Nothing
    pmPut _ kb = return $ Just ([], Just kb)
    pmInvGet b = return $ pure b
    pmInvBaseUpdate :: baseupdate -> Maybe (y -> ReadM (UpdateReader baseupdate) [(Bool, y)])
    pmInvBaseUpdate _ = Nothing
    in MkPinaforeLensMorphism {..}

composePinaforeLensMorphism ::
       forall baseupdate ap aq bx by cp cq.
       PinaforeLensMorphism baseupdate bx by cp cq
    -> PinaforeLensMorphism baseupdate ap aq by bx
    -> PinaforeLensMorphism baseupdate ap aq cp cq
composePinaforeLensMorphism (MkPinaforeLensMorphism getBC buBC putBC invBC invbuBC) ab@(MkPinaforeLensMorphism getAB buAB putAB invAB invbuAB) = let
    pmGet a =
        getComposeM $ do
            b <- MkComposeM $ getAB a
            MkComposeM $ getBC b
    pmBaseUpdate :: baseupdate -> Maybe (ap -> ReadM (UpdateReader baseupdate) (Maybe (Know cq)))
    pmBaseUpdate update =
        case (buAB update, buBC update) of
            (Nothing, Nothing) -> Nothing
            _ ->
                Just $ \a -> do
                    mkb <-
                        case buAB update of
                            Nothing -> return Nothing
                            Just amkb -> amkb a
                    case mkb of
                        Nothing ->
                            case buBC update of
                                Nothing -> return Nothing
                                Just bmkc -> do
                                    kb <- getAB a
                                    case kb of
                                        Unknown -> return Nothing
                                        Known b -> bmkc b
                        Just kb -> do
                            kkc <- for kb getBC
                            return $ Just $ exec kkc
    pmPut koldA kC =
        getComposeM $ do
            koldB <- liftOuter $ pmKGet ab koldA
            (edits1, mknewB) <- MkComposeM $ putBC koldB kC
            case mknewB of
                Nothing -> return (edits1, Nothing)
                Just knewB -> do
                    (edits2, mknewA) <- MkComposeM $ putAB koldA knewB
                    return (edits1 <> edits2, mknewA)
    pmInvGet c = do
        bb <- invBC c
        aaa <- for bb invAB
        return $ mconcat aaa
    pmInvBaseUpdate :: baseupdate -> Maybe (cp -> ReadM (UpdateReader baseupdate) [(Bool, aq)])
    pmInvBaseUpdate update =
        case (invbuBC update, invbuAB update) of
            (Nothing, Nothing) -> Nothing
            _ ->
                Just $ \c -> do
                    lbb <-
                        case invbuBC update of
                            Nothing -> return []
                            Just crlbb -> crlbb c
                    aa1 <-
                        for lbb $ \(t, b) -> do
                            aa <- invAB b
                            return $ fmap (\a -> (t, a)) aa
                    bb <- invBC c
                    aa2 <-
                        for bb $ \b -> do
                            case invbuAB update of
                                Nothing -> return []
                                Just brlba -> brlba b
                    return $ mconcat aa1 <> mconcat aa2
    in MkPinaforeLensMorphism {..}

partitionBPairs :: forall a. [(Bool, a)] -> ([a], [a]) -- False, True
partitionBPairs [] = ([], [])
partitionBPairs ((t, a):aa) = let
    (af, at) = partitionBPairs aa
    in if t
           then (af, a : at)
           else (a : af, at)

pairPinaforeLensMorphism ::
       forall baseupdate ap aq bp bq cp cq. Eq aq
    => PinaforeLensMorphism baseupdate ap aq bp bq
    -> PinaforeLensMorphism baseupdate ap aq cp cq
    -> PinaforeLensMorphism baseupdate ap aq (bp, cp) (bq, cq)
pairPinaforeLensMorphism (MkPinaforeLensMorphism getB buB putB invB invbuB) (MkPinaforeLensMorphism getC buC putC invC invbuC) = let
    pmGet a =
        getComposeM $ do
            b <- MkComposeM $ getB a
            c <- MkComposeM $ getC a
            return (b, c)
    pmBaseUpdate :: baseupdate -> Maybe (ap -> ReadM (UpdateReader baseupdate) (Maybe (Know (bq, cq))))
    pmBaseUpdate update =
        case (buB update, buC update) of
            (Nothing, Nothing) -> Nothing
            _ ->
                Just $ \a -> do
                    mkb <-
                        case buB update of
                            Just armkb -> armkb a
                            Nothing -> return Nothing
                    mkc <-
                        case buC update of
                            Just armkc -> armkc a
                            Nothing -> return Nothing
                    case (mkb, mkc) of
                        (Nothing, Nothing) -> return Nothing
                        _ -> do
                            kb <-
                                case mkb of
                                    Just kb -> return kb
                                    Nothing -> getB a
                            kc <-
                                case mkc of
                                    Just kc -> return kc
                                    Nothing -> getC a
                            return $
                                Just $ do
                                    b <- kb
                                    c <- kc
                                    return (b, c)
    pmPut _ Unknown = return Nothing -- can't delete
    pmPut ka (Known (b, c)) =
        getComposeM $ do
            (updb, bmka) <- MkComposeM $ putB ka $ Known b
            (updc, cmka) <- MkComposeM $ putC ka $ Known c
            return (updb <> updc, bmka <|> cmka)
    pmInvGet (b, c) = do
        ba <- invB b
        ca <- invC c
        return $ unFiniteSet $ intersection (MkFiniteSet ba) (MkFiniteSet ca)
    pmInvBaseUpdate :: baseupdate -> Maybe ((bp, cp) -> ReadM (UpdateReader baseupdate) [(Bool, aq)])
    pmInvBaseUpdate update =
        case (invbuB update, invbuC update) of
            (Nothing, Nothing) -> Nothing
            _ ->
                Just $ \(b, c) -> do
                    lba1 <-
                        case invbuB update of
                            Nothing -> return []
                            Just brlba -> brlba b
                    lba2 <-
                        case invbuC update of
                            Nothing -> return []
                            Just crlba -> crlba c
                    let
                        (a1f, a1t) = partitionBPairs lba1
                        (a2f, a2t) = partitionBPairs lba2
                        removals = fmap (\a -> (False, a)) $ a1f <> a2f
                    a1tt <-
                        case a1t of
                            [] -> return []
                            _ -> do
                                aa2 <- invC c
                                return $ List.intersect a1t aa2
                    a2tt <-
                        case a2t of
                            [] -> return []
                            _ -> do
                                aa1 <- invB b
                                return $ List.intersect a2t aa1
                    let additions = fmap (\a -> (True, a)) $ a1tt <> a2tt
                    return $ additions <> removals
    in MkPinaforeLensMorphism {..}

eitherPinaforeLensMorphism ::
       forall baseupdate ap aq bp bq cp cq.
       PinaforeLensMorphism baseupdate ap aq cp cq
    -> PinaforeLensMorphism baseupdate bp bq cp cq
    -> PinaforeLensMorphism baseupdate (Either ap bp) (Either aq bq) cp cq
eitherPinaforeLensMorphism (MkPinaforeLensMorphism getA buA putA invA invbuA) (MkPinaforeLensMorphism getB buB putB invB invbuB) = let
    pmGet (Left a) = getA a
    pmGet (Right b) = getB b
    pmBaseUpdate :: baseupdate -> Maybe (Either ap bp -> ReadM (UpdateReader baseupdate) (Maybe (Know cq)))
    pmBaseUpdate update =
        case (buA update, buB update) of
            (Nothing, Nothing) -> Nothing
            _ ->
                Just $ \case
                    Left a ->
                        case buA update of
                            Nothing -> return Nothing
                            Just armkc -> armkc a
                    Right b ->
                        case buB update of
                            Nothing -> return Nothing
                            Just brmkc -> brmkc b
    pmPut (Known (Left a)) kc =
        getComposeM $ do
            (bu, mka) <- MkComposeM $ putA (Known a) kc
            return $ (bu, fmap (fmap Left) mka)
    pmPut (Known (Right b)) kc =
        getComposeM $ do
            (bu, mkb) <- MkComposeM $ putB (Known b) kc
            return $ (bu, fmap (fmap Right) mkb)
    pmPut Unknown _ = return Nothing
    pmInvGet c = do
        aa <- invA c
        bb <- invB c
        return $ fmap Left aa <> fmap Right bb
    pmInvBaseUpdate :: baseupdate -> Maybe (cp -> ReadM (UpdateReader baseupdate) [(Bool, Either aq bq)])
    pmInvBaseUpdate update =
        case (invbuA update, invbuB update) of
            (Nothing, Nothing) -> Nothing
            _ ->
                Just $ \c -> do
                    tas <-
                        case invbuA update of
                            Nothing -> return []
                            Just crlba -> crlba c
                    tbs <-
                        case invbuB update of
                            Nothing -> return []
                            Just crlbb -> crlbb c
                    return $ fmap (fmap Left) tas <> fmap (fmap Right) tbs
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
    pmBaseUpdate :: baseupdate -> Maybe (ap -> ReadM (UpdateReader baseupdate) (Maybe (Know bq)))
    pmBaseUpdate _ = Nothing
    pmPut :: Know ap -> Know bp -> ReadM (UpdateReader baseupdate) (Maybe ([UpdateEdit baseupdate], Maybe (Know aq)))
    pmPut _ kb =
        return $ do
            ka <- bma kb
            return ([], Just ka)
    pmInvGet :: bp -> ReadM (UpdateReader baseupdate) [aq] -- not guaranteed to be unique
    pmInvGet b = return $ bsa b
    pmInvBaseUpdate :: baseupdate -> Maybe (bp -> ReadM (UpdateReader baseupdate) [(Bool, aq)])
    pmInvBaseUpdate _ = Nothing
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
    convUpdate armkb (Known a) = armkb a
    convUpdate _ Unknown = return $ Just Unknown
    pfUpdate baseupdate = fmap convUpdate $ pmBaseUpdate plm baseupdate
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
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr =
        case pmBaseUpdate plm pinupdate of
            Nothing -> return []
            Just armkb -> do
                ka <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                case ka of
                    Known a -> do
                        mkb <- runContextReadM mr $ armkb a
                        case mkb of
                            Nothing -> return []
                            Just kb -> return $ pure $ MkBiWholeUpdate kb
                    Unknown -> return []
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

bpairToFiniteSetUpdate :: forall a. (Bool, a) -> FiniteSetUpdate a
bpairToFiniteSetUpdate (False, a) = KeyUpdateDelete a
bpairToFiniteSetUpdate (True, a) = KeyUpdateInsertReplace a

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
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr =
        case pmInvBaseUpdate pinupdate of
            Nothing -> return []
            Just brlba -> do
                kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                case kb of
                    Unknown -> return []
                    Known b -> do
                        lba <- runContextReadM mr $ brlba b
                        return $ fmap bpairToFiniteSetUpdate lba
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
    clUpdate' (MkTupleUpdate SelectContext pinupdate) mr =
        case pmInvBaseUpdate pinupdate of
            Nothing -> return []
            Just brlba -> do
                bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
                updatesfs <-
                    for bs $ \b -> do
                        lba <- runContextReadM mr $ brlba b
                        return $ fmap bpairToFiniteSetUpdate lba
                return $ mconcat $ toList updatesfs
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
                    aa <- lift $ runContextReadM mr $ pmInvGet b
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
