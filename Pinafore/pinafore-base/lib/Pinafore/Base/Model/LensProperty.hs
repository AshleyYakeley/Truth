module Pinafore.Base.Model.LensProperty
    ( StorageLensProperty(..)
    , identityStorageLensProperty
    , composeStorageLensProperty
    , funcStorageLensProperty
    , nullStorageLensProperty
    , bijectionStorageLensProperty
    , pairStorageLensProperty
    , eitherStorageLensProperty
    , lensFunctionAttribute
    , storageLensAttributeChangeLens
    , storageLensPropertyInverseChangeLens
    , storageLensPropertyInverseChangeLensSet
    ) where

import Changes.Core
import Data.List qualified as List
import Pinafore.Base.Know
import Pinafore.Base.Model.LensAttribute
import Shapes

data StorageLensProperty ap aq bp bq baseupdate = MkStorageLensProperty
    { slpAttribute :: StorageLensAttribute ap aq bp bq baseupdate
    , slpInvGet :: bp -> ReadM (UpdateReader baseupdate) [aq] -- not guaranteed to be unique
    , slpInvBaseUpdate :: baseupdate -> ReadM (UpdateReader baseupdate) (Maybe (bp -> ReadM (UpdateReader baseupdate) [( Bool
                                                                                                                       , aq)]))
    }

instance CatFunctor (->) (NestedMorphism (->)) (StorageLensProperty ap aq bp) where
    cfmap f =
        MkNestedMorphism $ \(MkStorageLensProperty attr i ibu) -> let
            attr' = (unNestedMorphism $ cfmap f) attr
            in MkStorageLensProperty attr' i ibu

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (StorageLensProperty ap aq) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkStorageLensProperty attr i ibu) -> let
            attr' = (unNestedMorphism $ unNestedMorphism $ cfmap f) attr
            i' = (cfmap1 f) i
            ibu' = (fmap $ fmap $ fmap $ cfmap1 f) ibu
            in MkStorageLensProperty attr' i' ibu'

instance CatFunctor (->) (NestedMorphism (->)) (StorageLensProperty ap) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $ \(MkStorageLensProperty attr i ibu) -> let
            attr' = (unNestedMorphism $ unNestedMorphism $ unNestedMorphism $ cfmap f) attr
            i' = (fmap $ fmap $ fmap f) i
            ibu' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap $ fmap f) ibu
            in MkStorageLensProperty attr' i' ibu'

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (StorageLensProperty) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $ \(MkStorageLensProperty attr i ibu) -> let
            attr' = (unNestedMorphism $ unNestedMorphism $ unNestedMorphism $ unNestedMorphism $ cfmap f) attr
            in MkStorageLensProperty attr' i ibu

slpGetPointPreimage ::
       Eq aq => StorageLensProperty ap aq bp bq baseupdate -> bp -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
slpGetPointPreimage MkStorageLensProperty {..} b = fmap setFromList $ slpInvGet b

slpGetKnowPointPreimage ::
       Eq aq => StorageLensProperty ap aq bp bq baseupdate -> Know bp -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
slpGetKnowPointPreimage plm (Known b) = slpGetPointPreimage plm b
slpGetKnowPointPreimage _ Unknown = return mempty

slpGetSetPreimage ::
       Eq aq
    => StorageLensProperty ap aq bp bq baseupdate
    -> FiniteSet bp
    -> ReadM (UpdateReader baseupdate) (FiniteSet aq)
slpGetSetPreimage plm bs = do
    as <- for bs $ slpGetPointPreimage plm
    return $ mconcat $ toList as

identityStorageLensProperty :: forall baseupdate x y. StorageLensProperty x y y x baseupdate
identityStorageLensProperty = let
    slpAttribute = identityStorageLensAttribute
    slpInvGet b = return $ pure b
    slpInvBaseUpdate ::
           baseupdate -> ReadM (UpdateReader baseupdate) (Maybe (y -> ReadM (UpdateReader baseupdate) [(Bool, y)]))
    slpInvBaseUpdate _ = return Nothing
    in MkStorageLensProperty {..}

composeStorageLensProperty ::
       forall baseupdate ap aq bx by cp cq.
       StorageLensProperty bx by cp cq baseupdate
    -> StorageLensProperty ap aq by bx baseupdate
    -> StorageLensProperty ap aq cp cq baseupdate
composeStorageLensProperty (MkStorageLensProperty attrBC invBC invbuBC) (MkStorageLensProperty attrAB invAB invbuAB) = let
    slpAttribute = composeStorageLensAttribute attrBC attrAB
    slpInvGet c = do
        bb <- invBC c
        aaa <- for bb invAB
        return $ mconcat aaa
    slpInvBaseUpdate ::
           baseupdate -> ReadM (UpdateReader baseupdate) (Maybe (cp -> ReadM (UpdateReader baseupdate) [(Bool, aq)]))
    slpInvBaseUpdate update = do
        mfBC <- invbuBC update
        mfAB <- invbuAB update
        return $
            case (mfBC, mfAB) of
                (Nothing, Nothing) -> Nothing
                _ ->
                    Just $ \c -> do
                        lbb <-
                            case mfBC of
                                Nothing -> return []
                                Just crlbb -> crlbb c
                        aa1 <-
                            for lbb $ \(t, b) -> do
                                aa <- invAB b
                                return $ fmap (\a -> (t, a)) aa
                        bb <- invBC c
                        aa2 <-
                            for bb $ \b -> do
                                case mfAB of
                                    Nothing -> return []
                                    Just brlba -> brlba b
                        return $ mconcat aa1 <> mconcat aa2
    in MkStorageLensProperty {..}

partitionBPairs :: forall a. [(Bool, a)] -> ([a], [a]) -- False, True
partitionBPairs [] = ([], [])
partitionBPairs ((t, a):aa) = let
    (af, at) = partitionBPairs aa
    in if t
           then (af, a : at)
           else (a : af, at)

pairStorageLensProperty ::
       forall baseupdate ap aq bp bq cp cq. Eq aq
    => StorageLensProperty ap aq bp bq baseupdate
    -> StorageLensProperty ap aq cp cq baseupdate
    -> StorageLensProperty ap aq (bp, cp) (bq, cq) baseupdate
pairStorageLensProperty (MkStorageLensProperty attrB invB invbuB) (MkStorageLensProperty attrC invC invbuC) = let
    slpAttribute = pairStorageLensAttribute attrB attrC
    slpInvGet (b, c) = do
        ba <- invB b
        ca <- invC c
        return $ unFiniteSet $ intersection (MkFiniteSet ba) (MkFiniteSet ca)
    slpInvBaseUpdate ::
           baseupdate
        -> ReadM (UpdateReader baseupdate) (Maybe ((bp, cp) -> ReadM (UpdateReader baseupdate) [(Bool, aq)]))
    slpInvBaseUpdate update = do
        mfB <- invbuB update
        mfC <- invbuC update
        return $
            case (mfB, mfC) of
                (Nothing, Nothing) -> Nothing
                _ ->
                    Just $ \(b, c) -> do
                        lba1 <-
                            case mfB of
                                Nothing -> return []
                                Just brlba -> brlba b
                        lba2 <-
                            case mfC of
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
    in MkStorageLensProperty {..}

eitherStorageLensProperty ::
       forall baseupdate ap aq bp bq cp cq.
       StorageLensProperty ap aq cp cq baseupdate
    -> StorageLensProperty bp bq cp cq baseupdate
    -> StorageLensProperty (Either ap bp) (Either aq bq) cp cq baseupdate
eitherStorageLensProperty (MkStorageLensProperty attrA invA invbuA) (MkStorageLensProperty attrB invB invbuB) = let
    slpAttribute = eitherStorageLensAttribute attrA attrB
    slpInvGet c = do
        aa <- invA c
        bb <- invB c
        return $ fmap Left aa <> fmap Right bb
    slpInvBaseUpdate ::
           baseupdate
        -> ReadM (UpdateReader baseupdate) (Maybe (cp -> ReadM (UpdateReader baseupdate) [(Bool, Either aq bq)]))
    slpInvBaseUpdate update = do
        mfA <- invbuA update
        mfB <- invbuB update
        return $
            case (mfA, mfB) of
                (Nothing, Nothing) -> Nothing
                _ ->
                    Just $ \c -> do
                        tas <-
                            case mfA of
                                Nothing -> return []
                                Just crlba -> crlba c
                        tbs <-
                            case mfB of
                                Nothing -> return []
                                Just crlbb -> crlbb c
                        return $ fmap (fmap Left) tas <> fmap (fmap Right) tbs
    in MkStorageLensProperty {..}

funcStorageLensProperty ::
       forall baseupdate ap aq bp bq.
       (ap -> Know bq)
    -> (bp -> [aq])
    -> (Know ap -> Know bp -> Maybe (Know aq))
    -> StorageLensProperty ap aq bp bq baseupdate
funcStorageLensProperty ab bsa abma = let
    slpAttribute = funcStorageLensAttribute ab abma
    slpInvGet :: bp -> ReadM (UpdateReader baseupdate) [aq] -- not guaranteed to be unique
    slpInvGet b = return $ bsa b
    slpInvBaseUpdate ::
           baseupdate -> ReadM (UpdateReader baseupdate) (Maybe (bp -> ReadM (UpdateReader baseupdate) [(Bool, aq)]))
    slpInvBaseUpdate _ = return Nothing
    in MkStorageLensProperty {..}

nullStorageLensProperty :: forall baseupdate ap aq bp bq. StorageLensProperty ap aq bp bq baseupdate
nullStorageLensProperty = funcStorageLensProperty (\_ -> Unknown) (\_ -> mempty) (\_ _ -> Nothing)

bijectionStorageLensProperty :: Bijection a b -> StorageLensProperty a a b b baseupdate
bijectionStorageLensProperty (MkIsomorphism ab ba) =
    funcStorageLensProperty (Known . ab) (\b -> opoint $ ba b) (\_ kb -> Just $ fmap ba kb)

runContextReadM ::
       forall m baseupdate update t. MonadIO m
    => Readable m (ContextUpdateReader baseupdate update)
    -> ReadM (UpdateReader baseupdate) t
    -> m t
runContextReadM rd rmt = unReadM rmt $ tupleReadFunction SelectContext rd

bpairToFiniteSetUpdate :: forall a. (Bool, a) -> FiniteSetUpdate a
bpairToFiniteSetUpdate (False, a) = KeyUpdateDelete a
bpairToFiniteSetUpdate (True, a) = KeyUpdateInsertReplace a

putEditAB ::
       forall m baseupdate ap aq bp bq. MonadIO m
    => StorageLensAttribute ap aq bp bq baseupdate
    -> ap
    -> Know bp
    -> Readable m (UpdateReader baseupdate)
    -> m (Maybe [UpdateEdit baseupdate])
putEditAB plm a kb mr = do
    medits <-
        storageLensPutEditBA @m plm [MkBiWholeEdit kb] $ \case
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

storageLensPropertyInverseChangeLens ::
       forall baseupdate a bp bq. Eq a
    => StorageLensProperty a a bq bp baseupdate
    -> ChangeLens (ContextUpdate baseupdate (BiWholeUpdate (Know bp) (Know bq))) (FiniteSetUpdate a)
storageLensPropertyInverseChangeLens plm@MkStorageLensProperty {..} = let
    fsetReadFunction ::
           ReadFunction (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq))) (WholeReader (FiniteSet a))
    fsetReadFunction (mr :: Readable m _) ReadWhole = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        runContextReadM mr $ slpGetKnowPointPreimage plm kb
    clRead :: ReadFunction (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq))) (FiniteSetReader a)
    clRead mr rt = wholeFiniteSetReadFunction (fsetReadFunction mr) rt
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (BiWholeUpdate (Know bp) (Know bq))
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq)))
        -> m [FiniteSetUpdate a]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        mf <- unReadM (slpInvBaseUpdate pinupdate) $ tupleReadFunction SelectContext mr
        case mf of
            Nothing -> return []
            Just brlba -> do
                kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
                case kb of
                    Unknown -> return []
                    Known b -> do
                        lba <- runContextReadM mr $ brlba b
                        return $ fmap bpairToFiniteSetUpdate lba
    clUpdate (MkTupleUpdate SelectContent (MkBiWholeUpdate kb)) mr = do
        aset <- runContextReadM mr $ slpGetKnowPointPreimage plm kb
        aedits <- getReplaceEditsFromSubject aset
        return $ fmap editUpdate aedits
    putEdit ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq)))
        -> m (Maybe [UpdateEdit baseupdate])
    putEdit (KeyEditItem _ update) _ = never update
    putEdit (KeyEditDelete a) mr = putEditAB slpAttribute a Unknown $ tupleReadFunction SelectContext mr
    putEdit (KeyEditInsertReplace a) mr = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        putEditAB slpAttribute a kb $ tupleReadFunction SelectContext mr
    putEdit KeyEditClear mr = do
        kb <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        aa <- runContextReadM mr $ slpGetKnowPointPreimage plm kb
        lmpedits <- for (toList aa) $ \a -> putEditAB slpAttribute a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap mconcat $ sequenceA lmpedits
    clPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know bp) (Know bq)))
        -> m (Maybe [ContextUpdateEdit baseupdate (BiWholeUpdate (Know bp) (Know bq))])
    clPutEdits fsedits mr =
        unComposeInner $ do
            baseedits <- for fsedits $ \fsedit -> MkComposeInner $ putEdit fsedit mr
            return $ fmap (MkTupleUpdateEdit SelectContext) $ mconcat baseedits
    in MkChangeLens {..}

storageLensPropertyInverseChangeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => StorageLensProperty a a b b baseupdate
    -> ChangeLens (ContextUpdate baseupdate (FiniteSetUpdate b)) (FiniteSetUpdate a)
storageLensPropertyInverseChangeLensSet plm@MkStorageLensProperty {..} = let
    clRead' :: ReadFunction (ContextUpdateReader baseupdate (FiniteSetUpdate b)) (FiniteSetReader a)
    clRead' (mr :: Readable m _) KeyReadKeys = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        runContextReadM mr $ slpGetSetPreimage plm bs
    clRead' (mr :: Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))) (KeyReadItem a ReadWhole) = do
        kb <- runContextReadM mr $ slaRead slpAttribute a
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
    clUpdate' (MkTupleUpdate SelectContext pinupdate) mr = do
        mf <- unReadM (slpInvBaseUpdate pinupdate) $ tupleReadFunction SelectContext mr
        case mf of
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
        aset <- runContextReadM mr $ slpGetPointPreimage plm b
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
    clPutEdit' ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    clPutEdit' (KeyEditItem _ update) _ = never update
    clPutEdit' (KeyEditDelete a) mr = do
        mpedits <- putEditAB slpAttribute a Unknown $ tupleReadFunction SelectContext mr
        return $ fmap (\pedits -> fmap (MkTupleUpdateEdit SelectContext) pedits) mpedits
    clPutEdit' (KeyEditInsertReplace a) mr = do
        kb <- runContextReadM mr $ slaRead slpAttribute a
        return $
            case kb of
                Unknown -> Nothing
                Known b -> Just [MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace b]
    clPutEdit' KeyEditClear mr = do
        bs <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        unComposeInner $ do
            lpedits <-
                for (toList bs) $ \b -> do
                    aa <- lift $ runContextReadM mr $ slpInvGet b
                    lpedits <-
                        for (toList aa) $ \a ->
                            MkComposeInner $ putEditAB slpAttribute a Unknown $ tupleReadFunction SelectContext mr
                    return $ mconcat lpedits
            return $ fmap (MkTupleUpdateEdit SelectContext) $ mconcat lpedits
    clPutEdits' ::
           forall m. MonadIO m
        => [FiniteSetEdit a]
        -> Readable m (ContextUpdateReader baseupdate (FiniteSetUpdate b))
        -> m (Maybe [ContextUpdateEdit baseupdate (FiniteSetUpdate b)])
    clPutEdits' [] _ = unComposeInner $ return []
    clPutEdits' (e:ee) mr =
        unComposeInner $ do
            ea <- MkComposeInner $ clPutEdit' @m e mr
            eea <- MkComposeInner $ clPutEdits' ee $ applyEdits' ea mr
            return $ ea ++ eea
    in MkChangeLens clRead' clUpdate' clPutEdits'

instance EditContraFunctor (StorageLensProperty ap aq bp bq) where
    eaContraMap ::
           forall update1 update2.
           ChangeLens update2 update1
        -> StorageLensProperty ap aq bp bq update1
        -> StorageLensProperty ap aq bp bq update2
    eaContraMap lens@MkChangeLens {..} (MkStorageLensProperty attr1 inv1 invbu1) = let
        attr2 :: StorageLensAttribute ap aq bp bq update2
        attr2 = eaContraMap lens attr1
        inv2 :: bp -> ReadM (UpdateReader update2) [aq]
        inv2 bp = mapReadM clRead $ inv1 bp
        invbu2 :: update2 -> ReadM (UpdateReader update2) (Maybe (bp -> ReadM (UpdateReader update2) [(Bool, aq)]))
        invbu2 update2 = do
            updates1 <- MkReadM $ clUpdate update2
            mfs <- mapReadM clRead $ for updates1 invbu1
            return $
                case catMaybes mfs of
                    [] -> Nothing
                    fs ->
                        Just $ \b ->
                            mapReadM clRead $ do
                                las <- for fs $ \f -> f b
                                return $ mconcat las
        in MkStorageLensProperty attr2 inv2 invbu2
