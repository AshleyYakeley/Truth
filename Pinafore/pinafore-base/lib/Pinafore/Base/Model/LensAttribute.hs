module Pinafore.Base.Model.LensAttribute
    ( StorageLensAttribute(..)
    , slaRead
    , identityStorageLensAttribute
    , composeStorageLensAttribute
    , pairStorageLensAttribute
    , eitherStorageLensAttribute
    , funcStorageLensAttribute
    , lensFunctionAttribute
    , storageLensPutEditBA
    , storageLensAttributeChangeLens
    ) where

import Changes.Core
import Pinafore.Base.Know
import Pinafore.Base.Model.FunctionAttribute
import Shapes

data StorageLensAttribute ap aq bp bq baseupdate = MkStorageLensAttribute
    { slaFunction :: StorageFunctionAttribute baseupdate ap (Know bq)
    , slaPut :: Know ap -> Know bp -> ReadM (UpdateReader baseupdate) (Maybe ([UpdateEdit baseupdate], Maybe (Know aq)))
    }

instance CatFunctor (->) (NestedMorphism (->)) (StorageLensAttribute ap aq bp) where
    cfmap f =
        MkNestedMorphism $ \(MkStorageLensAttribute fa p) -> let
            fa' = cfmap (fmap f) fa
            in MkStorageLensAttribute fa' p

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (StorageLensAttribute ap aq) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkStorageLensAttribute fa p) -> let
            p' = (fmap $ cfmap1 $ endocfmap f) p
            in MkStorageLensAttribute fa p'

instance CatFunctor (->) (NestedMorphism (->)) (StorageLensAttribute ap) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $ \(MkStorageLensAttribute fa p) -> let
            p' = (fmap $ fmap $ fmap $ fmap $ fmap $ fmap $ fmap f) p
            in MkStorageLensAttribute fa p'

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (StorageLensAttribute) where
    cfmap f =
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $
        MkNestedMorphism $ \(MkStorageLensAttribute fa p) -> let
            fa' = cfmap1 f fa
            p' = (cfmap1 $ endocfmap f) p
            in MkStorageLensAttribute fa' p'

instance EditContraFunctor (StorageLensAttribute ap aq bp bq) where
    eaContraMap ::
           forall update1 update2.
           ChangeLens update2 update1
        -> StorageLensAttribute ap aq bp bq update1
        -> StorageLensAttribute ap aq bp bq update2
    eaContraMap MkChangeLens {..} (MkStorageLensAttribute (MkStorageFunctionAttribute get1 bu1) put1) = let
        get2 :: ap -> ReadM (UpdateReader update2) (Know bq)
        get2 a = mapReadM clRead $ get1 a
        bu2 :: update2 -> ReadM (UpdateReader update2) (Maybe (ap -> ReadM (UpdateReader update2) (Maybe (Know bq))))
        bu2 update2 = do
            updates1 <- MkReadM $ clUpdate update2
            mfs <- mapReadM clRead $ for updates1 bu1
            return $
                case catMaybes mfs of
                    [] -> Nothing
                    fs ->
                        Just $ \a ->
                            mapReadM clRead $ do
                                mkbs <- for fs $ \f -> f a
                                return $ fmap last $ nonEmpty $ catMaybes mkbs
        put2 :: Know ap -> Know bp -> ReadM (UpdateReader update2) (Maybe ([UpdateEdit update2], Maybe (Know aq)))
        put2 ka kb = do
            mea <- mapReadM clRead $ put1 ka kb
            forf mea $ \(edits1, mka) -> do
                medits2 <- MkReadM $ clPutEdits edits1
                return $ fmap (\edits2 -> (edits2, mka)) medits2
        in MkStorageLensAttribute (MkStorageFunctionAttribute get2 bu2) put2

slaRead :: StorageLensAttribute ap aq bp bq baseupdate -> ap -> ReadM (UpdateReader baseupdate) (Know bq)
slaRead plm = sfaRead $ slaFunction plm

slaKRead :: StorageLensAttribute ap aq bp bq baseupdate -> Know ap -> ReadM (UpdateReader baseupdate) (Know bq)
slaKRead plm (Known a) = slaRead plm a
slaKRead _ Unknown = return Unknown

identityStorageLensAttribute :: forall baseupdate x y. StorageLensAttribute x y y x baseupdate
identityStorageLensAttribute = let
    slaFunction = arr Known
    slaPut _ kb = return $ Just ([], Just kb)
    in MkStorageLensAttribute {..}

composeStorageLensAttribute ::
       forall baseupdate ap aq bx by cp cq.
       StorageLensAttribute bx by cp cq baseupdate
    -> StorageLensAttribute ap aq by bx baseupdate
    -> StorageLensAttribute ap aq cp cq baseupdate
composeStorageLensAttribute (MkStorageLensAttribute funcBC putBC) ab@(MkStorageLensAttribute funcAB putAB) = let
    slaFunction =
        proc a -> do
            kb <- funcAB -< a
            case kb of
                Known b -> funcBC -< b
                Unknown -> returnA -< Unknown
    slaPut koldA kC =
        unComposeInner $ do
            koldB <- lift $ slaKRead ab koldA
            (edits1, mknewB) <- MkComposeInner $ putBC koldB kC
            case mknewB of
                Nothing -> return (edits1, Nothing)
                Just knewB -> do
                    (edits2, mknewA) <- MkComposeInner $ putAB koldA knewB
                    return (edits1 <> edits2, mknewA)
    in MkStorageLensAttribute {..}

pairStorageLensAttribute ::
       forall baseupdate ap aq bp bq cp cq.
       StorageLensAttribute ap aq bp bq baseupdate
    -> StorageLensAttribute ap aq cp cq baseupdate
    -> StorageLensAttribute ap aq (bp, cp) (bq, cq) baseupdate
pairStorageLensAttribute (MkStorageLensAttribute funcB putB) (MkStorageLensAttribute funcC putC) = let
    slaFunction = liftA2 (liftA2 (,)) funcB funcC
    slaPut _ Unknown = return Nothing -- can't delete
    slaPut ka (Known (b, c)) =
        unComposeInner $ do
            (updb, bmka) <- MkComposeInner $ putB ka $ Known b
            (updc, cmka) <- MkComposeInner $ putC ka $ Known c
            return (updb <> updc, bmka <|> cmka)
    in MkStorageLensAttribute {..}

eitherStorageLensAttribute ::
       forall baseupdate ap aq bp bq cp cq.
       StorageLensAttribute ap aq cp cq baseupdate
    -> StorageLensAttribute bp bq cp cq baseupdate
    -> StorageLensAttribute (Either ap bp) (Either aq bq) cp cq baseupdate
eitherStorageLensAttribute (MkStorageLensAttribute funcA putA) (MkStorageLensAttribute funcB putB) = let
    slaFunction =
        proc eab ->
            case eab of
                Left a -> funcA -< a
                Right b -> funcB -< b
    slaPut (Known (Left a)) kc =
        unComposeInner $ do
            (bu, mka) <- MkComposeInner $ putA (Known a) kc
            return $ (bu, fmap (fmap Left) mka)
    slaPut (Known (Right b)) kc =
        unComposeInner $ do
            (bu, mkb) <- MkComposeInner $ putB (Known b) kc
            return $ (bu, fmap (fmap Right) mkb)
    slaPut Unknown _ = return Nothing
    in MkStorageLensAttribute {..}

funcStorageLensAttribute ::
       forall baseupdate ap aq bp bq.
       (ap -> Know bq)
    -> (Know ap -> Know bp -> Maybe (Know aq))
    -> StorageLensAttribute ap aq bp bq baseupdate
funcStorageLensAttribute ab bma = let
    slaFunction = arr ab
    slaPut :: Know ap -> Know bp -> ReadM (UpdateReader baseupdate) (Maybe ([UpdateEdit baseupdate], Maybe (Know aq)))
    slaPut ka kb =
        return $ do
            ka' <- bma ka kb
            return ([], Just ka')
    in MkStorageLensAttribute {..}

lensFunctionAttribute ::
       forall baseupdate ap aq bp bq.
       StorageLensAttribute ap aq bp bq baseupdate
    -> StorageFunctionAttribute baseupdate (Know ap) (Know bq)
lensFunctionAttribute plm =
    proc ka ->
        case ka of
            Known a -> slaFunction plm -< a
            Unknown -> returnA -< Unknown

runContextReadM ::
       forall m baseupdate update t. MonadIO m
    => Readable m (ContextUpdateReader baseupdate update)
    -> ReadM (UpdateReader baseupdate) t
    -> m t
runContextReadM rd rmt = unReadM rmt $ tupleReadFunction SelectContext rd

storageLensPutEditBA ::
       forall m baseupdate ap aq bp bq. MonadIO m
    => StorageLensAttribute ap aq bp bq baseupdate
    -> [BiWholeEdit (Know bp) (Know bq)]
    -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap)))
    -> m (Maybe [ContextUpdateEdit baseupdate (BiWholeUpdate (Know aq) (Know ap))])
storageLensPutEditBA plm editsB mr =
    case lastM editsB of
        Nothing -> return $ Just []
        Just (MkBiWholeEdit kb) -> do
            ka <- mr $ MkTupleUpdateReader SelectContent ReadWhole
            medits <- runContextReadM mr $ slaPut plm ka kb
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

storageLensAttributeChangeLens ::
       forall baseupdate ap aq bp bq.
       StorageLensAttribute ap aq bp bq baseupdate
    -> ChangeLens (ContextUpdate baseupdate (BiWholeUpdate (Know aq) (Know ap))) (BiWholeUpdate (Know bp) (Know bq))
storageLensAttributeChangeLens plm = let
    clRead ::
           forall .
           ReadFunction (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap))) (WholeReader (Know bq))
    clRead mr ReadWhole = do
        ka <- mr $ MkTupleUpdateReader SelectContent ReadWhole
        runContextReadM mr $ slaKRead plm ka
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate baseupdate (BiWholeUpdate (Know aq) (Know ap))
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap)))
        -> m [BiWholeUpdate (Know bp) (Know bq)]
    clUpdate (MkTupleUpdate SelectContext pinupdate) mr = do
        mf <- runContextReadM mr $ sfaUpdate (slaFunction plm) pinupdate
        case mf of
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
        kb <- runContextReadM mr $ slaKRead plm ka
        return $ pure $ MkBiWholeUpdate kb
    clPutEdits ::
           forall m. MonadIO m
        => [BiWholeEdit (Know bp) (Know bq)]
        -> Readable m (ContextUpdateReader baseupdate (BiWholeUpdate (Know aq) (Know ap)))
        -> m (Maybe [ContextUpdateEdit baseupdate (BiWholeUpdate (Know aq) (Know ap))])
    clPutEdits = storageLensPutEditBA plm
    in MkChangeLens {..}
