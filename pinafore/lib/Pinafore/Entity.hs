module Pinafore.Entity where

import Pinafore.Literal
import Pinafore.Table
import Shapes
import Text.Read (read)
import Truth.Core

data EntityType t where
    LiteralEntityType :: EntityType Literal
    PointEntityType :: EntityType Point
    ProductEntityType :: EntityType a -> EntityType b -> EntityType (a, b)
    SumEntityType :: EntityType a -> EntityType b -> EntityType (Either a b)

instance TestEquality EntityType where
    testEquality LiteralEntityType LiteralEntityType = return Refl
    testEquality PointEntityType PointEntityType = return Refl
    testEquality (ProductEntityType a1 b1) (ProductEntityType a2 b2) = do
        Refl <- testEquality a1 a2
        Refl <- testEquality b1 b2
        return Refl
    testEquality (SumEntityType a1 b1) (SumEntityType a2 b2) = do
        Refl <- testEquality a1 a2
        Refl <- testEquality b1 b2
        return Refl
    testEquality _ _ = Nothing

instance Representative EntityType where
    getRepWitness LiteralEntityType = Dict
    getRepWitness PointEntityType = Dict
    getRepWitness (ProductEntityType a b) =
        case (getRepWitness a, getRepWitness b) of
            (Dict, Dict) -> Dict
    getRepWitness (SumEntityType a b) =
        case (getRepWitness a, getRepWitness b) of
            (Dict, Dict) -> Dict

instance Is EntityType Literal where
    representative = LiteralEntityType

instance Is EntityType Point where
    representative = PointEntityType

instance (Is EntityType a, Is EntityType b) => Is EntityType (a, b) where
    representative = ProductEntityType representative representative

instance (Is EntityType a, Is EntityType b) => Is EntityType (Either a b) where
    representative = SumEntityType representative representative

data PinaforeEntityRead t where
    PinaforeEntityRead :: (Is EntityType s, Is EntityType v) => Predicate -> s -> PinaforeEntityRead (Maybe v)
    PinaforeEntityLookup :: (Is EntityType s, Is EntityType v) => Predicate -> v -> PinaforeEntityRead (FiniteSet s)

data PinaforeEntityEdit where
    PinaforeEntitySet :: (Is EntityType s, Is EntityType v) => Predicate -> s -> Maybe v -> PinaforeEntityEdit -- pred subj mval

type instance EditReader PinaforeEntityEdit = PinaforeEntityRead

predfst :: Predicate
predfst = MkPredicate $ read "9fa17b88-89f3-4baf-b4b3-fdb7280a0020"

predsnd :: Predicate
predsnd = MkPredicate $ read "3c667db3-c3a5-4ef9-9b15-6cad178c50c7"

predinl :: Predicate
predinl = MkPredicate $ read "ffb8fee1-6971-46c0-9954-62c2ec53e98a"

predinr :: Predicate
predinr = MkPredicate $ read "bbc7a8ca-17e1-4d42-9230-e6b889dea2e5"

pinaforeEntityLens :: EditLens PinaforeTableEdit PinaforeEntityEdit
pinaforeEntityLens = let
    findEntityPoint' ::
           forall m t. Monad m
        => EntityType t
        -> MutableRead m PinaforeTableRead
        -> t
        -> m (Maybe Point)
    findEntityPoint' LiteralEntityType mr lit = do
        fs <- mr $ PinaforeTableReadLookupLiteral lit
        return $ getSingle fs
    findEntityPoint' PointEntityType _ p = return $ Just p
    findEntityPoint' (ProductEntityType ta tb) mr (a, b) =
        getComposeM $ do
            pa <- MkComposeM $ findEntityPoint' ta mr a
            pb <- MkComposeM $ findEntityPoint' tb mr b
            MkComposeM $ do
                fs1 <- mr $ PinaforeTableReadLookupValue predfst pa
                fs2 <- mr $ PinaforeTableReadLookupValue predsnd pb
                return $ getSingle $ fs1 /\ fs2
    findEntityPoint' (SumEntityType ta _) mr (Left a) =
        getComposeM $ do
            pa <- MkComposeM $ findEntityPoint' ta mr a
            MkComposeM $ mr $ PinaforeTableReadGetValue predinl pa
    findEntityPoint' (SumEntityType _ tb) mr (Right b) =
        getComposeM $ do
            pb <- MkComposeM $ findEntityPoint' tb mr b
            MkComposeM $ mr $ PinaforeTableReadGetValue predinr pb
    findEntityPoint ::
           forall m t. (Monad m, Is EntityType t)
        => MutableRead m PinaforeTableRead
        -> t
        -> m (Maybe Point)
    findEntityPoint = findEntityPoint' representative
    pointToEntity' ::
           forall m t. Monad m
        => EntityType t
        -> MutableRead m PinaforeTableRead
        -> Point
        -> m (Maybe t)
    pointToEntity' LiteralEntityType mr p = mr $ PinaforeTableReadGetLiteral p
    pointToEntity' PointEntityType _ p = return $ Just p
    pointToEntity' (ProductEntityType ta tb) mr p =
        getComposeM $ do
            pa <- MkComposeM $ mr $ PinaforeTableReadGetValue predfst p
            a <- MkComposeM $ pointToEntity' ta mr pa
            pb <- MkComposeM $ mr $ PinaforeTableReadGetValue predsnd p
            b <- MkComposeM $ pointToEntity' tb mr pb
            return (a, b)
    pointToEntity' (SumEntityType ta tb) mr p = do
        spa <- mr $ PinaforeTableReadLookupValue predinl p
        case getSingle spa of
            Just pa -> do
                ma <- pointToEntity' ta mr pa
                return $ fmap Left ma
            Nothing -> do
                spb <- mr $ PinaforeTableReadLookupValue predinr p
                case getSingle spb of
                    Just pb -> do
                        mb <- pointToEntity' tb mr pb
                        return $ fmap Right mb
                    Nothing -> return Nothing
    pointToEntity ::
           forall m t. (Monad m, Is EntityType t)
        => MutableRead m PinaforeTableRead
        -> Point
        -> m (Maybe t)
    pointToEntity = pointToEntity' representative
    reifyNewEntity' ::
           forall m t. MonadIO m
        => EntityType t
        -> MutableRead m PinaforeTableRead
        -> t
        -> WriterT [PinaforeTableEdit] m Point
    reifyNewEntity' PointEntityType _ p = return p
    reifyNewEntity' LiteralEntityType _ lit = do
        p <- liftIO randomIO
        tell [PinaforeTableEditSetLiteral p $ Just lit]
        return p
    reifyNewEntity' (ProductEntityType ta tb) mr (a, b) = do
        pa <- reifyNewEntity' ta mr a
        pb <- reifyNewEntity' tb mr b
        p <- liftIO randomIO
        tell [PinaforeTableEditSetValue predfst p $ Just pa, PinaforeTableEditSetValue predsnd p $ Just pb]
        return p
    reifyNewEntity' (SumEntityType ta _) mr (Left a) = do
        pa <- reifyNewEntity' ta mr a
        p <- liftIO randomIO
        tell [PinaforeTableEditSetValue predinl pa $ Just p]
        return p
    reifyNewEntity' (SumEntityType _ tb) mr (Right b) = do
        pb <- reifyNewEntity' tb mr b
        p <- liftIO randomIO
        tell [PinaforeTableEditSetValue predinr pb $ Just p]
        return p
    reifyNewEntity ::
           forall m t. (MonadIO m, Is EntityType t)
        => MutableRead m PinaforeTableRead
        -> t
        -> WriterT [PinaforeTableEdit] m Point
    reifyNewEntity = reifyNewEntity' representative
    reifyEntity ::
           forall m t. (MonadIO m, Is EntityType t)
        => MutableRead m PinaforeTableRead
        -> t
        -> WriterT [PinaforeTableEdit] m Point
    reifyEntity mr t = do
        mp <- lift $ findEntityPoint mr t
        case mp of
            Just p -> return p
            Nothing -> reifyNewEntity mr t
    efGet :: ReadFunctionT IdentityT PinaforeTableRead PinaforeEntityRead
    efGet mr (PinaforeEntityRead prd se) =
        lift $
        getComposeM $ do
            sp <- MkComposeM $ findEntityPoint mr se
            vp <- MkComposeM $ mr $ PinaforeTableReadGetValue prd sp
            MkComposeM $ pointToEntity mr vp
    efGet mr (PinaforeEntityLookup prd ve) =
        lift $ do
            mvp <- findEntityPoint mr ve
            case mvp of
                Nothing -> return mempty
                Just vp -> do
                    ss <- mr $ PinaforeTableReadLookupValue prd vp
                    mse <- for ss $ \sp -> pointToEntity mr sp
                    return $ catMaybes mse
    -- PinaforeTableEditSetValue :: Predicate -> Point -> Maybe Point -> PinaforeTableEdit -- pred subj mval
    -- PinaforeTableEditSetLiteral :: Point -> Maybe Literal -> PinaforeTableEdit
    efUpdate ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> MutableRead m PinaforeTableRead
        -> IdentityT m [PinaforeEntityEdit]
    efUpdate _ _ = return [] -- NYI
    elFunction :: AnEditFunction IdentityT PinaforeTableEdit PinaforeEntityEdit
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [PinaforeEntityEdit]
        -> MutableRead m PinaforeTableRead
        -> IdentityT m (Maybe [PinaforeTableEdit])
    elPutEdits =
        elPutEditsFromPutEdit $ \(PinaforeEntitySet prd es mev) mr ->
            lift $ do
                ((), edits) <-
                    runWriterT $ do
                        ps <- reifyEntity mr es
                        mpv <- for mev $ reifyEntity mr
                        tell [PinaforeTableEditSetValue prd ps mpv]
                return $ Just edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
