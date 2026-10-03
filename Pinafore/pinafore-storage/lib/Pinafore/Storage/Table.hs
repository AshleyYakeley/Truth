module Pinafore.Storage.Table
    ( Anchor
    , Predicate (..)
    , Entity (..)
    , RefCount
    , QTableSubject (..)
    , QTableRead (..)
    , QTableEdit (..)
    , QTableUpdate
    , qTableEntityLens
    , qTableEntityReference
    )
where

import Changes.Core
import Pinafore.Base
import Shapes

type RefCount = Int

data QTableRead t where
    QTableReadPropertyGet :: Predicate -> Entity -> QTableRead (Maybe Entity)
    QTableReadPropertyLookup :: Predicate -> Entity -> QTableRead (ListSet Entity)
    QTableReadEntityRefCount :: Entity -> QTableRead (Maybe RefCount)
    QTableReadFactGet :: Predicate -> Entity -> QTableRead (Maybe Entity)
    QTableReadLiteralGet :: Entity -> QTableRead (Maybe Literal)

instance Show (QTableRead t) where
    show (QTableReadPropertyGet p s) = "prop get " ++ show p ++ " of " ++ show s
    show (QTableReadPropertyLookup p v) = "prop lookup " ++ show p ++ " for " ++ show v
    show (QTableReadEntityRefCount v) = "entity ref count " ++ show v
    show (QTableReadFactGet p s) = "fact get " ++ show p ++ " of " ++ show s
    show (QTableReadLiteralGet v) = "literal get " ++ show v

instance WitnessConstraint Show QTableRead where
    witnessConstraint (QTableReadPropertyGet _ _) = Dict
    witnessConstraint (QTableReadPropertyLookup _ _) = Dict
    witnessConstraint (QTableReadEntityRefCount _) = Dict
    witnessConstraint (QTableReadFactGet _ _) = Dict
    witnessConstraint (QTableReadLiteralGet _) = Dict

instance AllConstraint Show QTableRead where
    allConstraint = Dict

data QTableEdit where
    QTableEditPropertySet :: Predicate -> Entity -> Maybe Entity -> QTableEdit -- pred subj mval
    QTableEditEntityRefCount :: Entity -> Maybe RefCount -> QTableEdit -- pred subj mval
    QTableEditFactSet :: Predicate -> Entity -> Maybe Entity -> QTableEdit -- pred subj mval
    QTableEditLiteralSet :: Entity -> Maybe Literal -> QTableEdit

instance Show QTableEdit where
    show (QTableEditPropertySet p s mv) = "prop set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (QTableEditEntityRefCount v rc) = "entity count " ++ show v ++ " to " ++ show rc
    show (QTableEditFactSet p s v) = "fact set " ++ show p ++ " of " ++ show s ++ " is " ++ show v
    show (QTableEditLiteralSet v l) = "literal set " ++ show v ++ " is " ++ show l

data QTableSubject = MkQTableSubject
    { ptsPredicates :: [(Predicate, Entity, Entity)]
    , ptsRefCounts :: [(Entity, RefCount)]
    , ptsFacts :: [(Predicate, Entity, Entity)]
    , ptsLiterals :: [(Entity, Literal)]
    }

instance SubjectReader QTableRead where
    type ReaderSubject QTableRead = QTableSubject
    subjectToRead MkQTableSubject{..} (QTableReadPropertyGet rp rs) =
        listToMaybe $ [v | (p, s, v) <- ptsPredicates, p == rp && s == rs]
    subjectToRead MkQTableSubject{..} (QTableReadPropertyLookup rp rv) =
        setFromList [s | (p, s, v) <- ptsPredicates, p == rp, v == rv]
    subjectToRead MkQTableSubject{..} (QTableReadEntityRefCount rv) =
        listToMaybe $ [c | (v, c) <- ptsRefCounts, v == rv]
    subjectToRead MkQTableSubject{..} (QTableReadFactGet rp rs) =
        listToMaybe $ [v | (p, s, v) <- ptsFacts, p == rp && s == rs]
    subjectToRead MkQTableSubject{..} (QTableReadLiteralGet rv) = listToMaybe [l | (v, l) <- ptsLiterals, v == rv]

instance FloatingOn QTableEdit QTableEdit

type instance EditReader QTableEdit = QTableRead

instance ApplicableEdit QTableEdit where
    applyEdit (QTableEditPropertySet p s mv) _ (QTableReadPropertyGet p' s')
        | p == p' && s == s' = return mv
    applyEdit (QTableEditPropertySet p s mv) mr (QTableReadPropertyLookup p' v')
        | p == p' = do
            fs <- mr $ QTableReadPropertyLookup p' v'
            return
                $ case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (QTableEditEntityRefCount v mc) _ (QTableReadEntityRefCount v')
        | v == v' = return mc
    applyEdit (QTableEditFactSet p s mv) _ (QTableReadFactGet p' s')
        | p == p' && s == s' = return mv
    applyEdit (QTableEditLiteralSet v ml) _ (QTableReadLiteralGet v')
        | v == v' = return ml
    applyEdit _ mr rt = mr rt

replaceFirst :: (a -> Maybe (Maybe a, b)) -> [a] -> ([a], Maybe b)
replaceFirst _ [] = ([], Nothing)
replaceFirst f (a : aa)
    | Just (ma, b) <- f a =
        case ma of
            Just a' -> (a' : aa, Just b)
            Nothing -> (aa, Just b)
replaceFirst f (a : aa) =
    case replaceFirst f aa of
        (aa', mb) -> (a : aa', mb)

replaceOrAdd :: (a -> Bool) -> Maybe a -> [a] -> [a]
replaceOrAdd f mitem aa =
    case replaceFirst
        ( \a ->
            if f a
                then Just (mitem, ())
                else Nothing
        )
        aa of
        (aa', Just ()) -> aa'
        (aa', Nothing) ->
            case mitem of
                Nothing -> aa'
                Just a -> a : aa'

instance SubjectMapEdit QTableEdit where
    mapSubjectEdits =
        mapEditToMapEdits $ \edit (MkQTableSubject oldPredicates oldRefCounts oldFacts oldLiterals) ->
            case edit of
                QTableEditPropertySet prd (checkEntity "store.prop.s" -> s) (fmap (checkEntity "store.prop.v") -> mv) -> let
                    newPredicates =
                        replaceOrAdd
                            (\(prd', s', _) -> (prd' == prd) && (s == s'))
                            (fmap (\v -> (prd, s, v)) mv)
                            oldPredicates
                    in return $ MkQTableSubject newPredicates oldRefCounts oldFacts oldLiterals
                QTableEditEntityRefCount v mrc -> let
                    newRefCounts = replaceOrAdd (\(v', _) -> v == v') (fmap (\rc -> (v, rc)) mrc) oldRefCounts
                    in return $ MkQTableSubject oldPredicates newRefCounts oldFacts oldLiterals
                QTableEditFactSet prd (checkEntity "store.fact.s" -> s) (fmap (checkEntity "store.fact.v") -> mv) -> let
                    newFacts =
                        replaceOrAdd
                            (\(prd', s', _) -> (prd' == prd) && (s == s'))
                            (fmap (\v -> (prd, s, v)) mv)
                            oldFacts
                    in return $ MkQTableSubject oldPredicates oldRefCounts newFacts oldLiterals
                QTableEditLiteralSet (checkEntity "store.literal.v" -> v) ml -> let
                    newLiterals = replaceOrAdd (\(v', _) -> v == v') (fmap (\l -> (v, l)) ml) oldLiterals
                    in return $ MkQTableSubject oldPredicates oldRefCounts oldFacts newLiterals

instance InvertibleEdit QTableEdit where
    invertEdit (QTableEditPropertySet p s _) mr = do
        mv <- mr $ QTableReadPropertyGet p s
        return [QTableEditPropertySet p s mv]
    invertEdit (QTableEditEntityRefCount v _) mr = do
        mrc <- mr $ QTableReadEntityRefCount v
        return [QTableEditEntityRefCount v mrc]
    invertEdit (QTableEditFactSet p s _) mr = do
        mv <- mr $ QTableReadFactGet p s
        return [QTableEditFactSet p s mv]
    invertEdit (QTableEditLiteralSet v _) mr = do
        ml <- mr $ QTableReadLiteralGet v
        return [QTableEditLiteralSet v ml]

data PropertyCacheKey (cache :: (Type -> Type) -> Type) (t :: Type) (ct :: Type) where
    GetPropertyCacheKey :: PropertyCacheKey cache t (cache (SimpleCacheKey Entity (Maybe t)))
    LookupPropertyCacheKey :: PropertyCacheKey cache t (cache (SimpleCacheKey t (ListSet Entity)))

instance TestEquality (PropertyCacheKey cache t) where
    testEquality GetPropertyCacheKey GetPropertyCacheKey = Just Refl
    testEquality LookupPropertyCacheKey LookupPropertyCacheKey = Just Refl
    testEquality _ _ = Nothing

data QTableEditCacheKey cache ct where
    PropertyQTableEditCacheKey :: Predicate -> QTableEditCacheKey cache (cache (PropertyCacheKey cache Entity))
    RefCountQTableEditCacheKey :: QTableEditCacheKey cache (cache (SimpleCacheKey Entity (Maybe RefCount)))
    FactQTableEditCacheKey :: Predicate -> QTableEditCacheKey cache (cache (SimpleCacheKey Entity (Maybe Entity)))
    LiteralQTableEditCacheKey :: QTableEditCacheKey cache (cache (SimpleCacheKey Entity (Maybe Literal)))

instance TestEquality (QTableEditCacheKey cache) where
    testEquality (PropertyQTableEditCacheKey p1) (PropertyQTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality RefCountQTableEditCacheKey RefCountQTableEditCacheKey = Just Refl
    testEquality (FactQTableEditCacheKey p1) (FactQTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality LiteralQTableEditCacheKey LiteralQTableEditCacheKey = Just Refl
    testEquality _ _ = Nothing

data QTableEditKey
    = QTEKProperty
        Predicate
        Entity
    | QTEKEntityRefCount Entity
    | QTEKFact
        Predicate
        Entity
    | QTEKLiteral Entity
    deriving stock (Eq, Ord)

qTableEditKey :: QTableEdit -> QTableEditKey
qTableEditKey (QTableEditPropertySet p e _) = QTEKProperty p e
qTableEditKey (QTableEditEntityRefCount e _) = QTEKEntityRefCount e
qTableEditKey (QTableEditFactSet p e _) = QTEKFact p e
qTableEditKey (QTableEditLiteralSet e _) = QTEKLiteral e

instance CacheableEdit QTableEdit where
    trimEdits = trimWithMap qTableEditKey
    type EditCacheKey cache QTableEdit = QTableEditCacheKey cache
    editCacheAdd (QTableReadPropertyGet p s) mv =
        subcacheModify (PropertyQTableEditCacheKey p)
            $ subcacheModify GetPropertyCacheKey
            $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (QTableReadPropertyLookup p v) fs =
        subcacheModify (PropertyQTableEditCacheKey p)
            $ subcacheModify LookupPropertyCacheKey
            $ cacheAdd (MkSimpleCacheKey v) fs
    editCacheAdd (QTableReadEntityRefCount v) mv =
        subcacheModify RefCountQTableEditCacheKey $ cacheAdd (MkSimpleCacheKey v) mv
    editCacheAdd (QTableReadFactGet p s) mv =
        subcacheModify (FactQTableEditCacheKey p) $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (QTableReadLiteralGet s) mv =
        subcacheModify LiteralQTableEditCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheLookup (QTableReadPropertyGet p s) cache = do
        subcache1 <- cacheLookup (PropertyQTableEditCacheKey p) cache
        subcache2 <- cacheLookup GetPropertyCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheLookup (QTableReadPropertyLookup p v) cache = do
        subcache1 <- cacheLookup (PropertyQTableEditCacheKey p) cache
        subcache2 <- cacheLookup LookupPropertyCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
    editCacheLookup (QTableReadEntityRefCount v) cache = do
        subcache1 <- cacheLookup RefCountQTableEditCacheKey cache
        cacheLookup (MkSimpleCacheKey v) subcache1
    editCacheLookup (QTableReadFactGet p s) cache = do
        subcache1 <- cacheLookup (FactQTableEditCacheKey p) cache
        cacheLookup (MkSimpleCacheKey s) subcache1
    editCacheLookup (QTableReadLiteralGet s) cache = do
        subcache1 <- cacheLookup LiteralQTableEditCacheKey cache
        cacheLookup (MkSimpleCacheKey s) subcache1
    editCacheUpdate (QTableEditPropertySet p s mv) =
        subcacheModify (PropertyQTableEditCacheKey p) $ do
            subcacheModify GetPropertyCacheKey $ cacheModify (MkSimpleCacheKey s) $ Shapes.put $ Just mv
            subcacheModify LookupPropertyCacheKey
                $ cacheTraverse
                $ \(MkSimpleCacheKey v') ss' ->
                    return
                        $ Just
                        $ ( if mv == Just v'
                                then insertItem
                                else deleteKey
                          )
                            s
                            ss'
    editCacheUpdate (QTableEditEntityRefCount v t) =
        subcacheModify RefCountQTableEditCacheKey $ do cacheModify (MkSimpleCacheKey v) $ Shapes.put $ Just t
    editCacheUpdate (QTableEditFactSet prd s t) =
        subcacheModify (FactQTableEditCacheKey prd) $ do cacheModify (MkSimpleCacheKey s) $ Shapes.put $ Just t
    editCacheUpdate (QTableEditLiteralSet v t) =
        subcacheModify LiteralQTableEditCacheKey $ do cacheModify (MkSimpleCacheKey v) $ Shapes.put $ Just t

-- | Translate storage edits while making subsequent reads see the pending table edits.
type QTableEditM m = WriterT [QTableEdit] (StateT (ReadableW m QTableRead) m)

qTableEntityPutEdits :: forall m. MonadIO m => [QStorageEdit] -> Readable m QTableRead -> m (Maybe [QTableEdit])
qTableEntityPutEdits edits mr = let
    tableRead :: Readable (QTableEditM m) QTableRead
    tableRead = liftReadable stateReadable
    tablePush :: QTableEdit -> QTableEditM m ()
    tablePush edit = do
        lift $ modify $ \(MkReadableW rd) -> MkReadableW $ applyEdit edit rd
        tell [edit]
    acquireEntity :: Entity -> QTableEditM m ()
    acquireEntity entity = do
        mrc <- tableRead $ QTableReadEntityRefCount entity
        newrc <-
            return
                $ case mrc of
                    Nothing -> 1
                    Just oldrc -> succ oldrc
        tablePush $ QTableEditEntityRefCount entity $ Just newrc
    releaseByFact ::
        forall t. FieldStorer 'MultipleMode t -> Entity -> QTableEditM m ()
    releaseByFact (MkFieldStorer p subdef) entity = do
        msubv <- tableRead $ QTableReadFactGet p entity
        for_ msubv $ \subv -> releaseByEntity subdef subv
        tablePush $ QTableEditFactSet p entity Nothing
    releaseByConstructor ::
        forall t.
        ConstructorStorer 'MultipleMode t ->
        Entity ->
        QTableEditM m ()
    releaseByConstructor PlainConstructorStorer _ = return ()
    releaseByConstructor LiteralConstructorStorer entity
        | Just _ <- entityToLiteral entity = return ()
    releaseByConstructor LiteralConstructorStorer entity =
        tablePush $ QTableEditLiteralSet entity Nothing
    releaseByConstructor (ConstructorConstructorStorer _ facts) entity =
        listTypeFor_ facts $ \fact -> releaseByFact fact entity
    releaseByEntity ::
        forall t. MultipleEntityStorer t -> Entity -> QTableEditM m ()
    releaseByEntity (MkMultipleEntityStorer css) entity = do
        mrc <- tableRead $ QTableReadEntityRefCount entity
        case mrc of
            Just 1 -> do
                tablePush $ QTableEditEntityRefCount entity Nothing
                for_ css $ \(MkKnowShim def _) -> releaseByConstructor def entity
            Just oldrc -> tablePush $ QTableEditEntityRefCount entity $ Just $ pred oldrc
            Nothing -> return ()
    releaseByAdapter :: forall t. StoreAdapter t -> Entity -> QTableEditM m ()
    releaseByAdapter vtype entity =
        releaseByEntity (storeAdapterDefinitions vtype) entity
    setFact ::
        forall (t :: Type).
        FieldStorer 'SingleMode t ->
        Entity ->
        t ->
        QTableEditM m ()
    setFact (MkFieldStorer p subdef) v t = do
        let subv = entityStorerToEntity subdef t
        moldsub <- tableRead $ QTableReadFactGet p v
        case moldsub of
            Just _ -> return ()
            Nothing -> do
                tablePush $ QTableEditFactSet p v $ Just subv
                acquireEntity subv
        setEntity subdef subv t
    setFacts ::
        forall (t :: [Type]).
        ListType (FieldStorer 'SingleMode) t ->
        Entity ->
        ListProduct t ->
        QTableEditM m ()
    setFacts NilListType _ () = return ()
    setFacts (ConsListType f1 fr) v (a1, ar) = do
        setFact f1 v a1
        setFacts fr v ar
    setConstructor ::
        forall (t :: Type).
        ConstructorStorer 'SingleMode t ->
        Entity ->
        t ->
        QTableEditM m ()
    setConstructor PlainConstructorStorer _ _ = return ()
    setConstructor LiteralConstructorStorer v _
        | Just _ <- entityToLiteral v = return ()
    setConstructor LiteralConstructorStorer v l =
        tablePush $ QTableEditLiteralSet v $ Just l
    setConstructor (ConstructorConstructorStorer _ facts) v t = setFacts facts v t
    setEntity ::
        forall (t :: Type).
        SingleEntityStorer t ->
        Entity ->
        t ->
        QTableEditM m ()
    setEntity (MkSingleEntityStorer cs) e t = setConstructor cs e t
    setEntityFromAdapter :: Entity -> StoreAdapter t -> t -> QTableEditM m ()
    setEntityFromAdapter entity ea t = do
        case storeAdapterToDefinition ea t of
            MkSomeOf def tt -> setEntity def entity tt
    doEntityEdit :: QStorageEdit -> QTableEditM m ()
    doEntityEdit (MkQStorageEdit stype vtype p s (Known v)) = do
        let
            se = storeAdapterConvert stype s
            ve = storeAdapterConvert vtype v
        mroldv <- tableRead $ QTableReadPropertyGet p se
        case mroldv of
            Just oldv
                | oldv == ve -> return ()
            Nothing -> do
                setEntityFromAdapter se stype s
                acquireEntity se
                setEntityFromAdapter ve vtype v
                acquireEntity ve
            Just oldv -> do
                setEntityFromAdapter ve vtype v
                acquireEntity ve
                releaseByAdapter vtype oldv
        tablePush $ QTableEditPropertySet p se $ Just ve
    doEntityEdit (MkQStorageEdit stype vtype p s Unknown) = do
        let se = storeAdapterConvert stype s
        mroldv <- tableRead $ QTableReadPropertyGet p se
        case mroldv of
            Nothing -> return ()
            Just oldv -> do
                releaseByAdapter stype se
                releaseByAdapter vtype oldv
                tablePush $ QTableEditPropertySet p se Nothing
    in fmap Just $ evalStateT (execWriterT $ for_ edits doEntityEdit) $ MkReadableW mr

qTableEntityRead :: ReadFunction QTableRead QStorageRead
qTableEntityRead (tableRead :: Readable m QTableRead) = let
    readFact ::
        forall (t :: Type).
        FieldStorer 'MultipleMode t ->
        Entity ->
        ComposeInner Know m t
    readFact (MkFieldStorer p subdef) entity = do
        subentity <- MkComposeInner $ fmap maybeToKnow $ tableRead $ QTableReadFactGet p entity
        readEntity subdef subentity
    readFacts ::
        forall (t :: [Type]).
        ListType (FieldStorer 'MultipleMode) t ->
        Entity ->
        ComposeInner Know m (ListProduct t)
    readFacts NilListType _ = return ()
    readFacts (ConsListType f1 fr) entity = do
        t1 <- readFact f1 entity
        tr <- readFacts fr entity
        return (t1, tr)
    readConstructor ::
        forall (t :: Type).
        ConstructorStorer 'MultipleMode t ->
        Entity ->
        ComposeInner Know m t
    readConstructor PlainConstructorStorer entity = return entity
    readConstructor LiteralConstructorStorer entity
        | Just lit <- entityToLiteral entity = return lit
    readConstructor LiteralConstructorStorer entity =
        MkComposeInner $ fmap maybeToKnow $ tableRead $ QTableReadLiteralGet entity
    readConstructor (ConstructorConstructorStorer _ facts) entity = readFacts facts entity
    firstKnown :: forall n a b. MonadPlus n => [a] -> (a -> n b) -> n b
    firstKnown [] _ = empty
    firstKnown (a : aa) f = f a <|> firstKnown aa f
    readEntity ::
        forall (t :: Type).
        MultipleEntityStorer t ->
        Entity ->
        ComposeInner Know m t
    readEntity (MkMultipleEntityStorer css) entity =
        firstKnown css $ \(MkKnowShim def f) -> do
            dt <- readConstructor def entity
            liftInner $ f dt
    readStorage :: Readable m QStorageRead
    readStorage (QStorageReadGet stype prd subj) = do
        mval <- tableRead $ QTableReadPropertyGet prd $ storeAdapterConvert stype subj
        return $ maybeToKnow mval
    readStorage (QStorageReadLookup prd val) = tableRead $ QTableReadPropertyLookup prd val
    readStorage (QStorageReadEntity ea entity) =
        unComposeInner $ readEntity (storeAdapterDefinitions ea) entity
    in readStorage

-- | Facts and literals are immutable entity representations. Their maintenance
-- does not emit property updates; property assignments carry the notifications.
qTableEntityLens :: ChangeLens QTableUpdate QStorageUpdate
qTableEntityLens = let
    clRead :: ReadFunction QTableRead QStorageRead
    clRead = qTableEntityRead
    clUpdate :: forall m. MonadIO m => QTableUpdate -> Readable m QTableRead -> m [QStorageUpdate]
    clUpdate (MkEditUpdate (QTableEditPropertySet p s mv)) _ =
        return [MkQStorageUpdate p s $ maybeToKnow mv]
    clUpdate _ _ = return []
    clPutEdits :: forall m. MonadIO m => [QStorageEdit] -> Readable m QTableRead -> m (Maybe [QTableEdit])
    clPutEdits = qTableEntityPutEdits
    in MkChangeLens{..}

qTableEntityReference :: Reference QTableEdit -> Reference QStorageEdit
qTableEntityReference = mapReference qTableEntityLens

type QTableUpdate = EditUpdate QTableEdit
