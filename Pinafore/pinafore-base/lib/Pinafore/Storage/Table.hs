module Pinafore.Storage.Table
    ( Anchor
    , Predicate(..)
    , Entity(..)
    , RefCount
    , PinaforeTableSubject(..)
    , PinaforeTableRead(..)
    , PinaforeTableEdit(..)
    , PinaforeTableUpdate
    , pinaforeTableEntityReference
    ) where

import Changes.Core
import Pinafore.Base
import Shapes

type RefCount = Int

data PinaforeTableRead t where
    PinaforeTableReadPropertyGet :: Predicate -> Entity -> PinaforeTableRead (Maybe Entity)
    PinaforeTableReadPropertyLookup :: Predicate -> Entity -> PinaforeTableRead (FiniteSet Entity)
    PinaforeTableReadEntityRefCount :: Entity -> PinaforeTableRead (Maybe RefCount)
    PinaforeTableReadFactGet :: Predicate -> Entity -> PinaforeTableRead (Maybe Entity)
    PinaforeTableReadLiteralGet :: Entity -> PinaforeTableRead (Maybe Literal)

instance Show (PinaforeTableRead t) where
    show (PinaforeTableReadPropertyGet p s) = "prop get " ++ show p ++ " of " ++ show s
    show (PinaforeTableReadPropertyLookup p v) = "prop lookup " ++ show p ++ " for " ++ show v
    show (PinaforeTableReadEntityRefCount v) = "entity ref count " ++ show v
    show (PinaforeTableReadFactGet p s) = "fact get " ++ show p ++ " of " ++ show s
    show (PinaforeTableReadLiteralGet v) = "literal get " ++ show v

instance WitnessConstraint Show PinaforeTableRead where
    witnessConstraint (PinaforeTableReadPropertyGet _ _) = Dict
    witnessConstraint (PinaforeTableReadPropertyLookup _ _) = Dict
    witnessConstraint (PinaforeTableReadEntityRefCount _) = Dict
    witnessConstraint (PinaforeTableReadFactGet _ _) = Dict
    witnessConstraint (PinaforeTableReadLiteralGet _) = Dict

instance AllWitnessConstraint Show PinaforeTableRead where
    allWitnessConstraint = Dict

data PinaforeTableEdit where
    PinaforeTableEditPropertySet :: Predicate -> Entity -> Maybe Entity -> PinaforeTableEdit -- pred subj mval
    PinaforeTableEditEntityRefCount :: Entity -> Maybe RefCount -> PinaforeTableEdit -- pred subj mval
    PinaforeTableEditFactSet :: Predicate -> Entity -> Maybe Entity -> PinaforeTableEdit -- pred subj mval
    PinaforeTableEditLiteralSet :: Entity -> Maybe Literal -> PinaforeTableEdit

instance Show PinaforeTableEdit where
    show (PinaforeTableEditPropertySet p s mv) = "prop set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (PinaforeTableEditEntityRefCount v rc) = "entity count " ++ show v ++ " to " ++ show rc
    show (PinaforeTableEditFactSet p s v) = "fact set " ++ show p ++ " of " ++ show s ++ " is " ++ show v
    show (PinaforeTableEditLiteralSet v l) = "literal set " ++ show v ++ " is " ++ show l

data PinaforeTableSubject = MkPinaforeTableSubject
    { ptsPredicates :: [(Predicate, Entity, Entity)]
    , ptsRefCounts :: [(Entity, RefCount)]
    , ptsFacts :: [(Predicate, Entity, Entity)]
    , ptsLiterals :: [(Entity, Literal)]
    }

instance SubjectReader PinaforeTableRead where
    type ReaderSubject PinaforeTableRead = PinaforeTableSubject
    subjectToRead MkPinaforeTableSubject {..} (PinaforeTableReadPropertyGet rp rs) =
        listToMaybe $ [v | (p, s, v) <- ptsPredicates, p == rp && s == rs]
    subjectToRead MkPinaforeTableSubject {..} (PinaforeTableReadPropertyLookup rp rv) =
        MkFiniteSet [s | (p, s, v) <- ptsPredicates, p == rp, v == rv]
    subjectToRead MkPinaforeTableSubject {..} (PinaforeTableReadEntityRefCount rv) =
        listToMaybe $ [c | (v, c) <- ptsRefCounts, v == rv]
    subjectToRead MkPinaforeTableSubject {..} (PinaforeTableReadFactGet rp rs) =
        listToMaybe $ [v | (p, s, v) <- ptsFacts, p == rp && s == rs]
    subjectToRead MkPinaforeTableSubject {..} (PinaforeTableReadLiteralGet rv) =
        listToMaybe [l | (v, l) <- ptsLiterals, v == rv]

instance Floating PinaforeTableEdit PinaforeTableEdit

type instance EditReader PinaforeTableEdit = PinaforeTableRead

instance ApplicableEdit PinaforeTableEdit where
    applyEdit (PinaforeTableEditPropertySet p s mv) _ (PinaforeTableReadPropertyGet p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeTableEditPropertySet p s mv) mr (PinaforeTableReadPropertyLookup p' v')
        | p == p' = do
            fs <- mr $ PinaforeTableReadPropertyLookup p' v'
            return $
                case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (PinaforeTableEditEntityRefCount v mc) _ (PinaforeTableReadEntityRefCount v')
        | v == v' = return mc
    applyEdit (PinaforeTableEditFactSet p s mv) _ (PinaforeTableReadFactGet p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeTableEditLiteralSet v ml) _ (PinaforeTableReadLiteralGet v')
        | v == v' = return ml
    applyEdit _ mr rt = mr rt

replaceFirst :: (a -> Maybe (Maybe a, b)) -> [a] -> ([a], Maybe b)
replaceFirst _ [] = ([], Nothing)
replaceFirst f (a:aa)
    | Just (ma, b) <- f a =
        case ma of
            Just a' -> (a' : aa, Just b)
            Nothing -> (aa, Just b)
replaceFirst f (a:aa) =
    case replaceFirst f aa of
        (aa', mb) -> (a : aa', mb)

replaceOrAdd :: (a -> Bool) -> Maybe a -> [a] -> [a]
replaceOrAdd f mitem aa =
    case replaceFirst
             (\a ->
                  if f a
                      then Just (mitem, ())
                      else Nothing)
             aa of
        (aa', Just ()) -> aa'
        (aa', Nothing) ->
            case mitem of
                Nothing -> aa'
                Just a -> a : aa'

instance SubjectMapEdit PinaforeTableEdit where
    mapSubjectEdits =
        mapEditToMapEdits $ \edit (MkPinaforeTableSubject oldPredicates oldRefCounts oldFacts oldLiterals) ->
            case edit of
                PinaforeTableEditPropertySet prd (checkEntity "store.prop.s" -> s) (fmap (checkEntity "store.prop.v") -> mv) -> let
                    newPredicates =
                        replaceOrAdd
                            (\(prd', s', _) -> (prd' == prd) && (s == s'))
                            (fmap (\v -> (prd, s, v)) mv)
                            oldPredicates
                    in return $ MkPinaforeTableSubject newPredicates oldRefCounts oldFacts oldLiterals
                PinaforeTableEditEntityRefCount v mrc -> let
                    newRefCounts = replaceOrAdd (\(v', _) -> v == v') (fmap (\rc -> (v, rc)) mrc) oldRefCounts
                    in return $ MkPinaforeTableSubject oldPredicates newRefCounts oldFacts oldLiterals
                PinaforeTableEditFactSet prd (checkEntity "store.fact.s" -> s) (fmap (checkEntity "store.fact.v") -> mv) -> let
                    newFacts =
                        replaceOrAdd
                            (\(prd', s', _) -> (prd' == prd) && (s == s'))
                            (fmap (\v -> (prd, s, v)) mv)
                            oldFacts
                    in return $ MkPinaforeTableSubject oldPredicates oldRefCounts newFacts oldLiterals
                PinaforeTableEditLiteralSet (checkEntity "store.literal.v" -> v) ml -> let
                    newLiterals = replaceOrAdd (\(v', _) -> v == v') (fmap (\l -> (v, l)) ml) oldLiterals
                    in return $ MkPinaforeTableSubject oldPredicates oldRefCounts oldFacts newLiterals

instance InvertibleEdit PinaforeTableEdit where
    invertEdit (PinaforeTableEditPropertySet p s _) mr = do
        mv <- mr $ PinaforeTableReadPropertyGet p s
        return [PinaforeTableEditPropertySet p s mv]
    invertEdit (PinaforeTableEditEntityRefCount v _) mr = do
        mrc <- mr $ PinaforeTableReadEntityRefCount v
        return [PinaforeTableEditEntityRefCount v mrc]
    invertEdit (PinaforeTableEditFactSet p s _) mr = do
        mv <- mr $ PinaforeTableReadFactGet p s
        return [PinaforeTableEditFactSet p s mv]
    invertEdit (PinaforeTableEditLiteralSet v _) mr = do
        ml <- mr $ PinaforeTableReadLiteralGet v
        return [PinaforeTableEditLiteralSet v ml]

data PropertyCacheKey cache t ct where
    GetPropertyCacheKey :: PropertyCacheKey cache t (cache (SimpleCacheKey Entity (Maybe t)))
    LookupPropertyCacheKey :: PropertyCacheKey cache t (cache (SimpleCacheKey t (FiniteSet Entity)))

instance Eq t => TestEquality (PropertyCacheKey cache t) where
    testEquality GetPropertyCacheKey GetPropertyCacheKey = Just Refl
    testEquality LookupPropertyCacheKey LookupPropertyCacheKey = Just Refl
    testEquality _ _ = Nothing

data PinaforeTableEditCacheKey cache ct where
    PropertyPinaforeTableEditCacheKey
        :: Predicate -> PinaforeTableEditCacheKey cache (cache (PropertyCacheKey cache Entity))
    RefCountPinaforeTableEditCacheKey
        :: PinaforeTableEditCacheKey cache (cache (SimpleCacheKey Entity (Maybe RefCount)))
    FactPinaforeTableEditCacheKey
        :: Predicate -> PinaforeTableEditCacheKey cache (cache (SimpleCacheKey Entity (Maybe Entity)))
    LiteralPinaforeTableEditCacheKey :: PinaforeTableEditCacheKey cache (cache (SimpleCacheKey Entity (Maybe Literal)))

instance TestEquality (PinaforeTableEditCacheKey cache) where
    testEquality (PropertyPinaforeTableEditCacheKey p1) (PropertyPinaforeTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality RefCountPinaforeTableEditCacheKey RefCountPinaforeTableEditCacheKey = Just Refl
    testEquality (FactPinaforeTableEditCacheKey p1) (FactPinaforeTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality LiteralPinaforeTableEditCacheKey LiteralPinaforeTableEditCacheKey = Just Refl
    testEquality _ _ = Nothing

instance CacheableEdit PinaforeTableEdit where
    type EditCacheKey cache PinaforeTableEdit = PinaforeTableEditCacheKey cache
    editCacheAdd (PinaforeTableReadPropertyGet p s) mv =
        subcacheModify (PropertyPinaforeTableEditCacheKey p) $
        subcacheModify GetPropertyCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (PinaforeTableReadPropertyLookup p v) fs =
        subcacheModify (PropertyPinaforeTableEditCacheKey p) $
        subcacheModify LookupPropertyCacheKey $ cacheAdd (MkSimpleCacheKey v) fs
    editCacheAdd (PinaforeTableReadEntityRefCount v) mv =
        subcacheModify RefCountPinaforeTableEditCacheKey $ cacheAdd (MkSimpleCacheKey v) mv
    editCacheAdd (PinaforeTableReadFactGet p s) mv =
        subcacheModify (FactPinaforeTableEditCacheKey p) $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (PinaforeTableReadLiteralGet s) mv =
        subcacheModify LiteralPinaforeTableEditCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheLookup (PinaforeTableReadPropertyGet p s) cache = do
        subcache1 <- cacheLookup (PropertyPinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup GetPropertyCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheLookup (PinaforeTableReadPropertyLookup p v) cache = do
        subcache1 <- cacheLookup (PropertyPinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup LookupPropertyCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
    editCacheLookup (PinaforeTableReadEntityRefCount v) cache = do
        subcache1 <- cacheLookup RefCountPinaforeTableEditCacheKey cache
        cacheLookup (MkSimpleCacheKey v) subcache1
    editCacheLookup (PinaforeTableReadFactGet p s) cache = do
        subcache1 <- cacheLookup (FactPinaforeTableEditCacheKey p) cache
        cacheLookup (MkSimpleCacheKey s) subcache1
    editCacheLookup (PinaforeTableReadLiteralGet s) cache = do
        subcache1 <- cacheLookup LiteralPinaforeTableEditCacheKey cache
        cacheLookup (MkSimpleCacheKey s) subcache1
    editCacheUpdate (PinaforeTableEditPropertySet p s mv) =
        subcacheModify (PropertyPinaforeTableEditCacheKey p) $ do
            subcacheModify GetPropertyCacheKey $ cacheModify (MkSimpleCacheKey s) $ Shapes.put $ Just mv
            subcacheModify LookupPropertyCacheKey $
                cacheTraverse $ \(MkSimpleCacheKey v') ss' ->
                    return $
                    Just $
                    (if mv == Just v'
                         then insertItem
                         else deleteKey)
                        s
                        ss'
    editCacheUpdate (PinaforeTableEditEntityRefCount v t) =
        subcacheModify RefCountPinaforeTableEditCacheKey $ do cacheModify (MkSimpleCacheKey v) $ Shapes.put $ Just t
    editCacheUpdate (PinaforeTableEditFactSet prd s t) =
        subcacheModify (FactPinaforeTableEditCacheKey prd) $ do cacheModify (MkSimpleCacheKey s) $ Shapes.put $ Just t
    editCacheUpdate (PinaforeTableEditLiteralSet v t) =
        subcacheModify LiteralPinaforeTableEditCacheKey $ do cacheModify (MkSimpleCacheKey v) $ Shapes.put $ Just t

-- can't be a Lens, because reads can cause edits
pinaforeTableEntityReference :: Reference PinaforeTableEdit -> Reference PinaforeStorageEdit
pinaforeTableEntityReference (MkResource (trun :: ResourceRunner tt) (MkAReference tableRead tableMPush refCommitTask)) =
    case resourceRunnerUnliftAllDict trun of
        Dict ->
            case transStackDict @MonadIO @tt @IO of
                Dict ->
                    case transStackDict @MonadFail @tt @IO of
                        Dict -> let
                            tablePush :: EditSource -> PinaforeTableEdit -> ApplyStack tt IO ()
                            tablePush esrc edit = pushOrFail "can't push table edit" esrc $ tableMPush $ pure edit
                            acquireEntity :: EditSource -> Entity -> ApplyStack tt IO ()
                            acquireEntity esrc entity = do
                                mrc <- tableRead $ PinaforeTableReadEntityRefCount entity
                                newrc <-
                                    return $
                                    case mrc of
                                        Nothing -> 1
                                        Just oldrc -> succ oldrc
                                tablePush esrc $ PinaforeTableEditEntityRefCount entity $ Just newrc
                            releaseByFact ::
                                   forall t. EditSource -> FieldStorer 'MultipleMode t -> Entity -> ApplyStack tt IO ()
                            releaseByFact esrc (MkFieldStorer p subdef) entity = do
                                msubv <- tableRead $ PinaforeTableReadFactGet p entity
                                for_ msubv $ \subv -> releaseByEntity esrc subdef subv
                                tablePush esrc $ PinaforeTableEditFactSet p entity Nothing
                            releaseByConstructor ::
                                   forall t.
                                   EditSource
                                -> ConstructorStorer 'MultipleMode t
                                -> Entity
                                -> ApplyStack tt IO ()
                            releaseByConstructor _ PlainConstructorStorer _ = return ()
                            releaseByConstructor _ LiteralConstructorStorer entity
                                | Just _ <- entityToLiteral entity = return ()
                            releaseByConstructor esrc LiteralConstructorStorer entity =
                                tablePush esrc $ PinaforeTableEditLiteralSet entity Nothing
                            releaseByConstructor esrc (ConstructorConstructorStorer _ facts) entity =
                                listTypeFor_ facts $ \fact -> releaseByFact esrc fact entity
                            releaseByEntity ::
                                   forall t. EditSource -> EntityStorer 'MultipleMode t -> Entity -> ApplyStack tt IO ()
                            releaseByEntity esrc (MkEntityStorer css) entity = do
                                mrc <- tableRead $ PinaforeTableReadEntityRefCount entity
                                case mrc of
                                    Just 1 -> do
                                        tablePush esrc $ PinaforeTableEditEntityRefCount entity Nothing
                                        for_ css $ \(MkKnowShim def _) -> releaseByConstructor esrc def entity
                                    Just oldrc ->
                                        tablePush esrc $ PinaforeTableEditEntityRefCount entity $ Just $ pred oldrc
                                    Nothing -> return ()
                            releaseByAdapter :: forall t. EditSource -> EntityAdapter t -> Entity -> ApplyStack tt IO ()
                            releaseByAdapter esrc vtype entity =
                                releaseByEntity esrc (entityAdapterDefinitions vtype) entity
                            setFact ::
                                   forall (t :: Type).
                                   EditSource
                                -> FieldStorer 'SingleMode t
                                -> Entity
                                -> t
                                -> ApplyStack tt IO ()
                            setFact esrc (MkFieldStorer p subdef) v t = do
                                let subv = entityStorerToEntity subdef t
                                moldsub <- tableRead $ PinaforeTableReadFactGet p v
                                case moldsub of
                                    Just _ -> return ()
                                    Nothing -> do
                                        tablePush esrc $ PinaforeTableEditFactSet p v $ Just subv
                                        acquireEntity esrc subv
                                setEntity esrc subdef subv t
                            setFacts ::
                                   forall (t :: [Type]).
                                   EditSource
                                -> ListType (FieldStorer 'SingleMode) t
                                -> Entity
                                -> HList t
                                -> ApplyStack tt IO ()
                            setFacts _ NilListType _ () = return ()
                            setFacts esrc (ConsListType f1 fr) v (a1, ar) = do
                                setFact esrc f1 v a1
                                setFacts esrc fr v ar
                            setConstructor ::
                                   forall (t :: Type).
                                   EditSource
                                -> ConstructorStorer 'SingleMode t
                                -> Entity
                                -> t
                                -> ApplyStack tt IO ()
                            setConstructor _ PlainConstructorStorer _ _ = return ()
                            setConstructor _ LiteralConstructorStorer v _
                                | Just _ <- entityToLiteral v = return ()
                            setConstructor esrc LiteralConstructorStorer v l =
                                tablePush esrc $ PinaforeTableEditLiteralSet v $ Just l
                            setConstructor esrc (ConstructorConstructorStorer _ facts) v t = setFacts esrc facts v t
                            setEntity ::
                                   forall (t :: Type).
                                   EditSource
                                -> EntityStorer 'SingleMode t
                                -> Entity
                                -> t
                                -> ApplyStack tt IO ()
                            setEntity esrc (MkEntityStorer cs) e t = setConstructor esrc cs e t
                            setEntityFromAdapter :: EditSource -> Entity -> EntityAdapter t -> t -> ApplyStack tt IO ()
                            setEntityFromAdapter esrc entity ea t = do
                                case entityAdapterToDefinition ea t of
                                    MkAnyValue def tt -> setEntity esrc def entity tt
                            doEntityEdit :: EditSource -> PinaforeStorageEdit -> ApplyStack tt IO ()
                            doEntityEdit esrc (MkPinaforeStorageEdit stype vtype p s (Known v)) = do
                                let
                                    se = entityAdapterConvert stype s
                                    ve = entityAdapterConvert vtype v
                                mroldv <- tableRead $ PinaforeTableReadPropertyGet p se
                                case mroldv of
                                    Just oldv
                                        | oldv == ve -> return ()
                                    Nothing -> do
                                        setEntityFromAdapter esrc se stype s
                                        acquireEntity esrc se
                                        setEntityFromAdapter esrc ve vtype v
                                        acquireEntity esrc ve
                                    Just oldv -> do
                                        setEntityFromAdapter esrc ve vtype v
                                        acquireEntity esrc ve
                                        releaseByAdapter esrc vtype oldv
                                tablePush esrc $ PinaforeTableEditPropertySet p se $ Just ve
                            doEntityEdit esrc (MkPinaforeStorageEdit stype vtype p s Unknown) = do
                                let se = entityAdapterConvert stype s
                                mroldv <- tableRead $ PinaforeTableReadPropertyGet p se
                                case mroldv of
                                    Nothing -> return ()
                                    Just oldv -> do
                                        releaseByAdapter esrc stype se
                                        releaseByAdapter esrc vtype oldv
                                        tablePush esrc $ PinaforeTableEditPropertySet p se Nothing
                            readFact ::
                                   forall (t :: Type).
                                   FieldStorer 'MultipleMode t
                                -> Entity
                                -> ComposeInner Know (ApplyStack tt IO) t
                            readFact (MkFieldStorer p subdef) entity = do
                                subentity <-
                                    MkComposeInner $ fmap maybeToKnow $ tableRead $ PinaforeTableReadFactGet p entity
                                readEntity subdef subentity
                            readFacts ::
                                   forall (t :: [Type]).
                                   ListType (FieldStorer 'MultipleMode) t
                                -> Entity
                                -> ComposeInner Know (ApplyStack tt IO) (HList t)
                            readFacts NilListType _ = return ()
                            readFacts (ConsListType f1 fr) entity = do
                                t1 <- readFact f1 entity
                                tr <- readFacts fr entity
                                return (t1, tr)
                            readConstructor ::
                                   forall (t :: Type).
                                   ConstructorStorer 'MultipleMode t
                                -> Entity
                                -> ComposeInner Know (ApplyStack tt IO) t
                            readConstructor PlainConstructorStorer entity = return entity
                            readConstructor LiteralConstructorStorer entity
                                | Just lit <- entityToLiteral entity = return lit
                            readConstructor LiteralConstructorStorer entity =
                                MkComposeInner $ fmap maybeToKnow $ tableRead $ PinaforeTableReadLiteralGet entity
                            readConstructor (ConstructorConstructorStorer _ facts) entity = readFacts facts entity
                            firstKnown :: MonadPlus m => [a] -> (a -> m b) -> m b
                            firstKnown [] _ = empty
                            firstKnown (a:aa) f = f a <|> firstKnown aa f
                            readEntity ::
                                   forall (t :: Type).
                                   EntityStorer 'MultipleMode t
                                -> Entity
                                -> ComposeInner Know (ApplyStack tt IO) t
                            readEntity (MkEntityStorer css) entity =
                                firstKnown css $ \(MkKnowShim def f) -> do
                                    dt <- readConstructor def entity
                                    liftInner $ f dt
                            refRead :: Readable (ApplyStack tt IO) PinaforeStorageRead
                            refRead (PinaforeStorageReadGet stype prd subj) = do
                                mval <- tableRead $ PinaforeTableReadPropertyGet prd $ entityAdapterConvert stype subj
                                case mval of
                                    Just val -> return val
                                    Nothing -> do
                                        val <- newEntity
                                        doEntityEdit noEditSource $
                                            MkPinaforeStorageEdit stype plainEntityAdapter prd subj (Known val)
                                        return val
                            refRead (PinaforeStorageReadLookup prd val) =
                                tableRead $ PinaforeTableReadPropertyLookup prd val
                            refRead (PinaforeStorageReadEntity ea entity) =
                                getComposeInner $ readEntity (entityAdapterDefinitions ea) entity
                            refEdit ::
                                   NonEmpty PinaforeStorageEdit
                                -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                            refEdit = singleAlwaysEdit $ \edit esrc -> doEntityEdit esrc edit
                            in MkResource trun MkAReference {..}

type PinaforeTableUpdate = EditUpdate PinaforeTableEdit
