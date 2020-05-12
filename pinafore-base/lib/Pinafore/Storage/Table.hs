module Pinafore.Storage.Table
    ( Anchor
    , Predicate(..)
    , Entity(..)
    , PinaforeTableRead(..)
    , PinaforeTableEdit(..)
    , PinaforeTableUpdate
    , pinaforeTableEntityReference
    ) where

import Pinafore.Base
import Shapes
import Truth.Core
import Truth.Debug.Object

data PinaforeTableRead t where
    PinaforeTableReadGetPredicate :: Predicate -> Entity -> PinaforeTableRead (Maybe Entity)
    PinaforeTableReadLookupPredicate :: Predicate -> Entity -> PinaforeTableRead (FiniteSet Entity)
    PinaforeTableReadGetLiteral :: Entity -> PinaforeTableRead (Maybe Literal)

instance Show (PinaforeTableRead t) where
    show (PinaforeTableReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeTableReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeTableReadGetLiteral v) = "get literal of " ++ show v

instance WitnessConstraint Show PinaforeTableRead where
    witnessConstraint (PinaforeTableReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforeTableReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforeTableReadGetLiteral _) = Dict

instance AllWitnessConstraint Show PinaforeTableRead where
    allWitnessConstraint = Dict

data PinaforeTableEdit where
    PinaforeTableEditSetPredicate :: Predicate -> Entity -> Maybe Entity -> PinaforeTableEdit -- pred subj mval
    PinaforeTableEditSetLiteral :: Entity -> Maybe Literal -> PinaforeTableEdit

instance Show PinaforeTableEdit where
    show (PinaforeTableEditSetPredicate p s mv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (PinaforeTableEditSetLiteral v ml) = "set literal of " ++ show v ++ " to " ++ show ml

instance SubjectReader PinaforeTableRead where
    type ReaderSubject PinaforeTableRead = ([(Predicate, Entity, Entity)], [(Entity, Literal)])
    subjectToRead (triples, _) (PinaforeTableReadGetPredicate rp rs) =
        listToMaybe $ [v | (p, s, v) <- triples, p == rp && s == rs]
    subjectToRead (triples, _) (PinaforeTableReadLookupPredicate rp rv) =
        MkFiniteSet [s | (p, s, v) <- triples, p == rp, v == rv]
    subjectToRead (_, literals) (PinaforeTableReadGetLiteral rv) = listToMaybe [l | (v, l) <- literals, v == rv]

instance Floating PinaforeTableEdit PinaforeTableEdit

type instance EditReader PinaforeTableEdit = PinaforeTableRead

instance ApplicableEdit PinaforeTableEdit where
    applyEdit (PinaforeTableEditSetPredicate p s mv) _ (PinaforeTableReadGetPredicate p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeTableEditSetPredicate p s mv) mr (PinaforeTableReadLookupPredicate p' v')
        | p == p' = do
            fs <- mr $ PinaforeTableReadLookupPredicate p' v'
            return $
                case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (PinaforeTableEditSetLiteral v ml) _mr (PinaforeTableReadGetLiteral v')
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

instance SubjectMapEdit PinaforeTableEdit where
    mapSubjectEdits =
        mapEditToMapEdits $ \edit (oldtriples, oldliterals) ->
            case edit of
                PinaforeTableEditSetPredicate prd s mv -> let
                    match (prd', s', _)
                        | prd' == prd
                        , s == s' = Just (fmap (\v -> (prd, s, v)) mv, ())
                    match _ = Nothing
                    (newtriples', mu) = replaceFirst match oldtriples
                    newtriples =
                        case (mu, mv) of
                            (Nothing, Just v) -> (prd, s, v) : newtriples'
                            _ -> newtriples'
                    in return $ (newtriples, oldliterals)
                PinaforeTableEditSetLiteral v ml -> let
                    match (v', _)
                        | v == v' = Just (fmap (\l -> (v, l)) ml, ())
                    match _ = Nothing
                    (newliterals', mu) = replaceFirst match oldliterals
                    newliterals =
                        case (mu, ml) of
                            (Nothing, Just l) -> (v, l) : newliterals'
                            _ -> newliterals'
                    in return $ (oldtriples, newliterals)

instance InvertibleEdit PinaforeTableEdit where
    invertEdit (PinaforeTableEditSetPredicate p s _) mr = do
        mv <- mr $ PinaforeTableReadGetPredicate p s
        return [PinaforeTableEditSetPredicate p s mv]
    invertEdit (PinaforeTableEditSetLiteral v _) mr = do
        ml <- mr $ PinaforeTableReadGetLiteral v
        return [PinaforeTableEditSetLiteral v ml]

data EntityCacheKey cache t ct where
    GetEntityCacheKey :: EntityCacheKey cache t (cache (SimpleCacheKey Entity (Maybe t)))
    LookupEntityCacheKey :: EntityCacheKey cache t (cache (SimpleCacheKey t (FiniteSet Entity)))

instance Eq t => TestEquality (EntityCacheKey cache t) where
    testEquality GetEntityCacheKey GetEntityCacheKey = Just Refl
    testEquality LookupEntityCacheKey LookupEntityCacheKey = Just Refl
    testEquality _ _ = Nothing

data PinaforeTableEditCacheKey cache ct where
    PredicatePinaforeTableEditCacheKey
        :: Predicate -> PinaforeTableEditCacheKey cache (cache (EntityCacheKey cache Entity))
    LiteralPinaforeTableEditCacheKey :: PinaforeTableEditCacheKey cache (cache (EntityCacheKey cache Literal))

instance TestEquality (PinaforeTableEditCacheKey cache) where
    testEquality (PredicatePinaforeTableEditCacheKey p1) (PredicatePinaforeTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality LiteralPinaforeTableEditCacheKey LiteralPinaforeTableEditCacheKey = Just Refl
    testEquality _ _ = Nothing

instance CacheableEdit PinaforeTableEdit where
    type EditCacheKey cache PinaforeTableEdit = PinaforeTableEditCacheKey cache
    editCacheAdd (PinaforeTableReadGetPredicate p s) mv =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $
        subcacheModify GetEntityCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (PinaforeTableReadLookupPredicate p v) fs =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $
        subcacheModify LookupEntityCacheKey $ cacheAdd (MkSimpleCacheKey v) fs
    editCacheAdd (PinaforeTableReadGetLiteral s) mv =
        subcacheModify LiteralPinaforeTableEditCacheKey $
        subcacheModify GetEntityCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheLookup (PinaforeTableReadGetPredicate p s) cache = do
        subcache1 <- cacheLookup (PredicatePinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup GetEntityCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheLookup (PinaforeTableReadLookupPredicate p v) cache = do
        subcache1 <- cacheLookup (PredicatePinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup LookupEntityCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
    editCacheLookup (PinaforeTableReadGetLiteral s) cache = do
        subcache1 <- cacheLookup LiteralPinaforeTableEditCacheKey cache
        subcache2 <- cacheLookup GetEntityCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheUpdate (PinaforeTableEditSetPredicate p s mv) =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $ do
            subcacheModify GetEntityCacheKey $ cacheModify (MkSimpleCacheKey s) $ Shapes.put $ Just mv
            subcacheModify LookupEntityCacheKey $
                cacheTraverse $ \(MkSimpleCacheKey v') ss' ->
                    return $
                    Just $
                    (if mv == Just v'
                         then insertItem
                         else deleteKey)
                        s
                        ss'
    editCacheUpdate (PinaforeTableEditSetLiteral v mt) =
        subcacheModify LiteralPinaforeTableEditCacheKey $ do
            subcacheModify GetEntityCacheKey $ cacheModify (MkSimpleCacheKey v) $ Shapes.put $ Just mt
            subcacheModify LookupEntityCacheKey $
                cacheTraverse $ \(MkSimpleCacheKey t') vv' ->
                    return $
                    Just $
                    (if mt == Just t'
                         then insertItem
                         else deleteKey)
                        v
                        vv'

pinaforeTableEntityReference :: Reference PinaforeTableEdit -> Reference PinaforeEntityEdit
pinaforeTableEntityReference (MkResource (trun :: ResourceRunner tt) (MkAReference tableRead tableMPush refCommitTask)) =
    case resourceRunnerUnliftAllDict trun of
        Dict ->
            case transStackDict @MonadIO @tt @IO of
                Dict ->
                    case transStackDict @MonadFail @tt @IO of
                        Dict -> let
                            tablePush :: NonEmpty PinaforeTableEdit -> EditSource -> ApplyStack tt IO ()
                            tablePush edits esrc = traceBracket "pinaforeTableEntityReference.tablePush" $ pushOrFail "can't push table edit" esrc $ tableMPush edits
                            refRead :: Readable (ApplyStack tt IO) PinaforeEntityRead
                            refRead (PinaforeEntityReadGetPredicate prd subj) =
                                fmap maybeToKnow $ tableRead $ PinaforeTableReadGetPredicate prd subj
                            refRead (PinaforeEntityReadGetProperty prd subj) = do
                                mval <- tableRead $ PinaforeTableReadGetPredicate prd subj
                                case mval of
                                    Just val -> return val
                                    Nothing -> do
                                        val <- newEntity
                                        tablePush
                                            (pure $ PinaforeTableEditSetPredicate prd subj $ Just val)
                                            noEditSource
                                        return val
                            refRead (PinaforeEntityReadLookupPredicate prd val) =
                                tableRead $ PinaforeTableReadLookupPredicate prd val
                            refRead (PinaforeEntityReadToLiteral p) = do
                                ml <- tableRead $ PinaforeTableReadGetLiteral p
                                return $ maybeToKnow ml
                            refEdit ::
                                   NonEmpty PinaforeEntityEdit
                                -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                            refEdit edits = traceBracket "pinaforeTableEntityReference.objEdit.examine" $
                                singleAlwaysEdit (\case
                                    PinaforeEntityEditSetPredicate p s kv ->
                                        \esrc -> traceBracket "pinaforeTableEntityReference.objEdit.predicate" $ tablePush (pure $ PinaforeTableEditSetPredicate p s $ knowToMaybe kv) esrc
                                    PinaforeEntityEditSetLiteral p kl ->
                                        \esrc -> traceBracket "pinaforeTableEntityReference.objEdit.literal" $ tablePush (pure $ PinaforeTableEditSetLiteral p $ knowToMaybe kl) esrc) edits
                            in MkResource trun MkAReference {..}

type PinaforeTableUpdate = EditUpdate PinaforeTableEdit
