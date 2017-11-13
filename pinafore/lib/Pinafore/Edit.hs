module Pinafore.Edit
    ( Predicate(..)
    , Point(..)
    , PinaforeRead(..)
    , PinaforeEdit(..)
    , PinaforeFunctionValue
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

import Data.Aeson (FromJSON)
import Data.Serialize as Serialize (Serialize(..))
import Data.UUID hiding (fromString, fromText, toText)
import Pinafore.AsText
import Shapes
import Truth.Core

newtype Predicate =
    MkPredicate UUID
    deriving (Eq, FromJSON)

instance Show Predicate where
    show (MkPredicate uuid) = '%' : show uuid

newtype Point =
    MkPoint UUID
    deriving (Eq, Random, FromJSON)

instance Show Point where
    show (MkPoint uuid) = '!' : show uuid

instance Serialize Point where
    put (MkPoint uuid) = Serialize.put (toByteString uuid)
    get = do
        bs <- Serialize.get
        case fromByteString bs of
            Just uuid -> return $ MkPoint uuid
            Nothing -> fail "deserialize bad UUID"

data PinaforeRead t where
    PinaforeReadGetValue :: Predicate -> Point -> PinaforeRead (Maybe Point)
    PinaforeReadLookupValue :: Predicate -> Point -> PinaforeRead (FiniteSet Point)
    PinaforeReadGetLiteral :: Point -> PinaforeRead (Maybe Text)
    PinaforeReadLookupLiteral :: Text -> PinaforeRead (FiniteSet Point)

instance Show (PinaforeRead t) where
    show (PinaforeReadGetValue p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeReadLookupValue p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeReadGetLiteral v) = "get literal of " ++ show v
    show (PinaforeReadLookupLiteral l) = "lookup literal for " ++ show l

data PinaforeEdit where
    PinaforeEditSetValue :: Predicate -> Point -> Maybe Point -> PinaforeEdit -- pred subj mval
    PinaforeEditSetLiteral :: Point -> Maybe Text -> PinaforeEdit

instance Show PinaforeEdit where
    show (PinaforeEditSetValue p s mv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (PinaforeEditSetLiteral v ml) = "set literal of " ++ show v ++ " to " ++ show ml

instance SubjectReader PinaforeRead where
    type ReaderSubject PinaforeRead = ([(Predicate, Point, Point)], [(Point, Text)])
    readFromSubject (triples, _) (PinaforeReadGetValue rp rs) =
        listToMaybe $ [v | (p, s, v) <- triples, p == rp && s == rs]
    readFromSubject (triples, _) (PinaforeReadLookupValue rp rv) =
        MkFiniteSet [s | (p, s, v) <- triples, p == rp, v == rv]
    readFromSubject (_, literals) (PinaforeReadGetLiteral rv) = listToMaybe [l | (v, l) <- literals, v == rv]
    readFromSubject (_, literals) (PinaforeReadLookupLiteral rl) = MkFiniteSet [v | (v, l) <- literals, l == rl]

instance Floating PinaforeEdit PinaforeEdit

instance Edit PinaforeEdit where
    type EditReader PinaforeEdit = PinaforeRead
    applyEdit (PinaforeEditSetValue p s mv) (PinaforeReadGetValue p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeEditSetValue p s mv) (PinaforeReadLookupValue p' v')
        | p == p' = do
            fs <- readable $ PinaforeReadLookupValue p' v'
            return $
                case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (PinaforeEditSetLiteral v ml) (PinaforeReadGetLiteral v')
        | v == v' = return ml
    applyEdit (PinaforeEditSetLiteral v ml) (PinaforeReadLookupLiteral l') = do
        fv <- readable $ PinaforeReadLookupLiteral l'
        return $
            case ml of
                Just l
                    | l == l' -> insertSet v fv
                _ -> deleteSet v fv
    applyEdit _ rt = readable rt

instance InvertibleEdit PinaforeEdit where
    invertEdit (PinaforeEditSetValue p s _) = do
        mv <- readable $ PinaforeReadGetValue p s
        return [PinaforeEditSetValue p s mv]
    invertEdit (PinaforeEditSetLiteral v _) = do
        ml <- readable $ PinaforeReadGetLiteral v
        return [PinaforeEditSetLiteral v ml]

type PinaforeLensValue = GeneralLens PinaforeEdit

data PinaforeFunctionMorphism a b = MkPinaforeFunctionMorphism
    { pfFuncRead :: a -> Readable PinaforeRead b
    , pfUpdate :: PinaforeEdit -> Bool
    }

instance Functor (PinaforeFunctionMorphism a) where
    fmap f (MkPinaforeFunctionMorphism fr u) = MkPinaforeFunctionMorphism (\a -> fmap f (fr a)) u

instance Applicative (PinaforeFunctionMorphism a) where
    pure b = MkPinaforeFunctionMorphism (\_ -> pure b) (\_ -> False)
    MkPinaforeFunctionMorphism fr1 u1 <*> MkPinaforeFunctionMorphism fr2 u2 =
        MkPinaforeFunctionMorphism (\a -> (fr1 a) <*> (fr2 a)) (\edit -> u1 edit || u2 edit)

instance Category PinaforeFunctionMorphism where
    id = let
        pfFuncRead = return
        pfUpdate _ = False
        in MkPinaforeFunctionMorphism {..}
    (MkPinaforeFunctionMorphism lbc ubc) . (MkPinaforeFunctionMorphism lab uab) = let
        lac a = do
            b <- lab a
            lbc b
        uac edit = uab edit || ubc edit
        in MkPinaforeFunctionMorphism lac uac

instance Arrow PinaforeFunctionMorphism where
    arr ab = let
        pfFuncRead a = return $ ab a
        pfUpdate _ = False
        in MkPinaforeFunctionMorphism {..}
    first (MkPinaforeFunctionMorphism bc u) =
        MkPinaforeFunctionMorphism
            (\(b, d) -> do
                 c <- bc b
                 return (c, d))
            u
    second = cfmap

instance Traversable f => CatFunctor PinaforeFunctionMorphism f where
    cfmap (MkPinaforeFunctionMorphism f u) = MkPinaforeFunctionMorphism (\fa -> for fa f) u

pfPointedEditFunction ::
       forall a b. PinaforeFunctionMorphism a b -> PointedEditFunction PinaforeEdit (WholeEdit a) (WholeEdit b)
pfPointedEditFunction (MkPinaforeFunctionMorphism f u) = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    getBFromA :: a -> Readable (ContextEditReader PinaforeEdit (WholeEdit a)) b
    getBFromA a = mapReadable (\rt -> readable $ MkTupleEditReader EditContext rt) $ f a
    getB :: Readable (ContextEditReader PinaforeEdit (WholeEdit a)) b
    getB = do
        a <- readable $ MkTupleEditReader EditContent ReadWhole
        getBFromA a
    editGet :: forall t. () -> WholeReader b t -> Readable (ContextEditReader PinaforeEdit (WholeEdit a)) t
    editGet () ReadWhole = getB
    editUpdate ::
           ContextEdit PinaforeEdit (WholeEdit a)
        -> ()
        -> Readable (ContextEditReader PinaforeEdit (WholeEdit a)) ((), [WholeEdit b])
    editUpdate (MkTupleEdit EditContext pinedit) ()
        | u pinedit = do
            b <- getB
            return $ pure [MkWholeEdit b]
    editUpdate (MkTupleEdit EditContext _) () = return $ pure []
    editUpdate (MkTupleEdit EditContent (MkWholeEdit a)) () = do
        b <- getBFromA a
        return $ pure [MkWholeEdit b]
    in MkPointedEditFunction $ MkEditFunction {..}

type PinaforeFunctionValue t = GeneralFunction PinaforeEdit (WholeEdit t)

applyPinaforeFunction :: PinaforeFunctionMorphism a b -> PinaforeFunctionValue a -> PinaforeFunctionValue b
applyPinaforeFunction pf = pointedMapGeneralFunction (pfPointedEditFunction pf)

lensFunctionValue ::
       (FullSubjectReader (EditReader edit), Edit edit)
    => PinaforeLensValue edit
    -> PinaforeFunctionValue (EditSubject edit)
lensFunctionValue lens = convertGeneralFunction <.> generalLensFunction lens

data PinaforeLensMorphism a b = MkPinaforeLensMorphism
    { pmForward :: PointedEditLens PinaforeEdit (WholeEdit (Maybe a)) (WholeEdit (Maybe b))
    , pmInverse :: PinaforeFunctionMorphism b (FiniteSet a)
    }

instance Category PinaforeLensMorphism where
    id = MkPinaforeLensMorphism cid $ arr opoint
    (MkPinaforeLensMorphism fbc icb) . (MkPinaforeLensMorphism fab iba) =
        MkPinaforeLensMorphism (fbc <.> fab) $
        proc c -> do
            bb <- icb -< c
            aaa <- cfmap iba -< bb
            returnA -< mconcat $ toList aaa

funcPinaforeLensMorphism :: (a -> Maybe b) -> (b -> FiniteSet a) -> PinaforeLensMorphism a b
funcPinaforeLensMorphism amb bsa =
    MkPinaforeLensMorphism (readOnlyPointedEditLens $ funcPointedEditFunction $ \ma -> ma >>= amb) (arr bsa)

applyPinaforeLens ::
       PinaforeLensMorphism a b -> PinaforeLensValue (WholeEdit (Maybe a)) -> PinaforeLensValue (WholeEdit (Maybe b))
applyPinaforeLens pm = pointedMapGeneralLens $ pmForward pm

lensFunctionMorphism :: forall a b. PinaforeLensMorphism a b -> PinaforeFunctionMorphism a (Maybe b)
lensFunctionMorphism MkPinaforeLensMorphism {..} = let
    MkPinaforeFunctionMorphism _ pfUpdate = pmInverse
    pfFuncRead :: a -> Readable PinaforeRead (Maybe b)
    pfFuncRead a = pointedWholeFunctionRead (pointedEditLensFunction pmForward) $ Just a
    in MkPinaforeFunctionMorphism {..}

lensInverseFunctionMorphism :: PinaforeLensMorphism a b -> PinaforeFunctionMorphism b (FiniteSet a)
lensInverseFunctionMorphism = pmInverse

pmInverseEditFunction ::
       forall a b. (Eq a, Eq b)
    => PinaforeLensMorphism a b
    -> PointedEditFunction PinaforeEdit (WholeEdit (Maybe b)) (FiniteSetEdit a)
pmInverseEditFunction pm = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    getFiniteSet :: Maybe b -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) (FiniteSet a)
    getFiniteSet Nothing = return mempty
    getFiniteSet (Just b) = mapReadable (tupleReadFunction EditContext) $ pfFuncRead (pmInverse pm) b
    editGet :: forall t. () -> FiniteSetReader a t -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) t
    editGet () KeyReadKeys = do
        mb <- readable $ MkTupleEditReader EditContent ReadWhole
        getFiniteSet mb
    editGet () (KeyReadItem a ReadWhole) =
        case pmForward pm of
            MkPointedEditLens (MkEditLens (MkEditFunction _ g _) _) -> do
                mb <-
                    mapReadable
                        (\rt ->
                             case rt of
                                 MkTupleEditReader EditContext pr -> readable $ MkTupleEditReader EditContext pr
                                 MkTupleEditReader EditContent ReadWhole -> return $ Just a) $
                    g () ReadWhole
                mb' <- readable $ MkTupleEditReader EditContent ReadWhole
                return $
                    case (mb, mb') of
                        (Just b, Just b')
                            | b == b' -> Just a
                        _ -> Nothing
    editUpdate ::
           ContextEdit PinaforeEdit (WholeEdit (Maybe b))
        -> ()
        -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) ((), [FiniteSetEdit a])
    editUpdate (MkTupleEdit EditContext pinedit) ()
        | pfUpdate (pmInverse pm) pinedit = do
            mb <- readable $ MkTupleEditReader EditContent ReadWhole
            aset <- getFiniteSet mb
            aedits <- getReplaceEditsM aset
            return $ pure aedits
    editUpdate (MkTupleEdit EditContext _) () = return $ pure []
    editUpdate (MkTupleEdit EditContent (MkWholeEdit mb)) () = do
        aset <- getFiniteSet mb
        aedits <- getReplaceEditsM aset
        return $ pure aedits
    in MkPointedEditFunction $ MkEditFunction {..}

pmInverseEditLens ::
       forall a b. (Eq a, Eq b)
    => PinaforeLensMorphism a b
    -> PointedEditLens PinaforeEdit (WholeEdit (Maybe b)) (FiniteSetEdit a)
pmInverseEditLens pm@MkPinaforeLensMorphism {..} = let
    editLensFunction :: PureEditFunction (ContextEdit PinaforeEdit (WholeEdit (Maybe b))) (FiniteSetEdit a)
    MkPointedEditFunction editLensFunction = pmInverseEditFunction pm
    putEditBA ::
           ()
        -> WholeEdit (Maybe b)
        -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe a))) (Maybe ( ()
                                                                                  , [ContextEdit PinaforeEdit (WholeEdit (Maybe a))]))
    MkPointedEditLens (MkEditLens _ putEditBA) = pmForward
    putEditAB :: a -> Maybe b -> Readable PinaforeRead (Maybe [PinaforeEdit])
    putEditAB a mb = do
        medits <-
            mapReadable
                (\case
                     MkTupleEditReader EditContext rt -> readable rt
                     MkTupleEditReader EditContent ReadWhole -> return $ Just a) $
            putEditBA () (MkWholeEdit mb)
        return $
            fmap
                (\((), edits) ->
                     mapMaybe
                         (\case
                              MkTupleEdit EditContext edit -> Just edit
                              MkTupleEdit EditContent _ -> Nothing)
                         edits)
                medits
    mapContextReadable ::
           forall t. Readable PinaforeRead t -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) t
    mapContextReadable = mapReadable (readable . MkTupleEditReader EditContext)
    editLensPutEdit ::
           ()
        -> FiniteSetEdit a
        -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe b))) (Maybe ( ()
                                                                                  , [ContextEdit PinaforeEdit (WholeEdit (Maybe b))]))
    editLensPutEdit () (KeyEditItem _ edit) = never edit
    editLensPutEdit () (KeyDeleteItem a) = do
        mpedits <- mapContextReadable $ putEditAB a Nothing
        return $ fmap (\pedits -> pure $ fmap (MkTupleEdit EditContext) pedits) mpedits
    editLensPutEdit () (KeyInsertReplaceItem a) = do
        mb <- readable $ MkTupleEditReader EditContent ReadWhole
        case mb of
            Just _ -> do
                mpedits <- mapContextReadable $ putEditAB a mb
                return $ fmap (\pedits -> pure $ fmap (MkTupleEdit EditContext) pedits) mpedits
            Nothing -> return Nothing
    editLensPutEdit () KeyClear = do
        mb <- readable $ MkTupleEditReader EditContent ReadWhole
        case mb of
            Just b -> do
                aa <- mapContextReadable $ pfFuncRead pmInverse b
                lmpedits <- for (toList aa) $ \a -> mapContextReadable $ putEditAB a Nothing
                return $ fmap (\lpedits -> pure $ fmap (MkTupleEdit EditContext) $ mconcat lpedits) $ sequenceA lmpedits
            Nothing -> return Nothing
    in MkPointedEditLens $ MkEditLens {..}

applyInversePinaforeLens ::
       (Eq a, Eq b)
    => PinaforeLensMorphism a b
    -> PinaforeLensValue (WholeEdit (Maybe b))
    -> PinaforeLensValue (FiniteSetEdit a)
applyInversePinaforeLens pm val = pointedMapGeneralLens (pmInverseEditLens pm) val

literalPinaforeMap ::
       forall val. AsText val
    => PointedEditLens PinaforeEdit (WholeEdit (Maybe Point)) (WholeEdit (Maybe val))
literalPinaforeMap =
    MkPointedEditLens $ let
        editAccess :: IOStateAccess ()
        editAccess = unitStateAccess
        editGet ::
               forall t.
               ()
            -> WholeReader (Maybe val) t
            -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) t
        editGet () ReadWhole = do
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole
            case msubj of
                Just subj -> do
                    mbs <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetLiteral subj
                    return $ mbs >>= fromText
                Nothing -> return Nothing
        editUpdate ::
               (ContextEdit PinaforeEdit (WholeEdit (Maybe Point)))
            -> ()
            -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) ((), [WholeEdit (Maybe val)])
        editUpdate (MkTupleEdit EditContext (PinaforeEditSetLiteral s mbs)) () = do
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole
            return $
                pure $
                if Just s == msubj
                    then [MkWholeEdit $ mbs >>= fromText]
                    else []
        editUpdate (MkTupleEdit EditContext _) () = return $ pure []
        editUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) () = return $ pure [MkWholeEdit Nothing]
        editUpdate (MkTupleEdit EditContent (MkWholeEdit (Just subj))) () = do
            mbs <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetLiteral subj
            return $ pure $ [MkWholeEdit $ mbs >>= fromText]
        editLensFunction = MkEditFunction {..}
        editLensPutEdit ::
               ()
            -> WholeEdit (Maybe val)
            -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (Maybe ( ()
                                                                                          , [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))]))
        editLensPutEdit () (MkWholeEdit (fmap toText -> mbs)) = do
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole
            case msubj of
                Just subj -> return $ pure $ pure [MkTupleEdit EditContext $ PinaforeEditSetLiteral subj mbs]
                Nothing -> do
                    subj <- liftIO randomIO
                    return $
                        pure $
                        pure
                            [ MkTupleEdit EditContent $ MkWholeEdit $ Just subj
                            , MkTupleEdit EditContext $ PinaforeEditSetLiteral subj mbs
                            ]
        in MkEditLens {..}

literalInverseFunction ::
       forall val. AsText val
    => PinaforeFunctionMorphism val (FiniteSet Point)
literalInverseFunction = let
    pfFuncRead :: val -> Readable PinaforeRead (FiniteSet Point)
    pfFuncRead val = readable $ PinaforeReadLookupLiteral $ toText val
    pfUpdate :: PinaforeEdit -> Bool
    pfUpdate (PinaforeEditSetLiteral _ _) = True
    pfUpdate _ = False
    in MkPinaforeFunctionMorphism {..}

literalPinaforeLensMorphism ::
       forall val. AsText val
    => PinaforeLensMorphism Point val
literalPinaforeLensMorphism = MkPinaforeLensMorphism literalPinaforeMap literalInverseFunction

predicatePinaforeMap :: Predicate -> PointedEditLens PinaforeEdit (WholeEdit (Maybe Point)) (WholeEdit (Maybe Point))
predicatePinaforeMap prd =
    MkPointedEditLens $ let
        editAccess :: IOStateAccess ()
        editAccess = unitStateAccess
        editGet ::
               forall t.
               ()
            -> WholeReader (Maybe Point) t
            -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) t
        editGet () ReadWhole = do
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole
            case msubj of
                Just subj -> readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj
                Nothing -> return Nothing
        editUpdate ::
               (ContextEdit PinaforeEdit (WholeEdit (Maybe Point)))
            -> ()
            -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) ((), [WholeEdit (Maybe Point)])
        editUpdate (MkTupleEdit EditContext (PinaforeEditSetValue p s mv)) ()
            | p == prd = do
                msubj <- readable $ MkTupleEditReader EditContent ReadWhole
                return $
                    pure $
                    if Just s == msubj
                        then [MkWholeEdit mv]
                        else []
        editUpdate (MkTupleEdit EditContext _) () = return $ pure []
        editUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) () = return $ pure [MkWholeEdit Nothing]
        editUpdate (MkTupleEdit EditContent (MkWholeEdit (Just subj))) () = do
            mval <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj
            return $ pure $ [MkWholeEdit mval]
        editLensFunction = MkEditFunction {..}
        editLensPutEdit ::
               ()
            -> WholeEdit (Maybe Point)
            -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (Maybe ( ()
                                                                                          , [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))]))
        editLensPutEdit () (MkWholeEdit mv) = do
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole
            case msubj of
                Just subj -> return $ pure $ pure [MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv]
                Nothing -> do
                    subj <- liftIO randomIO
                    return $
                        pure $
                        pure
                            [ MkTupleEdit EditContent $ MkWholeEdit $ Just subj
                            , MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv
                            ]
        in MkEditLens {..}

predicateInverseFunction :: Predicate -> PinaforeFunctionMorphism Point (FiniteSet Point)
predicateInverseFunction prd = let
    pfFuncRead :: Point -> Readable PinaforeRead (FiniteSet Point)
    pfFuncRead val = readable $ PinaforeReadLookupValue prd val
    pfUpdate :: PinaforeEdit -> Bool
    pfUpdate (PinaforeEditSetValue p _ _)
        | p == prd = True
    pfUpdate _ = False
    in MkPinaforeFunctionMorphism {..}

predicatePinaforeLensMorphism :: Predicate -> PinaforeLensMorphism Point Point
predicatePinaforeLensMorphism prd = MkPinaforeLensMorphism (predicatePinaforeMap prd) (predicateInverseFunction prd)
