module Pinafore.Language.Set where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.NamedEntity
import Pinafore.Language.Reference
import Pinafore.Storage.Table (Entity)
import Shapes
import Truth.Core

data PinaforeSet baseedit pq where
    MkPinaforeSet :: Eq t => Range t pq -> PinaforeLensValue baseedit (FiniteSetEdit t) -> PinaforeSet baseedit pq

unPinaforeSet :: PinaforeSet baseedit '( p, p) -> PinaforeLensValue baseedit (FiniteSetEdit p)
unPinaforeSet (MkPinaforeSet tr lv) = (bijectionFiniteSetEditLens $ rangeBijection tr) . lv

instance IsoMapRange (PinaforeSet baseedit)

instance MapRange (PinaforeSet baseedit) where
    mapRange f (MkPinaforeSet r s) = MkPinaforeSet (mapRange f r) s

instance HasDolanVary '[ 'Rangevariance] (PinaforeSet baseedit) where
    dolanVary =
        ConsDolanKindVary (mkRangevary $ \mapr (MkPinaforeSet range lval) -> MkPinaforeSet (mapr range) lval) $
        NilDolanKindVary

pinaforeSetValue :: PinaforeSet baseedit '( q, q) -> PinaforeLensValue baseedit (FiniteSetEdit q)
pinaforeSetValue (MkPinaforeSet tr lv) = bijectionFiniteSetEditLens (rangeBijection tr) . lv

valuePinaforeSet :: Eq q => PinaforeLensValue baseedit (FiniteSetEdit q) -> PinaforeSet baseedit '( q, q)
valuePinaforeSet lv = MkPinaforeSet identityRange lv

pinaforeSetMeetValue ::
       PinaforeSet baseedit '( t, MeetType Entity t) -> PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t))
pinaforeSetMeetValue (MkPinaforeSet tr lv) = pinaforeSetValue $ MkPinaforeSet (unifyRange2 meet2 tr) lv

meetValuePinaforeSet ::
       PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t)) -> PinaforeSet baseedit '( MeetType Entity t, t)
meetValuePinaforeSet lv = MkPinaforeSet (unUnifyRange1 meet2 identityRange) lv

pinaforeSetMeet ::
       forall baseedit t.
       PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( MeetType Entity t, t)
pinaforeSetMeet seta setb =
    meetValuePinaforeSet $
    readOnlyEditLens meetEditFunction . pairCombineEditLenses (pinaforeSetMeetValue seta) (pinaforeSetMeetValue setb)

pinaforeSetJoin ::
       forall baseedit t.
       PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( MeetType Entity t, t)
pinaforeSetJoin seta setb =
    meetValuePinaforeSet $
    readOnlyEditLens joinEditFunction . pairCombineEditLenses (pinaforeSetMeetValue seta) (pinaforeSetMeetValue setb)

pinaforeSetAdd :: PinaforeSet baseedit '( p, q) -> p -> PinaforeAction baseedit ()
pinaforeSetAdd (MkPinaforeSet tr set) p = pinaforeLensPush set [KeyInsertReplaceItem $ rangeContra tr p]

pinaforeSetAddNew :: PinaforeSet baseedit '( NewEntity, TopType) -> PinaforeAction baseedit NewEntity
pinaforeSetAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeSetAdd set e
    return e

pinaforeSetRemove :: PinaforeSet baseedit '( p, q) -> p -> PinaforeAction baseedit ()
pinaforeSetRemove (MkPinaforeSet tr set) p = pinaforeLensPush set [KeyDeleteItem $ rangeContra tr p]

pinaforeSetRemoveAll :: PinaforeSet baseedit '( BottomType, TopType) -> PinaforeAction baseedit ()
pinaforeSetRemoveAll (MkPinaforeSet _ set) = pinaforeLensPush set [KeyClear]

pinaforeSetFunctionValue :: PinaforeSet baseedit '( t, a) -> PinaforeFunctionValue baseedit (FiniteSet a)
pinaforeSetFunctionValue (MkPinaforeSet tr set) = funcEditFunction (fmap $ rangeCo tr) . lensFunctionValue set

pinaforeSetMembership ::
       PinaforeSet baseedit '( BottomType, Entity) -> PinaforeReference baseedit '( TopType, Entity -> Bool)
pinaforeSetMembership set =
    pinaforeFunctionToReference $ funcEditFunction (\s -> Known $ \p -> member p s) . pinaforeSetFunctionValue set

pinaforeSetContains ::
       PinaforeSet baseedit '( a, TopType)
    -> PinaforeReference baseedit '( BottomType, a)
    -> PinaforeReference baseedit '( TopType, Bool)
pinaforeSetContains (MkPinaforeSet (rangeContra -> conv) (editLensFunction -> setf)) (pinaforeReferenceToFunction -> fva) = let
    containsEditFunction ::
           forall a b. Eq b
        => (a -> b)
        -> EditFunction (PairEdit (FiniteSetEdit b) (WholeEdit (Know a))) (WholeEdit (Know Bool))
    containsEditFunction ab = let
        getf ::
               forall m. MonadIO m
            => Know a
            -> MutableRead m (PairEditReader (FiniteSetEdit b) (WholeEdit (Know a)))
            -> m (Know Bool)
        getf Unknown _ = return Unknown
        getf (Known a) mr = do
            mu <- mr $ MkTupleEditReader SelectFirst $ KeyReadItem (ab a) ReadWhole
            return $ Known $ isJust mu
        efGet ::
               ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit b) (WholeEdit (Know a))) (WholeReader (Know Bool))
        efGet mr ReadWhole =
            lift $ do
                ka <- mr $ MkTupleEditReader SelectSecond ReadWhole
                getf ka mr
        efUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit b) (WholeEdit (Know a))
            -> MutableRead m (PairEditReader (FiniteSetEdit b) (WholeEdit (Know a)))
            -> IdentityT m [WholeEdit (Know Bool)]
        efUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit SelectFirst (KeyDeleteItem b)) mr =
            lift $ do
                ka <- mr $ MkTupleEditReader SelectSecond ReadWhole
                case ka of
                    Known a
                        | ab a == b -> return $ pure $ MkWholeEdit $ Known False
                    _ -> return []
        efUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem b)) mr =
            lift $ do
                ka <- mr $ MkTupleEditReader SelectSecond ReadWhole
                case ka of
                    Known a
                        | ab a == b -> return $ pure $ MkWholeEdit $ Known True
                    _ -> return []
        efUpdate (MkTupleEdit SelectFirst KeyClear) mr =
            lift $ do
                ka <- mr $ MkTupleEditReader SelectSecond ReadWhole
                case ka of
                    Known _ -> return $ pure $ MkWholeEdit $ Known False
                    _ -> return []
        efUpdate (MkTupleEdit SelectSecond (MkWholeEdit ka)) mr =
            lift $ do
                kk <- getf ka mr
                return $ pure $ MkWholeEdit kk
        in MkCloseUnlift identityUnlift MkAnEditFunction {..}
    in pinaforeFunctionToReference $ containsEditFunction conv . pairCombineEditFunctions setf fva

pinaforeSetSingle ::
       forall baseedit a.
       PinaforeSet baseedit '( BottomType, MeetType Entity a)
    -> PinaforeReference baseedit '( TopType, a)
pinaforeSetSingle set =
    pinaforeFunctionToReference $ funcEditFunction (fmap meet2 . maybeToKnow . getSingle) . pinaforeSetFunctionValue set

pinaforeSetFunc ::
       forall baseedit a b.
       (FiniteSet a -> b)
    -> PinaforeSet baseedit '( BottomType, a)
    -> PinaforeReference baseedit '( TopType, b)
pinaforeSetFunc f set = pinaforeFunctionToReference $ funcEditFunction (Known . f) . pinaforeSetFunctionValue set

setSumLens ::
       forall a b. (Eq a, Eq b)
    => EditLens (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (Either a b))
setSumLens = let
    efGet :: ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetReader (Either a b))
    efGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ fmap Left aa <> fmap Right bb
    efGet mr (KeyReadItem (Left a) ReadWhole) =
        lift $ do
            ma' <- mr $ MkTupleEditReader SelectFirst $ KeyReadItem a ReadWhole
            return $ fmap Left ma'
    efGet mr (KeyReadItem (Right b) ReadWhole) =
        lift $ do
            mb' <- mr $ MkTupleEditReader SelectSecond $ KeyReadItem b ReadWhole
            return $ fmap Right mb'
    efUpdate ::
           forall m. MonadIO m
        => PairEdit (FiniteSetEdit a) (FiniteSetEdit b)
        -> MutableRead m (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b))
        -> IdentityT m [FiniteSetEdit (Either a b)]
    efUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
    efUpdate (MkTupleEdit SelectFirst (KeyDeleteItem v)) _ = return $ pure $ KeyDeleteItem $ Left v
    efUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem v)) _ = return $ pure $ KeyInsertReplaceItem $ Left v
    efUpdate (MkTupleEdit SelectFirst KeyClear) mr =
        lift $ do
            vv <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            for (toList vv) $ \v -> return $ KeyDeleteItem $ Left v
    efUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
    efUpdate (MkTupleEdit SelectSecond (KeyDeleteItem v)) _ = return $ pure $ KeyDeleteItem $ Right v
    efUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem v)) _ = return $ pure $ KeyInsertReplaceItem $ Right v
    efUpdate (MkTupleEdit SelectSecond KeyClear) mr =
        lift $ do
            vv <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            for (toList vv) $ \v -> return $ KeyDeleteItem $ Right v
    elFunction :: AnEditFunction IdentityT (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (Either a b))
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit (Either a b)]
        -> MutableRead m (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b))
        -> IdentityT m (Maybe [PairEdit (FiniteSetEdit a) (FiniteSetEdit b)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \case
            KeyEditItem _ e -> never e
            KeyDeleteItem (Left v) -> return $ Just $ pure $ MkTupleEdit SelectFirst $ KeyDeleteItem v
            KeyDeleteItem (Right v) -> return $ Just $ pure $ MkTupleEdit SelectSecond $ KeyDeleteItem v
            KeyInsertReplaceItem (Left v) -> return $ Just $ pure $ MkTupleEdit SelectFirst $ KeyInsertReplaceItem v
            KeyInsertReplaceItem (Right v) -> return $ Just $ pure $ MkTupleEdit SelectSecond $ KeyInsertReplaceItem v
            KeyClear -> return $ Just $ [MkTupleEdit SelectFirst KeyClear, MkTupleEdit SelectSecond KeyClear]
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

pinaforeSetSum ::
       forall baseedit ap aq bp bq.
       PinaforeSet baseedit '( ap, aq)
    -> PinaforeSet baseedit '( bp, bq)
    -> PinaforeSet baseedit '( Either ap bp, Either aq bq)
pinaforeSetSum (MkPinaforeSet tra vala) (MkPinaforeSet trb valb) =
    MkPinaforeSet (eitherRange tra trb) $ setSumLens . pairCombineEditLenses vala valb
{-
setProductFunction :: forall a b. EditFunction (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (a,b))
setProductFunction = let
    in MkCloseUnlift identityUnlift MkAnEditFunction{..}
-}
{-
pinaforeSetProduct :: forall baseedit ap aq bp bq.
       PinaforeSet baseedit '( ap,aq) -> PinaforeSet baseedit '( bp, bq) -> PinaforeSet baseedit '(  (ap, bp), (aq, bq))
pinaforeSetProduct (MkPinaforeSet tra vala) (MkPinaforeSet trb valb) = MkPinaforeSet (pairRange tra trb) $ setProductLens . pairCombineEditLenses vala valb
-}
{- equivalent to:
data FiniteSetEdit subj where
    KeyEditItem :: subj -> ConstEdit subj -> FiniteSetEdit subj
    KeyDeleteItem :: subj -> FiniteSetEdit subj
    KeyInsertReplaceItem :: subj -> FiniteSetEdit subj
    KeyClear :: FiniteSetEdit subj
-}
