module Pinafore.Language.SetRef where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.OpenEntity
import Pinafore.Language.Reference
import Pinafore.Storage.Table (Entity)
import Shapes
import Truth.Core

data PinaforeSetRef baseedit pq where
    MkPinaforeSetRef :: Eq t => Range t pq -> PinaforeLensValue baseedit (FiniteSetEdit t) -> PinaforeSetRef baseedit pq

unPinaforeSetRef :: PinaforeSetRef baseedit '( p, p) -> PinaforeLensValue baseedit (FiniteSetEdit p)
unPinaforeSetRef (MkPinaforeSetRef tr lv) = (bijectionFiniteSetEditLens $ rangeBijection tr) . lv

instance IsoMapRange (PinaforeSetRef baseedit)

instance MapRange (PinaforeSetRef baseedit) where
    mapRange f (MkPinaforeSetRef r s) = MkPinaforeSetRef (mapRange f r) s

instance HasDolanVary '[ 'Rangevariance] (PinaforeSetRef baseedit) where
    dolanVary =
        ConsDolanVarianceMap (mkRangevary $ \mapr (MkPinaforeSetRef range lval) -> MkPinaforeSetRef (mapr range) lval) $
        NilDolanVarianceMap

pinaforeSetRefValue :: PinaforeSetRef baseedit '( q, q) -> PinaforeLensValue baseedit (FiniteSetEdit q)
pinaforeSetRefValue (MkPinaforeSetRef tr lv) = bijectionFiniteSetEditLens (rangeBijection tr) . lv

valuePinaforeSetRef :: Eq q => PinaforeLensValue baseedit (FiniteSetEdit q) -> PinaforeSetRef baseedit '( q, q)
valuePinaforeSetRef lv = MkPinaforeSetRef identityRange lv

pinaforeSetRefMeetValue ::
       PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t))
pinaforeSetRefMeetValue (MkPinaforeSetRef tr lv) = pinaforeSetRefValue $ MkPinaforeSetRef (unifyRange2 meet2 tr) lv

meetValuePinaforeSetRef ::
       PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t))
    -> PinaforeSetRef baseedit '( MeetType Entity t, t)
meetValuePinaforeSetRef lv = MkPinaforeSetRef (unUnifyRange1 meet2 identityRange) lv

pinaforeSetRefMeet ::
       forall baseedit t.
       PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( MeetType Entity t, t)
pinaforeSetRefMeet seta setb =
    meetValuePinaforeSetRef $
    readOnlyEditLens meetEditFunction .
    pairCombineEditLenses (pinaforeSetRefMeetValue seta) (pinaforeSetRefMeetValue setb)

pinaforeSetRefJoin ::
       forall baseedit t.
       PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( MeetType Entity t, t)
pinaforeSetRefJoin seta setb =
    meetValuePinaforeSetRef $
    readOnlyEditLens joinEditFunction .
    pairCombineEditLenses (pinaforeSetRefMeetValue seta) (pinaforeSetRefMeetValue setb)

pinaforeSetRefAdd :: PinaforeSetRef baseedit '( p, q) -> p -> PinaforeAction baseedit ()
pinaforeSetRefAdd (MkPinaforeSetRef tr set) p = pinaforeLensPush set [KeyInsertReplaceItem $ rangeContra tr p]

pinaforeSetRefAddNew :: PinaforeSetRef baseedit '( NewEntity, TopType) -> PinaforeAction baseedit NewEntity
pinaforeSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeSetRefAdd set e
    return e

pinaforeSetRefRemove :: PinaforeSetRef baseedit '( p, q) -> p -> PinaforeAction baseedit ()
pinaforeSetRefRemove (MkPinaforeSetRef tr set) p = pinaforeLensPush set [KeyDeleteItem $ rangeContra tr p]

pinaforeSetRefRemoveAll :: PinaforeSetRef baseedit '( BottomType, TopType) -> PinaforeAction baseedit ()
pinaforeSetRefRemoveAll (MkPinaforeSetRef _ set) = pinaforeLensPush set [KeyClear]

pinaforeSetRefFunctionValue :: PinaforeSetRef baseedit '( t, a) -> PinaforeFunctionValue baseedit (FiniteSet a)
pinaforeSetRefFunctionValue (MkPinaforeSetRef tr set) = funcEditFunction (fmap $ rangeCo tr) . lensFunctionValue set

pinaforeSetRefMembership ::
       PinaforeSetRef baseedit '( BottomType, Entity) -> PinaforeReference baseedit '( TopType, Entity -> Bool)
pinaforeSetRefMembership set =
    pinaforeFunctionToReference $ funcEditFunction (\s -> Known $ \p -> member p s) . pinaforeSetRefFunctionValue set

pinaforeSetRefContains ::
       PinaforeSetRef baseedit '( a, TopType)
    -> PinaforeReference baseedit '( BottomType, a)
    -> PinaforeReference baseedit '( TopType, Bool)
pinaforeSetRefContains (MkPinaforeSetRef (rangeContra -> conv) (editLensFunction -> setf)) (pinaforeReferenceToFunction -> fva) = let
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

pinaforeSetRefSingle ::
       forall baseedit a.
       PinaforeSetRef baseedit '( BottomType, MeetType Entity a)
    -> PinaforeReference baseedit '( TopType, a)
pinaforeSetRefSingle set =
    pinaforeFunctionToReference $
    funcEditFunction (fmap meet2 . maybeToKnow . getSingle) . pinaforeSetRefFunctionValue set

pinaforeSetRefFunc ::
       forall baseedit a b.
       (FiniteSet a -> b)
    -> PinaforeSetRef baseedit '( BottomType, a)
    -> PinaforeReference baseedit '( TopType, b)
pinaforeSetRefFunc f set = pinaforeFunctionToReference $ funcEditFunction (Known . f) . pinaforeSetRefFunctionValue set

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

pinaforeSetRefSum ::
       forall baseedit ap aq bp bq.
       PinaforeSetRef baseedit '( ap, aq)
    -> PinaforeSetRef baseedit '( bp, bq)
    -> PinaforeSetRef baseedit '( Either ap bp, Either aq bq)
pinaforeSetRefSum (MkPinaforeSetRef tra vala) (MkPinaforeSetRef trb valb) =
    MkPinaforeSetRef (eitherRange tra trb) $ setSumLens . pairCombineEditLenses vala valb
{-
setProductFunction :: forall a b. EditFunction (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (a,b))
setProductFunction = let
    in MkCloseUnlift identityUnlift MkAnEditFunction{..}
-}
{-
pinaforeSetRefProduct :: forall baseedit ap aq bp bq.
       PinaforeSetRef baseedit '( ap,aq) -> PinaforeSetRef baseedit '( bp, bq) -> PinaforeSetRef baseedit '(  (ap, bp), (aq, bq))
pinaforeSetRefProduct (MkPinaforeSetRef tra vala) (MkPinaforeSetRef trb valb) = MkPinaforeSetRef (pairRange tra trb) $ setProductLens . pairCombineEditLenses vala valb
-}
{- equivalent to:
data FiniteSetEdit subj where
    KeyEditItem :: subj -> ConstEdit subj -> FiniteSetEdit subj
    KeyDeleteItem :: subj -> FiniteSetEdit subj
    KeyInsertReplaceItem :: subj -> FiniteSetEdit subj
    KeyClear :: FiniteSetEdit subj
-}
