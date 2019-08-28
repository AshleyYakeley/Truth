module Pinafore.Language.Value.SetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Pinafore.Storage (Entity)
import Shapes
import Truth.Core

data PinaforeSetRef baseedit pq where
    MkPinaforeSetRef
        :: Eq t => Range JMShim t pq -> PinaforeLensValue baseedit (FiniteSetEdit t) -> PinaforeSetRef baseedit pq

unPinaforeSetRef :: PinaforeSetRef baseedit '( p, p) -> PinaforeLensValue baseedit (FiniteSetEdit p)
unPinaforeSetRef (MkPinaforeSetRef tr lv) =
    (bijectionFiniteSetEditLens $ isoMapCat fromEnhanced $ rangeBijection tr) . lv

instance CatFunctor (CatRange (->)) (->) (PinaforeSetRef baseedit) where
    cfmap f (MkPinaforeSetRef r v) = MkPinaforeSetRef (cfmap f r) v

instance HasVariance 'Rangevariance (PinaforeSetRef baseedit) where
    varianceRepresentational = Nothing

pinaforeSetRefValue :: PinaforeSetRef baseedit '( q, q) -> PinaforeLensValue baseedit (FiniteSetEdit q)
pinaforeSetRefValue (MkPinaforeSetRef tr lv) =
    bijectionFiniteSetEditLens (isoMapCat fromEnhanced $ rangeBijection tr) . lv

valuePinaforeSetRef :: Eq q => PinaforeLensValue baseedit (FiniteSetEdit q) -> PinaforeSetRef baseedit '( q, q)
valuePinaforeSetRef lv = MkPinaforeSetRef identityRange lv

pinaforeSetRefMeetValue ::
       PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t))
pinaforeSetRefMeetValue (MkPinaforeSetRef tr lv) = pinaforeSetRefValue $ MkPinaforeSetRef (contraMapRange meet2 tr) lv

meetValuePinaforeSetRef ::
       PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t))
    -> PinaforeSetRef baseedit '( MeetType Entity t, t)
meetValuePinaforeSetRef lv = MkPinaforeSetRef (coMapRange meet2 identityRange) lv

pinaforeSetRefMeet ::
       forall baseedit t.
       PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( MeetType Entity t, t)
pinaforeSetRefMeet seta setb =
    meetValuePinaforeSetRef $
    readOnlyEditLens meetUpdateFunction .
    pairCombineEditLenses (pinaforeSetRefMeetValue seta) (pinaforeSetRefMeetValue setb)

pinaforeSetRefJoin ::
       forall baseedit t.
       PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( t, MeetType Entity t)
    -> PinaforeSetRef baseedit '( MeetType Entity t, t)
pinaforeSetRefJoin seta setb =
    meetValuePinaforeSetRef $
    readOnlyEditLens joinUpdateFunction .
    pairCombineEditLenses (pinaforeSetRefMeetValue seta) (pinaforeSetRefMeetValue setb)

pinaforeSetRefAdd :: PinaforeSetRef baseedit '( p, q) -> p -> PinaforeAction baseedit ()
pinaforeSetRefAdd (MkPinaforeSetRef tr set) p =
    pinaforeLensPush set [KeyInsertReplaceItem $ fromEnhanced (rangeContra tr) p]

pinaforeSetRefAddNew :: PinaforeSetRef baseedit '( NewEntity, TopType) -> PinaforeAction baseedit NewEntity
pinaforeSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeSetRefAdd set e
    return e

pinaforeSetRefRemove :: PinaforeSetRef baseedit '( p, q) -> p -> PinaforeAction baseedit ()
pinaforeSetRefRemove (MkPinaforeSetRef tr set) p =
    pinaforeLensPush set [KeyDeleteItem $ fromEnhanced (rangeContra tr) p]

pinaforeSetRefRemoveAll :: PinaforeSetRef baseedit '( BottomType, TopType) -> PinaforeAction baseedit ()
pinaforeSetRefRemoveAll (MkPinaforeSetRef _ set) = pinaforeLensPush set [KeyClear]

pinaforeSetRefFunctionValue :: PinaforeSetRef baseedit '( t, a) -> PinaforeFunctionValue baseedit (FiniteSet a)
pinaforeSetRefFunctionValue (MkPinaforeSetRef tr set) =
    funcUpdateFunction (fmap $ fromEnhanced $ rangeCo tr) . lensFunctionValue set

pinaforeSetRefMember ::
       forall baseedit a. PinaforeSetRef baseedit '( a, TopType) -> a -> PinaforeRef baseedit '( Bool, Bool)
pinaforeSetRefMember (MkPinaforeSetRef tr set) val = let
    tval = fromEnhanced (rangeContra tr) val
    in LensPinaforeRef identityRange $ wholeEditLens knowMaybeLens . finiteSetEditLens tval . set

pinaforeSetRefSingle ::
       forall baseedit a.
       PinaforeSetRef baseedit '( BottomType, MeetType Entity a)
    -> PinaforeRef baseedit '( TopType, a)
pinaforeSetRefSingle set =
    pinaforeFunctionToRef $ funcUpdateFunction (fmap meet2 . maybeToKnow . getSingle) . pinaforeSetRefFunctionValue set

pinaforeSetRefFunc ::
       forall baseedit a b.
       (FiniteSet a -> b)
    -> PinaforeSetRef baseedit '( BottomType, a)
    -> PinaforeRef baseedit '( TopType, b)
pinaforeSetRefFunc f set = pinaforeFunctionToRef $ funcUpdateFunction (Known . f) . pinaforeSetRefFunctionValue set

setSumLens ::
       forall a b. (Eq a, Eq b)
    => EditLens (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (Either a b))
setSumLens = let
    ufGet :: ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetReader (Either a b))
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ fmap Left aa <> fmap Right bb
    ufGet mr (KeyReadItem (Left a) ReadWhole) =
        lift $ do
            ma' <- mr $ MkTupleEditReader SelectFirst $ KeyReadItem a ReadWhole
            return $ fmap Left ma'
    ufGet mr (KeyReadItem (Right b) ReadWhole) =
        lift $ do
            mb' <- mr $ MkTupleEditReader SelectSecond $ KeyReadItem b ReadWhole
            return $ fmap Right mb'
    ufUpdate ::
           forall m. MonadIO m
        => PairEdit (FiniteSetEdit a) (FiniteSetEdit b)
        -> MutableRead m (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b))
        -> IdentityT m [FiniteSetEdit (Either a b)]
    ufUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
    ufUpdate (MkTupleEdit SelectFirst (KeyDeleteItem v)) _ = return $ pure $ KeyDeleteItem $ Left v
    ufUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem v)) _ = return $ pure $ KeyInsertReplaceItem $ Left v
    ufUpdate (MkTupleEdit SelectFirst KeyClear) mr =
        lift $ do
            vv <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            for (toList vv) $ \v -> return $ KeyDeleteItem $ Left v
    ufUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
    ufUpdate (MkTupleEdit SelectSecond (KeyDeleteItem v)) _ = return $ pure $ KeyDeleteItem $ Right v
    ufUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem v)) _ = return $ pure $ KeyInsertReplaceItem $ Right v
    ufUpdate (MkTupleEdit SelectSecond KeyClear) mr =
        lift $ do
            vv <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            for (toList vv) $ \v -> return $ KeyDeleteItem $ Right v
    elFunction :: AnUpdateFunction IdentityT (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (Either a b))
    elFunction = MkAnUpdateFunction {..}
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

setProductFunction :: forall a b. UpdateFunction (PairEdit (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetEdit (a, b))
setProductFunction = let
    ufGet :: ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b)) (FiniteSetReader (a, b))
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ liftA2 (,) aa bb
    ufGet mr (KeyReadItem (a, b) ReadWhole) =
        lift $
        getComposeM $ do
            a' <- MkComposeM $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem a ReadWhole
            b' <- MkComposeM $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem b ReadWhole
            return (a', b')
    ufUpdate ::
           forall m. MonadIO m
        => PairEdit (FiniteSetEdit a) (FiniteSetEdit b)
        -> MutableRead m (PairEditReader (FiniteSetEdit a) (FiniteSetEdit b))
        -> IdentityT m [FiniteSetEdit (a, b)]
    ufUpdate (MkTupleEdit SelectFirst KeyClear) _ = return [KeyClear]
    ufUpdate (MkTupleEdit SelectSecond KeyClear) _ = return [KeyClear]
    ufUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
    ufUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
    ufUpdate (MkTupleEdit SelectFirst (KeyDeleteItem a)) mr =
        lift $ do
            bb <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ fmap (\b -> KeyDeleteItem (a, b)) $ toList bb
    ufUpdate (MkTupleEdit SelectSecond (KeyDeleteItem b)) mr =
        lift $ do
            aa <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            return $ fmap (\a -> KeyDeleteItem (a, b)) $ toList aa
    ufUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem a)) mr =
        lift $ do
            bb <- mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ fmap (\b -> KeyInsertReplaceItem (a, b)) $ toList bb
    ufUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem b)) mr =
        lift $ do
            aa <- mr $ MkTupleEditReader SelectFirst KeyReadKeys
            return $ fmap (\a -> KeyInsertReplaceItem (a, b)) $ toList aa
    in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

pinaforeSetRefProduct ::
       forall baseedit ap aq bp bq.
       PinaforeSetRef baseedit '( ap, aq)
    -> PinaforeSetRef baseedit '( bp, bq)
    -> PinaforeSetRef baseedit '( (ap, bp), (aq, bq))
pinaforeSetRefProduct (MkPinaforeSetRef tra vala) (MkPinaforeSetRef trb valb) =
    MkPinaforeSetRef (pairRange tra trb) $ readOnlyEditLens setProductFunction . pairCombineEditLenses vala valb