module Pinafore.Language.Value.SetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Pinafore.Storage (Entity)
import Shapes
import Truth.Core

data PinaforeSetRef baseupdate pq where
    MkPinaforeSetRef
        :: Eq t => Range JMShim t pq -> PinaforeLensValue baseupdate (FiniteSetUpdate t) -> PinaforeSetRef baseupdate pq

unPinaforeSetRef :: PinaforeSetRef baseupdate '( p, p) -> PinaforeLensValue baseupdate (FiniteSetUpdate p)
unPinaforeSetRef (MkPinaforeSetRef tr lv) =
    (bijectionFiniteSetEditLens $ isoMapCat fromEnhanced $ rangeBijection tr) . lv

instance CatFunctor (CatRange (->)) (->) (PinaforeSetRef baseupdate) where
    cfmap f (MkPinaforeSetRef r v) = MkPinaforeSetRef (cfmap f r) v

instance HasVariance 'Rangevariance (PinaforeSetRef baseupdate) where
    varianceRepresentational = Nothing

pinaforeSetRefValue :: PinaforeSetRef baseupdate '( q, q) -> PinaforeLensValue baseupdate (FiniteSetUpdate q)
pinaforeSetRefValue (MkPinaforeSetRef tr lv) =
    bijectionFiniteSetEditLens (isoMapCat fromEnhanced $ rangeBijection tr) . lv

valuePinaforeSetRef :: Eq q => PinaforeLensValue baseupdate (FiniteSetUpdate q) -> PinaforeSetRef baseupdate '( q, q)
valuePinaforeSetRef lv = MkPinaforeSetRef identityRange lv

pinaforeSetRefMeetValue ::
       PinaforeSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeLensValue baseupdate (FiniteSetUpdate (MeetType Entity t))
pinaforeSetRefMeetValue (MkPinaforeSetRef tr lv) = pinaforeSetRefValue $ MkPinaforeSetRef (contraMapRange meet2 tr) lv

meetValuePinaforeSetRef ::
       PinaforeLensValue baseupdate (FiniteSetUpdate (MeetType Entity t))
    -> PinaforeSetRef baseupdate '( MeetType Entity t, t)
meetValuePinaforeSetRef lv = MkPinaforeSetRef (coMapRange meet2 identityRange) lv

pinaforeSetRefMeet ::
       forall baseupdate t.
       PinaforeSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeSetRef baseupdate '( MeetType Entity t, t)
pinaforeSetRefMeet seta setb =
    meetValuePinaforeSetRef $
    readOnlyEditLens meetUpdateFunction .
    pairCombineEditLenses (pinaforeSetRefMeetValue seta) (pinaforeSetRefMeetValue setb)

pinaforeSetRefJoin ::
       forall baseupdate t.
       PinaforeSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeSetRef baseupdate '( MeetType Entity t, t)
pinaforeSetRefJoin seta setb =
    meetValuePinaforeSetRef $
    readOnlyEditLens joinUpdateFunction .
    pairCombineEditLenses (pinaforeSetRefMeetValue seta) (pinaforeSetRefMeetValue setb)

pinaforeSetRefAdd :: PinaforeSetRef baseupdate '( p, q) -> p -> PinaforeAction baseupdate ()
pinaforeSetRefAdd (MkPinaforeSetRef tr set) p =
    pinaforeLensPush set [KeyEditInsertReplace $ fromEnhanced (rangeContra tr) p]

pinaforeSetRefAddNew :: PinaforeSetRef baseupdate '( NewEntity, TopType) -> PinaforeAction baseupdate NewEntity
pinaforeSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeSetRefAdd set e
    return e

pinaforeSetRefRemove :: PinaforeSetRef baseupdate '( p, q) -> p -> PinaforeAction baseupdate ()
pinaforeSetRefRemove (MkPinaforeSetRef tr set) p =
    pinaforeLensPush set [KeyEditDelete $ fromEnhanced (rangeContra tr) p]

pinaforeSetRefRemoveAll :: PinaforeSetRef baseupdate '( BottomType, TopType) -> PinaforeAction baseupdate ()
pinaforeSetRefRemoveAll (MkPinaforeSetRef _ set) = pinaforeLensPush set [KeyEditClear]

pinaforeSetRefFunctionValue :: PinaforeSetRef baseupdate '( t, a) -> PinaforeFunctionValue baseupdate (FiniteSet a)
pinaforeSetRefFunctionValue (MkPinaforeSetRef tr set) =
    funcUpdateFunction (fmap $ fromEnhanced $ rangeCo tr) . lensFunctionValue set

pinaforeSetRefMember ::
       forall baseupdate a. PinaforeSetRef baseupdate '( a, TopType) -> a -> PinaforeRef baseupdate '( Bool, Bool)
pinaforeSetRefMember (MkPinaforeSetRef tr set) val = let
    tval = fromEnhanced (rangeContra tr) val
    in LensPinaforeRef identityRange $ wholeEditLens knowMaybeLens . finiteSetEditLens tval . set

pinaforeSetRefSingle ::
       forall baseupdate a.
       PinaforeSetRef baseupdate '( BottomType, MeetType Entity a)
    -> PinaforeRef baseupdate '( TopType, a)
pinaforeSetRefSingle set =
    pinaforeFunctionToRef $ funcUpdateFunction (fmap meet2 . maybeToKnow . getSingle) . pinaforeSetRefFunctionValue set

pinaforeSetRefFunc ::
       forall baseupdate a b.
       (FiniteSet a -> b)
    -> PinaforeSetRef baseupdate '( BottomType, a)
    -> PinaforeRef baseupdate '( TopType, b)
pinaforeSetRefFunc f set = pinaforeFunctionToRef $ funcUpdateFunction (Known . f) . pinaforeSetRefFunctionValue set

setSumLens ::
       forall a b. (Eq a, Eq b)
    => EditLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
setSumLens = let
    ufGet ::
           ReadFunctionT IdentityT (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (Either a b))
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap Left aa <> fmap Right bb
    ufGet mr (KeyReadItem (Left a) ReadWhole) =
        lift $ do
            ma' <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            return $ fmap Left ma'
    ufGet mr (KeyReadItem (Right b) ReadWhole) =
        lift $ do
            mb' <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return $ fmap Right mb'
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> IdentityT m [FiniteSetUpdate (Either a b)]
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Left v
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace v)) _ = return $ pure $ KeyUpdateInsertReplace $ Left v
    ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr =
        lift $ do
            vv <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            for (toList vv) $ \v -> return $ KeyUpdateDelete $ Left v
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Right v
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace v)) _ =
        return $ pure $ KeyUpdateInsertReplace $ Right v
    ufUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr =
        lift $ do
            vv <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            for (toList vv) $ \v -> return $ KeyUpdateDelete $ Right v
    elFunction ::
           AnUpdateFunction IdentityT (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit (Either a b)]
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> IdentityT m (Maybe [PairUpdateEdit (FiniteSetUpdate a) (FiniteSetUpdate b)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \case
            KeyEditItem _ e -> never e
            KeyEditDelete (Left v) -> return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ KeyEditDelete v
            KeyEditDelete (Right v) -> return $ Just $ pure $ MkTupleUpdateEdit SelectSecond $ KeyEditDelete v
            KeyEditInsertReplace (Left v) ->
                return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ KeyEditInsertReplace v
            KeyEditInsertReplace (Right v) ->
                return $ Just $ pure $ MkTupleUpdateEdit SelectSecond $ KeyEditInsertReplace v
            KeyEditClear ->
                return $
                Just $ [MkTupleUpdateEdit SelectFirst KeyEditClear, MkTupleUpdateEdit SelectSecond KeyEditClear]
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

pinaforeSetRefSum ::
       forall baseupdate ap aq bp bq.
       PinaforeSetRef baseupdate '( ap, aq)
    -> PinaforeSetRef baseupdate '( bp, bq)
    -> PinaforeSetRef baseupdate '( Either ap bp, Either aq bq)
pinaforeSetRefSum (MkPinaforeSetRef tra vala) (MkPinaforeSetRef trb valb) =
    MkPinaforeSetRef (eitherRange tra trb) $ setSumLens . pairCombineEditLenses vala valb

setProductFunction ::
       forall a b. UpdateFunction (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (a, b))
setProductFunction = let
    ufGet :: ReadFunctionT IdentityT (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (a, b))
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ liftA2 (,) aa bb
    ufGet mr (KeyReadItem (a, b) ReadWhole) =
        lift $
        getComposeM $ do
            a' <- MkComposeM $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            b' <- MkComposeM $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return (a', b')
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> IdentityT m [FiniteSetUpdate (a, b)]
    ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _ = return [KeyUpdateClear]
    ufUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) _ = return [KeyUpdateClear]
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _ = never update
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ update)) _ = never update
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr =
        lift $ do
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap (\b -> KeyUpdateDelete (a, b)) $ toList bb
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete b)) mr =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $ fmap (\a -> KeyUpdateDelete (a, b)) $ toList aa
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr =
        lift $ do
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap (\b -> KeyUpdateInsertReplace (a, b)) $ toList bb
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace b)) mr =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $ fmap (\a -> KeyUpdateInsertReplace (a, b)) $ toList aa
    in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

pinaforeSetRefProduct ::
       forall baseupdate ap aq bp bq.
       PinaforeSetRef baseupdate '( ap, aq)
    -> PinaforeSetRef baseupdate '( bp, bq)
    -> PinaforeSetRef baseupdate '( (ap, bp), (aq, bq))
pinaforeSetRefProduct (MkPinaforeSetRef tra vala) (MkPinaforeSetRef trb valb) =
    MkPinaforeSetRef (pairRange tra trb) $ readOnlyEditLens setProductFunction . pairCombineEditLenses vala valb
