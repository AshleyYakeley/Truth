module Pinafore.Language.Value.FiniteSetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Pinafore.Storage (Entity)
import Shapes
import Truth.Core

data PinaforeFiniteSetRef baseupdate pq where
    MkPinaforeFiniteSetRef
        :: Eq t
        => Range JMShim t pq
        -> PinaforeLensValue baseupdate (FiniteSetUpdate t)
        -> PinaforeFiniteSetRef baseupdate pq

unPinaforeFiniteSetRef :: PinaforeFiniteSetRef baseupdate '( p, p) -> PinaforeLensValue baseupdate (FiniteSetUpdate p)
unPinaforeFiniteSetRef (MkPinaforeFiniteSetRef tr lv) =
    (bijectionFiniteSetEditLens $ isoMapCat fromEnhanced $ rangeBijection tr) . lv

instance CatFunctor (CatRange (->)) (->) (PinaforeFiniteSetRef baseupdate) where
    cfmap f (MkPinaforeFiniteSetRef r v) = MkPinaforeFiniteSetRef (cfmap f r) v

instance HasVariance 'Rangevariance (PinaforeFiniteSetRef baseupdate) where
    varianceRepresentational = Nothing

pinaforeFiniteSetRefValue ::
       PinaforeFiniteSetRef baseupdate '( q, q) -> PinaforeLensValue baseupdate (FiniteSetUpdate q)
pinaforeFiniteSetRefValue (MkPinaforeFiniteSetRef tr lv) =
    bijectionFiniteSetEditLens (isoMapCat fromEnhanced $ rangeBijection tr) . lv

valuePinaforeFiniteSetRef ::
       Eq q => PinaforeLensValue baseupdate (FiniteSetUpdate q) -> PinaforeFiniteSetRef baseupdate '( q, q)
valuePinaforeFiniteSetRef lv = MkPinaforeFiniteSetRef identityRange lv

pinaforeFiniteSetRefMeetValue ::
       PinaforeFiniteSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeLensValue baseupdate (FiniteSetUpdate (MeetType Entity t))
pinaforeFiniteSetRefMeetValue (MkPinaforeFiniteSetRef tr lv) =
    pinaforeFiniteSetRefValue $ MkPinaforeFiniteSetRef (contraMapRange meet2 tr) lv

meetValuePinaforeFiniteSetRef ::
       PinaforeLensValue baseupdate (FiniteSetUpdate (MeetType Entity t))
    -> PinaforeFiniteSetRef baseupdate '( MeetType Entity t, t)
meetValuePinaforeFiniteSetRef lv = MkPinaforeFiniteSetRef (coMapRange meet2 identityRange) lv

pinaforeFiniteSetRefMeet ::
       forall baseupdate t.
       PinaforeFiniteSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef baseupdate '( MeetType Entity t, t)
pinaforeFiniteSetRefMeet seta setb =
    meetValuePinaforeFiniteSetRef $
    readOnlyEditLens meetUpdateFunction .
    pairCombineEditLenses (pinaforeFiniteSetRefMeetValue seta) (pinaforeFiniteSetRefMeetValue setb)

pinaforeFiniteSetRefJoin ::
       forall baseupdate t.
       PinaforeFiniteSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef baseupdate '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef baseupdate '( MeetType Entity t, t)
pinaforeFiniteSetRefJoin seta setb =
    meetValuePinaforeFiniteSetRef $
    readOnlyEditLens joinUpdateFunction .
    pairCombineEditLenses (pinaforeFiniteSetRefMeetValue seta) (pinaforeFiniteSetRefMeetValue setb)

pinaforeFiniteSetRefAdd :: PinaforeFiniteSetRef baseupdate '( p, q) -> p -> PinaforeAction baseupdate ()
pinaforeFiniteSetRefAdd (MkPinaforeFiniteSetRef tr set) p =
    pinaforeLensPush set [KeyEditInsertReplace $ fromEnhanced (rangeContra tr) p]

pinaforeFiniteSetRefAddNew ::
       PinaforeFiniteSetRef baseupdate '( NewEntity, TopType) -> PinaforeAction baseupdate NewEntity
pinaforeFiniteSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeFiniteSetRefAdd set e
    return e

pinaforeFiniteSetRefRemove :: PinaforeFiniteSetRef baseupdate '( p, q) -> p -> PinaforeAction baseupdate ()
pinaforeFiniteSetRefRemove (MkPinaforeFiniteSetRef tr set) p =
    pinaforeLensPush set [KeyEditDelete $ fromEnhanced (rangeContra tr) p]

pinaforeFiniteSetRefRemoveAll :: PinaforeFiniteSetRef baseupdate '( BottomType, TopType) -> PinaforeAction baseupdate ()
pinaforeFiniteSetRefRemoveAll (MkPinaforeFiniteSetRef _ set) = pinaforeLensPush set [KeyEditClear]

pinaforeFiniteSetRefFunctionValue ::
       PinaforeFiniteSetRef baseupdate '( t, a) -> PinaforeFunctionValue baseupdate (FiniteSet a)
pinaforeFiniteSetRefFunctionValue (MkPinaforeFiniteSetRef tr set) =
    funcUpdateFunction (fmap $ fromEnhanced $ rangeCo tr) . lensFunctionValue set

pinaforeFiniteSetRefMember ::
       forall baseupdate a. PinaforeFiniteSetRef baseupdate '( a, TopType) -> a -> PinaforeRef baseupdate '( Bool, Bool)
pinaforeFiniteSetRefMember (MkPinaforeFiniteSetRef tr set) val = let
    tval = fromEnhanced (rangeContra tr) val
    in LensPinaforeRef identityRange $ wholeEditLens knowMaybeLens . finiteSetEditLens tval . set

pinaforeFiniteSetRefSingle ::
       forall baseupdate a.
       PinaforeFiniteSetRef baseupdate '( BottomType, MeetType Entity a)
    -> PinaforeRef baseupdate '( TopType, a)
pinaforeFiniteSetRefSingle set =
    pinaforeFunctionToRef $
    funcUpdateFunction (fmap meet2 . maybeToKnow . getSingle) . pinaforeFiniteSetRefFunctionValue set

pinaforeFiniteSetRefFunc ::
       forall baseupdate a b.
       (FiniteSet a -> b)
    -> PinaforeFiniteSetRef baseupdate '( BottomType, a)
    -> PinaforeRef baseupdate '( TopType, b)
pinaforeFiniteSetRefFunc f set =
    pinaforeFunctionToRef $ funcUpdateFunction (Known . f) . pinaforeFiniteSetRefFunctionValue set

pinaforeFiniteSetRefCartesianSum ::
       forall baseupdate ap aq bp bq.
       PinaforeFiniteSetRef baseupdate '( ap, aq)
    -> PinaforeFiniteSetRef baseupdate '( bp, bq)
    -> PinaforeFiniteSetRef baseupdate '( Either ap bp, Either aq bq)
pinaforeFiniteSetRefCartesianSum (MkPinaforeFiniteSetRef tra vala) (MkPinaforeFiniteSetRef trb valb) =
    MkPinaforeFiniteSetRef (eitherRange tra trb) $ finiteSetCartesianSumEditLens . pairCombineEditLenses vala valb

pinaforeFiniteSetRefCartesianProduct ::
       forall baseupdate ap aq bp bq.
       PinaforeFiniteSetRef baseupdate '( ap, aq)
    -> PinaforeFiniteSetRef baseupdate '( bp, bq)
    -> PinaforeFiniteSetRef baseupdate '( (ap, bp), (aq, bq))
pinaforeFiniteSetRefCartesianProduct (MkPinaforeFiniteSetRef tra vala) (MkPinaforeFiniteSetRef trb valb) =
    MkPinaforeFiniteSetRef (pairRange tra trb) $
    readOnlyEditLens finiteSetCartesianProductUpdateFunction . pairCombineEditLenses vala valb
