module Pinafore.Language.Value.ListRef where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.WholeRef
import Shapes

type LangListRef :: (Type, Type) -> Type
data LangListRef pq where
    OrderedLangListRef :: WModel (OrderedListUpdate [q] (ROWUpdate q)) -> LangListRef '( p, q)
    FullLangListRef
        :: WModel (BiUpdate (ListUpdate [p] (WholeUpdate p)) (ListUpdate [q] (WholeUpdate q))) -> LangListRef '( p, q)

instance CatFunctor (CatRange (->)) (->) LangListRef where
    cfmap (MkCatRange _ qq) (OrderedLangListRef model) =
        OrderedLangListRef $ eaMap (liftOrderedListChangeLens $ funcChangeLens qq) model
    cfmap (MkCatRange pp qq) (FullLangListRef model) =
        FullLangListRef $ eaMap (mappingBiChangeLens (listWholeChangeMap pp) (listWholeChangeMap qq)) model

instance HasVariance 'Rangevariance LangListRef where
    varianceRepresentational = Nothing

langListRefToOrdered :: LangListRef '( p, q) -> WModel (OrderedListUpdate [q] (ROWUpdate q))
langListRefToOrdered (OrderedLangListRef model) = model
langListRefToOrdered (FullLangListRef model) =
    eaMap
        (liftOrderedListChangeLens toReadOnlyChangeLens .
         listOrderedListChangeLens . fromReadOnlyRejectingChangeLens . biReadOnlyChangeLens)
        model

langListRefCountRef :: LangListRef '( BottomType, TopType) -> PinaforeImmutableWholeRef Int
langListRefCountRef (OrderedLangListRef model) =
    functionImmutableRef $ eaMap (funcChangeLens coerce . orderedListLengthLens) model
langListRefCountRef (FullLangListRef model) =
    functionImmutableRef $
    eaMap (funcChangeLens coerce . liftReadOnlyChangeLens listLengthLens . biReadOnlyChangeLens) model

langListRefRead :: LangListRef '( p, q) -> ReadM (ListReader [q] (WholeReader q)) t -> PinaforeAction t
langListRefRead (OrderedLangListRef model) rm = pinaforeRefGet model rm
langListRefRead (FullLangListRef model) rm = pinaforeRefGet model rm

langListRefGetCount :: LangListRef '( BottomType, TopType) -> PinaforeAction Int
langListRefGetCount ref = fmap unSequencePoint $ langListRefRead ref $ readM ListReadLength

langListRefGetItem :: forall q. Int -> LangListRef '( BottomType, q) -> PinaforeAction q
langListRefGetItem i ref = do
    mq <- langListRefRead ref $ readM $ ListReadItem (MkSequencePoint i) ReadWhole
    pinaforeActionKnow $ maybeToKnow mq

langListRefInsert :: forall p. Int -> p -> LangListRef '( p, TopType) -> PinaforeAction ()
langListRefInsert _ _ (OrderedLangListRef _) = empty
langListRefInsert i val (FullLangListRef model) =
    pinaforeRefPush model $ pure $ MkBiEdit $ ListEditInsert (MkSequencePoint i) val

langListRefSet :: forall p. Int -> p -> LangListRef '( p, TopType) -> PinaforeAction ()
langListRefSet _ _ (OrderedLangListRef _) = empty
langListRefSet i val (FullLangListRef model) =
    pinaforeRefPush model $ pure $ MkBiEdit $ ListEditItem (MkSequencePoint i) $ MkWholeReaderEdit val

langListRefDelete :: Int -> LangListRef '( BottomType, TopType) -> PinaforeAction ()
langListRefDelete i (OrderedLangListRef model) =
    pinaforeRefPush model $ pure $ OrderedListEditDelete (MkSequencePoint i)
langListRefDelete i (FullLangListRef model) =
    pinaforeRefPush model $ pure $ MkBiEdit $ ListEditDelete (MkSequencePoint i)

langListRefClear :: LangListRef '( BottomType, TopType) -> PinaforeAction ()
langListRefClear (OrderedLangListRef model) = pinaforeRefPush model $ pure OrderedListEditClear
langListRefClear (FullLangListRef model) = pinaforeRefPush model $ pure $ MkBiEdit ListEditClear

biKnowWhole :: forall p q. ChangeLens (BiWholeUpdate (Know [p]) (Know [q])) (BiWholeUpdate [p] [q])
biKnowWhole = mapBiWholeChangeLens Known $ fromKnow []

langWholeRefToListRef :: LangWholeRef '( [a], [a]) -> LangListRef '( a, a)
langWholeRefToListRef (MutableLangWholeRef model) = FullLangListRef $ eaMap (convertBiChangeLens id . biKnowWhole) model
langWholeRefToListRef (ImmutableLangWholeRef ref) =
    OrderedLangListRef $
    eaMap (fromReadOnlyRejectingChangeLens . convertReadOnlyChangeLens) $ pinaforeImmutableRefValue [] ref

langListRefItem :: forall p q. Bool -> Int -> LangListRef '( p, q) -> PinaforeAction (LangWholeRef '( p, q))
langListRefItem present i (FullLangListRef lmodel) = do
    let
        linearListItemCL :: forall t. LinearFloatingChangeLens _ (ListUpdate [t] (WholeUpdate t)) (WholeUpdate (Know t))
        linearListItemCL =
            composeExpFloatingChangeLens (changeLensToExpFloating $ bijectionWholeChangeLens $ invert knowMaybe) $
            listItemLinearLens present $ MkSequencePoint i
    wmodel <-
        pinaforeFloatMap
            (expToFloatingChangeLens $ biLinearFloatingChangeLens (linearListItemCL @p) (linearListItemCL @q))
            lmodel
    return $ MutableLangWholeRef wmodel
langListRefItem _present i (OrderedLangListRef lmodel) = do
    let ip = MkSequencePoint i
    wmodel <- pinaforeFloatMap (changeLensToFloating (funcChangeLens maybeToKnow) . orderedListItemLens ip) lmodel
    return $ ImmutableLangWholeRef $ MkPinaforeImmutableWholeRef wmodel
