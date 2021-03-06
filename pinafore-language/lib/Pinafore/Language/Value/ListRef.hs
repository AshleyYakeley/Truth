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

langListRefCount :: LangListRef '( BottomType, TopType) -> PinaforeImmutableWholeRef Int
langListRefCount (OrderedLangListRef model) =
    functionImmutableRef $ eaMap (funcChangeLens coerce . orderedListLengthLens) model
langListRefCount (FullLangListRef model) =
    functionImmutableRef $
    eaMap (funcChangeLens coerce . liftReadOnlyChangeLens listLengthLens . biReadOnlyChangeLens) model

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
