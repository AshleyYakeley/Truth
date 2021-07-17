module Pinafore.Language.Value.RefOrder where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.WholeRef
import Shapes

newtype LangRefOrder a =
    MkLangRefOrder (ModelRefOrder a)
    deriving (Semigroup, Monoid)

instance Contravariant LangRefOrder where
    contramap ba (MkLangRefOrder o) = MkLangRefOrder $ mapModelRefOrder ba o

instance HasVariance 'Contravariance LangRefOrder where
    varianceRepresentational = Nothing

pureLangRefOrder :: forall a. Order a -> LangRefOrder a
pureLangRefOrder cmp = MkLangRefOrder $ pureModelRefOrder cmp

langRefOrderOn :: forall a b. LangMorphism '( a, TopType) '( BottomType, b) -> LangRefOrder b -> LangRefOrder a
langRefOrderOn (MkLangMorphism m) (MkLangRefOrder o) = MkLangRefOrder $ modelRefOrderOn m o

refOrders :: forall a. [LangRefOrder a] -> LangRefOrder a
refOrders = mconcat

reverseLangRefOrder :: forall a. LangRefOrder a -> LangRefOrder a
reverseLangRefOrder (MkLangRefOrder o) = MkLangRefOrder $ reverseModelRefOrder o

qOrderSet :: forall a. LangRefOrder a -> PinaforeROWRef (FiniteSet a) -> PinaforeROWRef (Know [a])
qOrderSet (MkLangRefOrder ro) = modelRefOrderSet ro

langRefOrderCompare ::
       forall a.
       LangRefOrder a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef Ordering
langRefOrderCompare (MkLangRefOrder m) = modelRefOrderCompare m

pinaforeSetGetOrdered :: forall a. LangRefOrder a -> LangFiniteSetRef '( BottomType, a) -> LangWholeRef '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeROWRefToWholeRef $ qOrderSet order $ langFiniteSetRefFunctionValue set

pinaforeUpdateOrder ::
       LangRefOrder a
    -> (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r)
    -> r
pinaforeUpdateOrder (MkLangRefOrder m) = modelRefUpdateOrder m
