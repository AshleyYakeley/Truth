module Pinafore.Language.Value.ModelOrder where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetModel
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.WholeModel
import Shapes

newtype LangModelOrder a =
    MkLangModelOrder (ModelModelOrder a)
    deriving (Semigroup, Monoid)

instance Contravariant LangModelOrder where
    contramap ba (MkLangModelOrder o) = MkLangModelOrder $ mapModelModelOrder ba o

instance MaybeRepresentational LangModelOrder where
    maybeRepresentational = Nothing

instance HasVariance LangModelOrder where
    type VarianceOf LangModelOrder = 'Contravariance

pureLangModelOrder :: forall a. Order a -> LangModelOrder a
pureLangModelOrder cmp = MkLangModelOrder $ pureModelModelOrder cmp

langModelOrderOn :: forall a b. LangMorphism '( a, TopType) '( BottomType, b) -> LangModelOrder b -> LangModelOrder a
langModelOrderOn (MkLangMorphism m) (MkLangModelOrder o) = MkLangModelOrder $ modelModelOrderOn m o

modelOrders :: forall a. [LangModelOrder a] -> LangModelOrder a
modelOrders = mconcat

reverseLangModelOrder :: forall a. LangModelOrder a -> LangModelOrder a
reverseLangModelOrder (MkLangModelOrder o) = MkLangModelOrder $ reverseModelModelOrder o

qOrderSet :: forall a. LangModelOrder a -> PinaforeROWModel (FiniteSet a) -> PinaforeROWModel (Know [a])
qOrderSet (MkLangModelOrder ro) = modelModelOrderSet ro

langModelOrderCompare ::
       forall a.
       LangModelOrder a
    -> PinaforeImmutableWholeModel a
    -> PinaforeImmutableWholeModel a
    -> PinaforeImmutableWholeModel Ordering
langModelOrderCompare (MkLangModelOrder m) = modelModelOrderCompare m

pinaforeSetGetOrdered ::
       forall a. LangModelOrder a -> LangFiniteSetModel '( BottomType, a) -> LangWholeModel '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeROWModelToWholeModel $ qOrderSet order $ langFiniteSetModelFunctionValue set

pinaforeUpdateOrder ::
       LangModelOrder a
    -> (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r)
    -> r
pinaforeUpdateOrder (MkLangModelOrder m) = modelRefUpdateOrder m
