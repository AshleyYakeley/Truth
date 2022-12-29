module Pinafore.Language.Value.ModelOrder where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Attribute
import Pinafore.Language.Value.FiniteSetModel
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

langModelOrderOn :: forall a b. LangAttribute '( a, TopType) '( BottomType, b) -> LangModelOrder b -> LangModelOrder a
langModelOrderOn (MkLangAttribute m) (MkLangModelOrder o) = MkLangModelOrder $ modelModelOrderOn m o

modelOrders :: forall a. [LangModelOrder a] -> LangModelOrder a
modelOrders = mconcat

reverseLangModelOrder :: forall a. LangModelOrder a -> LangModelOrder a
reverseLangModelOrder (MkLangModelOrder o) = MkLangModelOrder $ reverseModelModelOrder o

qOrderSet :: forall a. LangModelOrder a -> WROWModel (FiniteSet a) -> WROWModel (Know [a])
qOrderSet (MkLangModelOrder ro) = modelModelOrderSet ro

langModelOrderCompare ::
       forall a. LangModelOrder a -> ImmutableWholeModel a -> ImmutableWholeModel a -> ImmutableWholeModel Ordering
langModelOrderCompare (MkLangModelOrder m) = modelModelOrderCompare m

finiteSetGetOrdered ::
       forall a. LangModelOrder a -> LangFiniteSetModel '( BottomType, a) -> LangWholeModel '( TopType, [a])
finiteSetGetOrdered order set = wROWModelToWholeModel $ qOrderSet order $ langFiniteSetModelFunctionValue set

modelOrderUpdateOrder ::
       LangModelOrder a
    -> (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r)
    -> r
modelOrderUpdateOrder (MkLangModelOrder m) = modelRefUpdateOrder m
