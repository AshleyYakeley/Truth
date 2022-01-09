module Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
    ( Mapping
    , runMapping
    , getConstructorMapping
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type Mapping :: Symbol -> Type -> Type
newtype Mapping n t =
    MkMapping (Kleisli Endo (UVarT n -> UVarT n) t)
    deriving (Semigroup, Monoid, IsoVariant, Summish, Productish)

varMapping :: forall (n :: Symbol). Mapping n (UVarT n)
varMapping = MkMapping $ Kleisli $ \ab -> Endo ab

runMapping :: Mapping n t -> (UVarT n -> UVarT n) -> t -> t
runMapping (MkMapping (Kleisli f)) ab = appEndo $ f ab

mapMapping :: ((p -> p) -> (q -> q)) -> Mapping n p -> Mapping n q
mapMapping ff (MkMapping (Kleisli f)) = MkMapping $ Kleisli $ \tt -> Endo $ ff $ appEndo $ f tt

joinMapping :: ((p -> p) -> (q -> q) -> (t -> t)) -> Mapping n p -> Mapping n q -> Mapping n t
joinMapping ff (MkMapping (Kleisli fp)) (MkMapping (Kleisli fq)) =
    MkMapping $ Kleisli $ \tt -> Endo $ ff (appEndo $ fp tt) (appEndo $ fq tt)

getArgumentMapping ::
       forall v n (t :: Type) (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       Name
    -> VarianceType v
    -> SymbolType n
    -> CCRVariation sv f
    -> NonpolarArgument PinaforeGroundType sv a
    -> NonpolarArguments PinaforeGroundType dv (f a) t
    -> PinaforeInterpreter (Mapping n t)
getArgumentMapping tname v var svm (CoNonpolarArgument t) args = do
    mapping <- getNonpolarMapping tname v var t
    return $ mapMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) mapping
getArgumentMapping tname v var svm (ContraNonpolarArgument t) args = do
    mapping <- invertVarianceType v $ \v' -> getNonpolarMapping tname v' var t
    return $ mapMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) mapping
getArgumentMapping tname v var svm (RangeNonpolarArgument tp tq) args = do
    mappingp <- invertVarianceType v $ \v' -> getNonpolarMapping tname v' var tp
    mappingq <- getNonpolarMapping tname v var tq
    return $ joinMapping (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq)) mappingp mappingq

getArgumentsMapping ::
       forall v n (t :: Type) dv gt.
       Name
    -> VarianceType v
    -> SymbolType n
    -> DolanVarianceMap dv gt
    -> NonpolarArguments PinaforeGroundType dv gt t
    -> PinaforeInterpreter (Mapping n t)
getArgumentsMapping _ _ _ NilDolanVarianceMap NilCCRArguments = return mempty
getArgumentsMapping tname v var (ConsDolanVarianceMap ccrv dvm) (ConsCCRArguments arg args) = do
    vmap1 <- getArgumentMapping tname v var ccrv arg args
    vmapr <- getArgumentsMapping tname v var dvm args
    return $ vmap1 <> vmapr

getNonpolarMapping ::
       forall v n t.
       Name
    -> VarianceType v
    -> SymbolType n
    -> PinaforeNonpolarType t
    -> PinaforeInterpreter (Mapping n t)
getNonpolarMapping _ CoVarianceType var (VarNonpolarType var')
    | Just Refl <- testEquality var var' = return varMapping
getNonpolarMapping tname ContraVarianceType var (VarNonpolarType var')
    | Just Refl <- testEquality var var' =
        throw $ InterpretTypeDeclTypeVariableWrongPolarityError tname $ symbolTypeToName var
getNonpolarMapping _ _ _ (VarNonpolarType _) = return mempty
getNonpolarMapping tname v var (GroundedNonpolarType gt args) = getArgumentsMapping tname v var (pgtVarianceMap gt) args

getConstructorMapping ::
       forall v n tl.
       Name
    -> VarianceType v
    -> SymbolType n
    -> ListType PinaforeNonpolarType tl
    -> PinaforeInterpreter (Mapping n (HList tl))
getConstructorMapping _ _ _ NilListType = return pUnit
getConstructorMapping tname v var (ConsListType t1 tr) = do
    vmap1 <- getNonpolarMapping tname v var t1
    vmapr <- getConstructorMapping tname v var tr
    return $ vmap1 <***> vmapr
