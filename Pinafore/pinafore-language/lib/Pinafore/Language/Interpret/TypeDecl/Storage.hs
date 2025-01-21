module Pinafore.Language.Interpret.TypeDecl.Storage
    ( CovParam (..)
    , CovParams
    , paramsToCovParams
    , nonpolarToStoreAdapter
    , nonpolarGroundedToStoreAdapter
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.TypeDecl.Parameter
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

type CovParam :: CCRArgumentKind
data CovParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    MkCovParam :: TypeVarT tv -> CovParam CoCCRVariance tv

type CovParams :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
type CovParams = CCRArguments CovParam

paramsToCovParams :: CovaryType dv -> CCRTypeParams dv gt decltype -> CovParams dv gt decltype
paramsToCovParams NilListType NilCCRArguments = NilCCRArguments
paramsToCovParams (ConsListType Refl ct) (ConsCCRArguments (CoCCRTypeParam n) args) =
    ConsCCRArguments (MkCovParam n) $ paramsToCovParams ct args

matchParamArgs ::
    forall f dv (gt1 :: CCRVariancesKind dv) (gt2 :: CCRVariancesKind dv) ta.
    CovParams dv gt1 ta ->
    Arguments f gt2 ta ->
    gt1 :~: gt2
matchParamArgs NilCCRArguments NilArguments = Refl
matchParamArgs (ConsCCRArguments _ args) (ConsArguments _ args') =
    case matchParamArgs args args' of
        Refl -> Refl

lookupVar ::
    forall f dv (gt :: CCRVariancesKind dv) tv ta.
    CovParams dv gt ta ->
    TypeVarT tv ->
    QInterpreter (Arguments f gt ta -> f tv)
lookupVar NilCCRArguments var = throw $ InterpretTypeNotStorableError $ exprShow var
lookupVar (ConsCCRArguments (MkCovParam var') params) var
    | Just Refl <- testEquality var var' =
        return $ \(ConsArguments ft args) ->
            case matchParamArgs params args of
                Refl -> ft
lookupVar (ConsCCRArguments _ params) var = do
    f <- lookupVar @f params var
    return $ \(ConsArguments _ args) ->
        case matchParamArgs params args of
            Refl -> f args

nonpolarGroundedToStoreAdapter ::
    CovParams dv gt ta ->
    QNonpolarGroundedType t ->
    QInterpreter (QExprRec (Compose ((->) (Arguments StoreAdapter gt ta)) StoreAdapter t))
nonpolarGroundedToStoreAdapter params (MkNonpolarGroundedType ground args) = do
    (cvt, MkStorableGroundType _ (MkSealedStorability _ storability)) <-
        case dolanToMonoGroundType ground of
            Nothing -> throw $ InterpretTypeNotStorableError $ showGroundType ground
            Just x -> return x
    aargsexpr <-
        getCompose
            $ ccrArgumentsToArgumentsM (\(CoNonpolarArgument arg) -> Compose $ nonpolarToStoreAdapter params arg) cvt args
    return
        $ liftA2
            (\(MkAllFor stba) aargs -> Compose $ \eargs -> stba $ mapArguments (\(Compose eaf) -> eaf eargs) aargs)
            (stbAdapterExprRec storability)
            aargsexpr

nonpolarToStoreAdapter ::
    CovParams dv gt ta ->
    QNonpolarType t ->
    QInterpreter (QExprRec (Compose ((->) (Arguments StoreAdapter gt ta)) StoreAdapter t))
nonpolarToStoreAdapter params (VarNonpolarType var) = fmap (pure . Compose) $ lookupVar params var
nonpolarToStoreAdapter params (GroundedNonpolarType t) = nonpolarGroundedToStoreAdapter params t
nonpolarToStoreAdapter _ t@(RecursiveNonpolarType{}) = throw $ InterpretTypeNotStorableError $ exprShow t
