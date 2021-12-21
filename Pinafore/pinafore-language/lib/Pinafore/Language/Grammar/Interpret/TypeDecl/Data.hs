module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( makeDataTypeBox
    ) where

import qualified Data.List as List
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes
import Shapes.Unsafe (unsafeGetRefl)

-- | Structure of a datatype
data PinaforeDataType :: forall (k :: Type). k -> Type where
    NilDataType :: PinaforeDataType Void
    ConsDataType
        :: ListType (PinaforeNonpolarType '[]) tl -> PinaforeDataType tt -> PinaforeDataType (Either (HList tl) tt)

-- | Structural equality
instance TestHetEquality PinaforeDataType where
    testHetEquality NilDataType NilDataType = return HRefl
    testHetEquality (ConsDataType a1 ar) (ConsDataType b1 br) = do
        Refl <- testEquality a1 b1
        HRefl <- testHetEquality ar br
        return HRefl
    testHetEquality _ _ = Nothing

data DataBox =
    forall t. MkDataBox (PinaforeDataType t)
                        [Constructor (PinaforeNonpolarType '[]) t]

assembleDataType :: [(Name, AnyW (ListType (PinaforeNonpolarType '[])))] -> DataBox
assembleDataType [] = MkDataBox NilDataType []
assembleDataType ((n, MkAnyW el):cc) =
    case assembleDataType cc of
        MkDataBox ct conss ->
            MkDataBox (ConsDataType el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

data DataTypeFamily :: FamilyKind where
    MkDataTypeFamily :: forall (tid :: BigNat). TypeIDType tid -> DataTypeFamily (Identified tid)

instance TestHetEquality DataTypeFamily where
    testHetEquality (MkDataTypeFamily ia) (MkDataTypeFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

datatypeIOWitness :: IOWitness ('MkWitKind DataTypeFamily)
datatypeIOWitness = $(iowitness [t|'MkWitKind DataTypeFamily|])

interpretDataTypeConstructor ::
       SyntaxDatatypeConstructor -> PinaforeInterpreter (Name, AnyW (ListType (PinaforeNonpolarType '[])))
interpretDataTypeConstructor (MkSyntaxDatatypeConstructor consName stypes) = do
    etypes <- for stypes interpretNonpolarType
    return (consName, assembleListType etypes)

type TParam :: CCRVariance -> Type
data TParam sv where
    CoTParam :: SymbolType t -> TParam CoCCRVariance
    ContraTParam :: SymbolType t -> TParam ContraCCRVariance
    RangeTParam :: SymbolType p -> SymbolType q -> TParam 'RangeCCRVariance -- contra, co

tParamVars :: TParam sv -> [AnyW SymbolType]
tParamVars (CoTParam t) = [MkAnyW t]
tParamVars (ContraTParam t) = [MkAnyW t]
tParamVars (RangeTParam p q) = [MkAnyW p, MkAnyW q]

tParamVarianceType :: TParam sv -> CCRVarianceType sv
tParamVarianceType (CoTParam _) = CoCCRVarianceType
tParamVarianceType (ContraTParam _) = ContraCCRVarianceType
tParamVarianceType (RangeTParam _ _) = RangeCCRVarianceType

withTParam :: SyntaxDatatypeParameter -> (forall t. TParam t -> r) -> r
withTParam (PositiveSyntaxDatatypeParameter n) cont = nameToSymbolType n $ \v -> cont $ CoTParam v
withTParam (NegativeSyntaxDatatypeParameter n) cont = nameToSymbolType n $ \v -> cont $ ContraTParam v
withTParam (RangeSyntaxDatatypeParameter np nq) cont =
    nameToSymbolType np $ \vp -> nameToSymbolType nq $ \vq -> cont $ RangeTParam vp vq

type TParams :: DolanVariance -> Type
type TParams = ListType TParam

tParamsVars :: TParams dv -> [AnyW SymbolType]
tParamsVars pp = mconcat $ listTypeToList tParamVars pp

withTParams :: [SyntaxDatatypeParameter] -> (forall dv. TParams dv -> r) -> r
withTParams [] cont = cont NilListType
withTParams (sp:spp) cont = withTParam sp $ \p -> withTParams spp $ \pp -> cont $ ConsListType p pp

tParamsVarianceType :: TParams dv -> DolanVarianceType dv
tParamsVarianceType = mapListType tParamVarianceType

{-
type DolanVarianceMap :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data DolanVarianceMap dv f where
    NilDolanVarianceMap :: forall (f :: Type). DolanVarianceMap '[] f
    ConsDolanVarianceMap
        :: forall (sv :: CCRVariance) (dv :: DolanVariance) (f :: CCRVarianceKind sv -> DolanVarianceKind dv).
           HasCCRVariance sv f
        => (forall a. DolanVarianceMap dv (f a))
        -> DolanVarianceMap (sv ': dv) f
-}
paramToArg ::
       forall sv r.
       TParam sv
    -> (forall tp tn.
            (InKind tp, InKind tn) =>
                    SingleArgument sv PinaforeType 'Positive tp -> SingleArgument sv PinaforeType 'Negative tn -> CCRVarianceCategory (PinaforePolyShim Type) sv tn tp -> r)
    -> r
paramToArg (CoTParam var) call =
    call (singleDolanType $ VarDolanSingularType var) (singleDolanType $ VarDolanSingularType var) $ iJoinR1 . iMeetL1
paramToArg (ContraTParam var) call =
    call (singleDolanType $ VarDolanSingularType var) (singleDolanType $ VarDolanSingularType var) $
    MkCatDual $ iJoinR1 . iMeetL1
paramToArg (RangeTParam varp varq) call =
    call
        (MkRangeType (singleDolanType $ VarDolanSingularType varp) (singleDolanType $ VarDolanSingularType varq))
        (MkRangeType (singleDolanType $ VarDolanSingularType varp) (singleDolanType $ VarDolanSingularType varq)) $
    MkCatRange (iJoinR1 . iMeetL1) (iJoinR1 . iMeetL1)

paramsToDolanArgs ::
       forall (dv :: DolanVariance) (gtp :: DolanVarianceKind dv) (gtn :: DolanVarianceKind dv) r.
       TParams dv
    -> DolanVarianceMap dv gtp
    -> DolanVarianceMap dv gtn
    -> PinaforePolyShim (DolanVarianceKind dv) gtn gtp
    -> (forall tp tn.
                DolanArguments dv PinaforeType gtp 'Positive tp -> DolanArguments dv PinaforeType gtn 'Negative tn -> PinaforePolyShim Type tn tp -> r)
    -> r
paramsToDolanArgs NilListType NilDolanVarianceMap NilDolanVarianceMap conv call =
    call NilDolanArguments NilDolanArguments conv
paramsToDolanArgs (ConsListType p pp) (ConsDolanVarianceMap dvmp) (ConsDolanVarianceMap dvmn) conv call =
    paramToArg p $ \posarg negarg conv1 ->
        paramsToDolanArgs pp dvmp dvmn (applyPolyShim (tParamVarianceType p) conv conv1) $ \posargs negargs convs ->
            call (ConsDolanArguments posarg posargs) (ConsDolanArguments negarg negargs) convs

makeDataTypeBox ::
       Name
    -> Markdown
    -> [SyntaxDatatypeParameter]
    -> [SyntaxDatatypeConstructor]
    -> PinaforeInterpreter PinaforeTypeBox
makeDataTypeBox name doc params sconss =
    withTParams params $ \(tparams :: TParams dv) ->
        withRepresentative (tParamsVarianceType tparams) $
        newTypeID $ \(tidsym :: _ tid) ->
            case unsafeIdentifyKind @_ @(DolanVarianceKind dv) tidsym of
                Identity Refl ->
                    case dolanVarianceInKind @dv representative @(Identified tid) of
                        Dict ->
                            case dolanVarianceInCategory @PinaforePolyShim @dv representative of
                                Dict -> let
                                    mkgt ::
                                           DolanVarianceMap dv (Identified tid)
                                        -> PinaforeGroundType dv (Identified tid)
                                    mkgt dvm =
                                        MkPinaforeGroundType
                                            { pgtVarianceType = representative
                                            , pgtVarianceMap = dvm
                                            , pgtShowType = standardListTypeExprShow @dv $ exprShow name
                                            , pgtFamilyType = MkFamilyType datatypeIOWitness $ MkDataTypeFamily tidsym
                                            , pgtGreatestDynamicSupertype = \_ -> Nothing
                                            }
                                    mktype dvm = MkBoundType $ mkgt dvm
                                    in mkTypeFixBox name doc mktype $ do
                                           tconss <- for sconss interpretDataTypeConstructor
                                           MkDataBox (_dt :: _ dt) conss <- return $ assembleDataType tconss
                                           let
                                               freevars :: [AnyW SymbolType]
                                               freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                                               declaredvars :: [AnyW SymbolType]
                                               declaredvars = tParamsVars tparams
                                               unboundvars :: [AnyW SymbolType]
                                               unboundvars = freevars List.\\ declaredvars
                                           case nonEmpty unboundvars of
                                               Nothing -> return ()
                                               Just vv ->
                                                   throw $
                                                   InterpretUnboundTypeVariablesError $
                                                   fmap (\(MkAnyW s) -> symbolTypeToName s) vv
                                           dvm :: DolanVarianceMap dv (Identified tid) <-
                                               case tparams of
                                                   NilListType -> return NilDolanVarianceMap
                                                   _ -> throw $ UnicodeDecodeError "ISSUE #41"
                                           paramsToDolanArgs @dv @(Identified tid) @(Identified tid) tparams dvm dvm cid $ \(posargs :: _ tp) negargs conv -> do
                                               let
                                                   gt = mkgt dvm
                                                   ctfpos :: PinaforeShimWit 'Positive tp
                                                   ctfpos =
                                                       singleDolanShimWit $
                                                       mkPolarShimWit $ GroundedDolanSingularType gt posargs
                                                   ctfneg :: PinaforeShimWit 'Negative tp
                                                   ctfneg =
                                                       mapShimWit (MkPolarMap conv) $
                                                       singleDolanShimWit $
                                                       mkPolarShimWit $ GroundedDolanSingularType gt negargs
                                               tident <- unsafeGetRefl @Type @dt @tp
                                               let tiso = reflId tident
                                               patts <-
                                                   for conss $ \(MkConstructor cname lt at tma) -> do
                                                       ltp <- return $ mapListType nonpolarToDolanType lt
                                                       ltn <- return $ mapListType nonpolarToDolanType lt
                                                       let
                                                           expr =
                                                               qConstExprAny $
                                                               MkAnyValue (qFunctionPosWitnesses ltn ctfpos) $ \hl ->
                                                                   isoForwards tiso $ at hl
                                                           pc =
                                                               toPatternConstructor ctfneg ltp $ \t ->
                                                                   tma $ isoBackwards tiso t
                                                       withNewPatternConstructor cname doc expr pc
                                               return (dvm, compAll patts)
