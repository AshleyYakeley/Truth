module Pinafore.Language.Grammar.Interpret.Type
    ( interpretType
    , interpretOpenEntityType
    , interpretMonoEntityType
    , interpretConcreteDynamicEntityType
    , interpretNonpolarType
    , interpretSubtypeRelation
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Interpret.ScopeBuilder
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

type PinaforeTypeM = MPolarW PinaforeType

type PinaforeRangeType3 = MPolarRangeType PinaforeType

interpretType ::
       forall polarity. Is PolarityType polarity
    => SyntaxType
    -> PinaforeSourceInterpreter (AnyW (PinaforeType polarity))
interpretType st = do
    mpol <- isMPolarity @polarity $ interpretTypeM @('Just polarity) st
    case mpol of
        SingleMPolarW atw -> return atw

interpretOpenEntityType :: SyntaxType -> PinaforeSourceInterpreter (AnyW OpenEntityType)
interpretOpenEntityType st = do
    mpol <- interpretTypeM @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkAnyW tm ->
                    case dolanTypeToSingular tm of
                        Just (MkAnyW (GroundedDolanSingularType (EntityPinaforeGroundType _ (OpenEntityGroundType t)) NilDolanArguments)) ->
                            return $ MkAnyW t
                        _ -> throw $ InterpretTypeNotOpenEntityError $ exprShow tm

interpretConcreteDynamicEntityType :: SyntaxType -> PinaforeSourceInterpreter (Name, DynamicType)
interpretConcreteDynamicEntityType st = do
    mpol <- interpretTypeM @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkAnyW tm ->
                    case dolanTypeToSingular tm of
                        Just (MkAnyW (GroundedDolanSingularType (EntityPinaforeGroundType _ (ADynamicEntityGroundType n dts)) NilDolanArguments))
                            | [dt] <- toList dts -> return (n, dt)
                        _ -> throw $ InterpretTypeNotConcreteDynamicEntityError $ exprShow tm

interpretMonoEntityType :: SyntaxType -> PinaforeSourceInterpreter (AnyW MonoEntityType)
interpretMonoEntityType st = do
    mpol <- interpretTypeM @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkAnyW tm ->
                    case dolanToMonoType tm of
                        Just (MkShimWit t _) -> return $ MkAnyW t
                        Nothing -> throw $ InterpretTypeNotEntityError $ exprShow tm

interpretNonpolarType :: SyntaxType -> PinaforeSourceInterpreter (AnyW (PinaforeNonpolarType '[]))
interpretNonpolarType st = do
    mpol <- interpretTypeM @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkAnyW tm ->
                    case dolanTypeToNonpolar tm of
                        Just t -> return t
                        Nothing -> throw $ InterpretTypeNotAmbipolarError $ exprShow tm

interpretTypeM ::
       forall mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceInterpreter (PinaforeTypeM mpolarity)
interpretTypeM (MkWithSourcePos spos t) = localSourcePos spos $ interpretTypeM' t

interpretTypeM' ::
       forall mpolarity. Is MPolarityType mpolarity
    => SyntaxType'
    -> PinaforeSourceInterpreter (PinaforeTypeM mpolarity)
interpretTypeM' BottomSyntaxType =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> return $ toMPolar mempty
        MNegativeType -> throw $ InterpretTypeExprBadLimitError Negative
        MBothType -> throw $ InterpretTypeExprBadLimitError Negative
interpretTypeM' TopSyntaxType =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> throw $ InterpretTypeExprBadLimitError Positive
        MNegativeType -> return $ toMPolar mempty
        MBothType -> throw $ InterpretTypeExprBadLimitError Positive
interpretTypeM' (OrSyntaxType st1 st2) =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> do
            t1 <- interpretTypeM st1
            t2 <- interpretTypeM st2
            return $ toMPolar (<>) t1 t2
        MNegativeType -> throw $ InterpretTypeExprBadJoinMeetError Negative
        MBothType -> throw $ InterpretTypeExprBadJoinMeetError Negative
interpretTypeM' (AndSyntaxType st1 st2) =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> throw $ InterpretTypeExprBadJoinMeetError Positive
        MNegativeType -> do
            t1 <- interpretTypeM st1
            t2 <- interpretTypeM st2
            return $ toMPolar (<>) t1 t2
        MBothType -> throw $ InterpretTypeExprBadJoinMeetError Positive
interpretTypeM' (SingleSyntaxType sgt sargs) = do
    MkPinaforeGroundTypeM agt <- interpretGroundTypeConst sgt
    case agt of
        MkAnyW gt ->
            toMPolarWM $ do
                aargs <- interpretArgs sgt (groundTypeVarianceType gt) sargs
                case aargs of
                    MkAnyW args -> return $ MkAnyW $ singleDolanType $ GroundedDolanSingularType gt args
interpretTypeM' (VarSyntaxType name) =
    nameToSymbolType name $ \t -> return $ toMPolar $ MkAnyW $ singleDolanType $ VarDolanSingularType t
interpretTypeM' (RecursiveSyntaxType name st) = do
    mt <- interpretTypeM st
    nameToSymbolType name $ \var ->
        return $
        mapMPolarW (\(MkAnyW t) -> assignUVarWit var t $ MkAnyW $ singleDolanType $ RecursiveDolanSingularType var t) mt

interpretTypeRangeFromType ::
       forall mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceInterpreter (PinaforeRangeType3 mpolarity)
interpretTypeRangeFromType st = do
    t <- interpretTypeM @'Nothing st
    let
        ff :: forall polarity. Is PolarityType polarity
           => PinaforeTypeM 'Nothing
           -> AnyInKind (RangeType PinaforeType polarity)
        ff (BothMPolarW atw) =
            case (invertPolarity @polarity $ atw @(InvertPolarity polarity), atw @polarity) of
                (MkAnyW tp, MkAnyW tq) -> MkAnyInKind $ MkRangeType tp tq
    return $ toMPolar $ ff t

interpretTypeArgument ::
       forall mpolarity. Is MPolarityType mpolarity
    => SyntaxTypeArgument
    -> PinaforeSourceInterpreter (PinaforeRangeType3 mpolarity)
interpretTypeArgument (SimpleSyntaxTypeArgument st) = interpretTypeRangeFromType st
interpretTypeArgument (RangeSyntaxTypeArgument ss) = do
    tt <- for ss interpretTypeRangeItem
    return $ mconcat tt

interpretTypeRangeItem ::
       forall mpolarity. Is MPolarityType mpolarity
    => (Maybe SyntaxVariance, SyntaxType)
    -> PinaforeSourceInterpreter (PinaforeRangeType3 mpolarity)
interpretTypeRangeItem (Just CoSyntaxVariance, st) = do
    atq <- interpretTypeM st
    return $ toMPolar (\(MkAnyW tq) -> MkAnyInKind $ MkRangeType NilDolanType tq) atq
interpretTypeRangeItem (Just ContraSyntaxVariance, st) = do
    atp <- invertMPolarity @mpolarity $ interpretTypeM st
    return $ toMPolar (\(MkAnyW tp) -> MkAnyInKind $ MkRangeType tp NilDolanType) (MkInvertMPolarW atp)
interpretTypeRangeItem (Nothing, st) = interpretTypeRangeFromType st

groundTypeText :: SyntaxGroundType -> Text
groundTypeText (ConstSyntaxGroundType n) = toText n
groundTypeText FunctionSyntaxGroundType = "->"
groundTypeText MorphismSyntaxGroundType = "~>"
groundTypeText ListSyntaxGroundType = "[]"
groundTypeText PairSyntaxGroundType = "(,)"
groundTypeText UnitSyntaxGroundType = "()"

data PinaforeGroundTypeM where
    MkPinaforeGroundTypeM :: AnyW (PinaforeGroundType dv) -> PinaforeGroundTypeM

interpretArgs ::
       forall polarity dv (gt :: DolanVarianceKind dv). Is PolarityType polarity
    => SyntaxGroundType
    -> DolanVarianceType dv
    -> [SyntaxTypeArgument]
    -> PinaforeSourceInterpreter (AnyW (DolanArguments dv PinaforeType gt polarity))
interpretArgs _ NilListType [] = return $ MkAnyW NilDolanArguments
interpretArgs sgt NilListType (_:_) = throw $ InterpretTypeOverApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType _ _) [] = throw $ InterpretTypeUnderApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType CoCCRVarianceType dv) (SimpleSyntaxTypeArgument st:stt) = do
    at <- isMPolarity @polarity $ interpretTypeM st
    case fromMPolarSingle at of
        MkAnyW t -> do
            aargs <- interpretArgs sgt dv stt
            case aargs of
                MkAnyW args -> return $ MkAnyW $ ConsDolanArguments t args
interpretArgs sgt (ConsListType CoCCRVarianceType _) (RangeSyntaxTypeArgument _:_) =
    throw $ InterpretTypeRangeApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType ContraCCRVarianceType dv) (SimpleSyntaxTypeArgument st:stt) =
    invertPolarity @polarity $ do
        at <- isMPolarity @(InvertPolarity polarity) $ interpretTypeM st
        case fromMPolarSingle at of
            MkAnyW t -> do
                aargs <- interpretArgs sgt dv stt
                case aargs of
                    MkAnyW args -> return $ MkAnyW $ ConsDolanArguments t args
interpretArgs sgt (ConsListType ContraCCRVarianceType _) (RangeSyntaxTypeArgument _:_) =
    throw $ InterpretTypeRangeApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType RangeCCRVarianceType dv) (st:stt) = do
    at <- isMPolarity @polarity $ interpretTypeArgument st
    case fromMPolarSingle at of
        MkAnyInKind t -> do
            aargs <- interpretArgs sgt dv stt
            case aargs of
                MkAnyW args -> return $ MkAnyW $ ConsDolanArguments t args

interpretGroundTypeConst :: SyntaxGroundType -> PinaforeSourceInterpreter PinaforeGroundTypeM
interpretGroundTypeConst UnitSyntaxGroundType =
    return $
    MkPinaforeGroundTypeM $ MkAnyW $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType UnitLiteralType
interpretGroundTypeConst FunctionSyntaxGroundType = return $ MkPinaforeGroundTypeM $ MkAnyW funcGroundType
interpretGroundTypeConst MorphismSyntaxGroundType = return $ MkPinaforeGroundTypeM $ MkAnyW morphismGroundType
interpretGroundTypeConst ListSyntaxGroundType =
    return $
    MkPinaforeGroundTypeM $ MkAnyW $ EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType
interpretGroundTypeConst PairSyntaxGroundType =
    return $
    MkPinaforeGroundTypeM $
    MkAnyW $ EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType
interpretGroundTypeConst (ConstSyntaxGroundType n) = do
    MkBoundType t <- lookupBoundType n
    return $ MkPinaforeGroundTypeM $ MkAnyW t

interpretSubtypeRelation' :: SourcePos -> SyntaxType -> SyntaxType -> ScopeBuilder ()
interpretSubtypeRelation' spos sta stb =
    interpScopeBuilder $
    mapSourcePos spos $ \ma -> do
        ata <- interpretMonoEntityType sta
        atb <- interpretMonoEntityType stb
        case ata of
            MkAnyW ta ->
                case ta of
                    MkMonoType tea NilArguments ->
                        case atb of
                            MkAnyW tb ->
                                case tb of
                                    MkMonoType (OpenEntityGroundType tidb) NilArguments ->
                                        remonadSourcePos (withEntitySubtype tea tidb) ma
                                    _ -> throw $ TypeNotOpenEntityError $ exprShow tb
                    _ -> throw $ TypeNotSimpleEntityError $ exprShow ta

interpretSubtypeRelation :: SourcePos -> Markdown -> SyntaxType -> SyntaxType -> ScopeBuilder Docs
interpretSubtypeRelation spos docDescription sta stb = do
    interpretSubtypeRelation' spos sta stb
    let
        docName = exprShow sta <> " <: " <> exprShow stb
        docValueType = ""
        docType = SubtypeRelationDocType
    return $ defDocs MkDefDoc {..}
