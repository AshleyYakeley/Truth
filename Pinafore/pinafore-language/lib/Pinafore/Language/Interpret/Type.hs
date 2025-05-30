module Pinafore.Language.Interpret.Type
    ( interpretType
    , interpretNonpolarType
    , interpretNonpolarGroundedType
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

type PinaforeTypeM = MPolarW QType

type PinaforeRangeType3 = MPolarRangeType QType

interpretType ::
    forall polarity.
    Is PolarityType polarity =>
    SyntaxType ->
    QInterpreter (Some (QType polarity))
interpretType st = do
    mpol <- isMPolarity @polarity $ interpretTypeM @('Just polarity) st
    case mpol of
        SingleMPolarW atw -> return atw

interpretNonpolarType :: SyntaxType -> QInterpreter (Some QNonpolarType)
interpretNonpolarType st = do
    mpol <- interpretTypeM @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkSome tm ->
                    case unrollTopType tm of
                        MkShimWit tm' _ ->
                            case positiveToNonpolar @QTypeSystem tm' of
                                Just t -> return $ shimWitToSome t
                                Nothing -> throw $ InterpretTypeNotAmbipolarError $ exprShow tm

interpretNonpolarGroundedType :: SyntaxType -> QInterpreter (Some QNonpolarGroundedType)
interpretNonpolarGroundedType st = do
    t <- interpretNonpolarType st
    case t of
        MkSome (ToGroundedNonpolarType gnt) -> return $ MkSome gnt
        _ -> throw $ InterpretTypeNotGroundedError $ exprShow t

interpretTypeM ::
    forall mpolarity.
    Is MPolarityType mpolarity =>
    SyntaxType ->
    QInterpreter (PinaforeTypeM mpolarity)
interpretTypeM (MkWithSourcePos spos t) = paramWith sourcePosParam spos $ interpretTypeM' t

interpretTypeM' ::
    forall mpolarity.
    Is MPolarityType mpolarity =>
    SyntaxType' ->
    QInterpreter (PinaforeTypeM mpolarity)
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
        MkSome gt ->
            toMPolarWM $ do
                aargs <- interpretArgs sgt (groundTypeVarianceType gt) sargs
                case aargs of
                    MkSome args -> return $ typeToSomeDolan $ MkDolanGroundedType gt args
interpretTypeM' (VarSyntaxType name) =
    nameToTypeVarT name $ \t -> return $ toMPolar $ MkSome $ singleDolanType $ VarDolanSingularType t
interpretTypeM' (RecursiveSyntaxType name st) = do
    mt <- interpretTypeM st
    nameToTypeVarT name $ \var ->
        mapMPolarWM
            ( \(MkSome t) ->
                assignTypeVarWit var t
                    $ case safeRecursiveDolanSingularType var t of
                        SuccessResult rt -> return $ MkSome $ singleDolanType rt
                        FailureResult (ImmediateRecursiveTypeError _) ->
                            throw $ InterpretTypeRecursionImmediate name $ exprShow t
                        FailureResult (ContravariantRecursiveTypeError _) ->
                            throw $ InterpretTypeRecursionNotCovariant name $ exprShow t
            )
            mt

interpretTypeRangeFromType ::
    forall mpolarity.
    Is MPolarityType mpolarity =>
    SyntaxType ->
    QInterpreter (PinaforeRangeType3 mpolarity)
interpretTypeRangeFromType st = do
    t <- interpretTypeM @'Nothing st
    let
        ff ::
            forall polarity.
            Is PolarityType polarity =>
            PinaforeTypeM 'Nothing ->
            Some (RangeType QType polarity)
        ff (BothMPolarW atw) =
            case (withInvertPolarity @polarity $ atw @(InvertPolarity polarity), atw @polarity) of
                (MkSome tp, MkSome tq) -> MkSome $ MkRangeType tp tq
    return $ toMPolar $ ff t

interpretTypeArgument ::
    forall mpolarity.
    Is MPolarityType mpolarity =>
    SyntaxTypeArgument ->
    QInterpreter (PinaforeRangeType3 mpolarity)
interpretTypeArgument (MkSyntaxTypeArgument ss) = do
    tt <- for ss interpretTypeRangeItem
    return $ mconcat tt

interpretTypeRangeItem ::
    forall mpolarity.
    Is MPolarityType mpolarity =>
    (Maybe SyntaxVariance, SyntaxType) ->
    QInterpreter (PinaforeRangeType3 mpolarity)
interpretTypeRangeItem (Just CoSyntaxVariance, st) = do
    atq <- interpretTypeM st
    return $ toMPolar (\(MkSome tq) -> MkSome $ MkRangeType NilDolanType tq) atq
interpretTypeRangeItem (Just ContraSyntaxVariance, st) = do
    atp <- invertMPolarity @mpolarity $ interpretTypeM st
    return $ toMPolar (\(MkSome tp) -> MkSome $ MkRangeType tp NilDolanType) (MkInvertMPolarW atp)
interpretTypeRangeItem (Nothing, st) = interpretTypeRangeFromType st

groundTypeText :: SyntaxGroundType -> NamedText
groundTypeText (ConstSyntaxGroundType n) = showNamedText n

data PinaforeGroundTypeM where
    MkPinaforeGroundTypeM :: Some (QGroundType dv) -> PinaforeGroundTypeM

interpretArgs ::
    forall polarity dv (gt :: CCRVariancesKind dv).
    Is PolarityType polarity =>
    SyntaxGroundType ->
    CCRVariancesType dv ->
    [SyntaxTypeArgument] ->
    QInterpreter (Some (CCRPolarArguments dv QType gt polarity))
interpretArgs _ NilListType [] = return $ MkSome NilCCRArguments
interpretArgs sgt NilListType (_ : _) = throw $ InterpretTypeOverApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType _ _) [] = throw $ InterpretTypeUnderApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType CoCCRVarianceType dv) (SimpleSyntaxTypeArgument st : stt) = do
    at <- isMPolarity @polarity $ interpretTypeM st
    case fromMPolarSingle at of
        MkSome t -> do
            aargs <- interpretArgs sgt dv stt
            case aargs of
                MkSome args -> return $ MkSome $ ConsCCRArguments (CoCCRPolarArgument t) args
interpretArgs sgt (ConsListType CoCCRVarianceType _) (MkSyntaxTypeArgument _ : _) =
    throw $ InterpretTypeRangeApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType ContraCCRVarianceType dv) (SimpleSyntaxTypeArgument st : stt) =
    withInvertPolarity @polarity $ do
        at <- isMPolarity @(InvertPolarity polarity) $ interpretTypeM st
        case fromMPolarSingle at of
            MkSome t -> do
                aargs <- interpretArgs sgt dv stt
                case aargs of
                    MkSome args -> return $ MkSome $ ConsCCRArguments (ContraCCRPolarArgument t) args
interpretArgs sgt (ConsListType ContraCCRVarianceType _) (MkSyntaxTypeArgument _ : _) =
    throw $ InterpretTypeRangeApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType RangeCCRVarianceType dv) (st : stt) = do
    at <- isMPolarity @polarity $ interpretTypeArgument @('Just polarity) st
    case fromMPolarSingle at of
        MkSome (MkRangeType tp tq) -> do
            aargs <- interpretArgs sgt dv stt
            case aargs of
                MkSome args -> return $ MkSome $ ConsCCRArguments (RangeCCRPolarArgument tp tq) args

interpretGroundTypeConst :: SyntaxGroundType -> QInterpreter PinaforeGroundTypeM
interpretGroundTypeConst (ConstSyntaxGroundType n) = do
    MkSomeGroundType t <- lookupBoundType n
    return $ MkPinaforeGroundTypeM $ MkSome t
