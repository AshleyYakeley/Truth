module Pinafore.Language.Interpret.Type
    ( interpretType
    , interpretConcreteEntityType
    , interpretSubtypeRelation
    ) where

import Data.Shim
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Syntax
import Pinafore.Language.Type.Literal
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Show
import Shapes

type PinaforeTypeM baseupdate = MPolarW (PinaforeType baseupdate)

type PinaforeRangeType3 baseupdate = MPolarRangeType (PinaforeType baseupdate)

interpretType ::
       forall baseupdate polarity. Is PolarityType polarity
    => SyntaxType
    -> PinaforeSourceScoped baseupdate (AnyW (PinaforeType baseupdate polarity))
interpretType st = do
    mpol <- isMPolarity @polarity $ interpretTypeM @baseupdate @('Just polarity) st
    case mpol of
        SingleMPolarW atw -> return atw

interpretConcreteEntityType :: SyntaxType -> PinaforeSourceScoped baseupdate (AnyW ConcreteEntityType)
interpretConcreteEntityType st = do
    mpol <- interpretTypeM @_ @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkAnyW tm ->
                    case pinaforeToConcreteEntityType tm of
                        Just (MkShimWit t _) -> return $ MkAnyW t
                        Nothing -> throwError $ InterpretTypeNotEntityError $ exprShow tm

interpretTypeM ::
       forall baseupdate mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceScoped baseupdate (PinaforeTypeM baseupdate mpolarity)
interpretTypeM BottomSyntaxType =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> return $ toMPolar mempty
        MNegativeType -> throwError $ InterpretTypeExprBadLimitError Negative
        MBothType -> throwError $ InterpretTypeExprBadLimitError Negative
interpretTypeM TopSyntaxType =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> throwError $ InterpretTypeExprBadLimitError Positive
        MNegativeType -> return $ toMPolar mempty
        MBothType -> throwError $ InterpretTypeExprBadLimitError Positive
interpretTypeM (OrSyntaxType st1 st2) =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> do
            t1 <- interpretTypeM st1
            t2 <- interpretTypeM st2
            return $ toMPolar (<>) t1 t2
        MNegativeType -> throwError $ InterpretTypeExprBadJoinMeetError Negative
        MBothType -> throwError $ InterpretTypeExprBadJoinMeetError Negative
interpretTypeM (AndSyntaxType st1 st2) =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> throwError $ InterpretTypeExprBadJoinMeetError Positive
        MNegativeType -> do
            t1 <- interpretTypeM st1
            t2 <- interpretTypeM st2
            return $ toMPolar (<>) t1 t2
        MBothType -> throwError $ InterpretTypeExprBadJoinMeetError Positive
interpretTypeM (SingleSyntaxType sgt sargs) = do
    MkPinaforeGroundTypeM dvt pgt <- interpretGroundTypeConst @baseupdate @mpolarity sgt
    forMPolarW pgt $ \(MkAnyW gt) -> do
        aargs <- interpretArgs sgt dvt sargs
        case aargs of
            MkAnyW args -> return $ MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType gt args
interpretTypeM (VarSyntaxType name) =
    nameToSymbolType name $ \t -> return $ toMPolar $ MkAnyW $ singlePinaforeType $ VarPinaforeSingularType t

interpretTypeRangeFromType ::
       forall baseupdate mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceScoped baseupdate (PinaforeRangeType3 baseupdate mpolarity)
interpretTypeRangeFromType st = do
    t <- interpretTypeM @baseupdate @'Nothing st
    let
        ff :: forall polarity. Is PolarityType polarity
           => PinaforeTypeM baseupdate 'Nothing
           -> AnyInKind (RangeType (PinaforeType baseupdate) polarity)
        ff (BothMPolarW atw) =
            case (invertPolarity @polarity $ atw @(InvertPolarity polarity), atw @polarity) of
                (MkAnyW tp, MkAnyW tq) -> MkAnyInKind $ MkRangeType tp tq
    return $ toMPolar $ ff t

interpretTypeArgument ::
       forall baseupdate mpolarity. Is MPolarityType mpolarity
    => SyntaxTypeArgument
    -> PinaforeSourceScoped baseupdate (PinaforeRangeType3 baseupdate mpolarity)
interpretTypeArgument (SimpleSyntaxTypeArgument st) = interpretTypeRangeFromType st
interpretTypeArgument (RangeSyntaxTypeArgument ss) = do
    tt <- for ss interpretTypeRangeItem
    return $ mconcat tt

interpretTypeRangeItem ::
       forall baseupdate mpolarity. Is MPolarityType mpolarity
    => (Maybe SyntaxVariance, SyntaxType)
    -> PinaforeSourceScoped baseupdate (PinaforeRangeType3 baseupdate mpolarity)
interpretTypeRangeItem (Just CoSyntaxVariance, st) = do
    atq <- interpretTypeM st
    return $ toMPolar (\(MkAnyW tq) -> MkAnyInKind $ MkRangeType NilPinaforeType tq) atq
interpretTypeRangeItem (Just ContraSyntaxVariance, st) = do
    atp <- invertMPolarity @mpolarity $ interpretTypeM st
    return $ toMPolar (\(MkAnyW tp) -> MkAnyInKind $ MkRangeType tp NilPinaforeType) (MkInvertMPolarW atp)
interpretTypeRangeItem (Nothing, st) = interpretTypeRangeFromType st

groundTypeText :: SyntaxGroundType -> Text
groundTypeText (ConstSyntaxGroundType n) = unName n
groundTypeText FunctionSyntaxGroundType = "->"
groundTypeText MorphismSyntaxGroundType = "~>"
groundTypeText ListSyntaxGroundType = "[]"
groundTypeText PairSyntaxGroundType = "(,)"
groundTypeText UnitSyntaxGroundType = "()"

data PinaforeGroundTypeM baseupdate mpolarity where
    MkPinaforeGroundTypeM
        :: DolanVarianceType dv
        -> MPolarW (PinaforeGroundType baseupdate dv) mpolarity
        -> PinaforeGroundTypeM baseupdate mpolarity

interpretArgs ::
       forall baseupdate polarity dv (gt :: DolanVarianceKind dv). Is PolarityType polarity
    => SyntaxGroundType
    -> DolanVarianceType dv
    -> [SyntaxTypeArgument]
    -> PinaforeSourceScoped baseupdate (AnyW (DolanArguments dv (PinaforeType baseupdate) gt polarity))
interpretArgs _ NilListType [] = return $ MkAnyW NilDolanArguments
interpretArgs sgt NilListType (_:_) = throwError $ InterpretTypeOverApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType _ _) [] = throwError $ InterpretTypeUnderApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType CovarianceType dv) (SimpleSyntaxTypeArgument st:stt) = do
    at <- isMPolarity @polarity $ interpretTypeM st
    case fromMPolarSingle at of
        MkAnyW t -> do
            aargs <- interpretArgs sgt dv stt
            case aargs of
                MkAnyW args -> return $ MkAnyW $ ConsDolanArguments t args
interpretArgs sgt (ConsListType CovarianceType _) (RangeSyntaxTypeArgument _:_) =
    throwError $ InterpretTypeRangeApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType ContravarianceType dv) (SimpleSyntaxTypeArgument st:stt) =
    invertPolarity @polarity $ do
        at <- isMPolarity @(InvertPolarity polarity) $ interpretTypeM st
        case fromMPolarSingle at of
            MkAnyW t -> do
                aargs <- interpretArgs sgt dv stt
                case aargs of
                    MkAnyW args -> return $ MkAnyW $ ConsDolanArguments t args
interpretArgs sgt (ConsListType ContravarianceType _) (RangeSyntaxTypeArgument _:_) =
    throwError $ InterpretTypeRangeApplyError $ groundTypeText sgt
interpretArgs sgt (ConsListType RangevarianceType dv) (st:stt) = do
    at <- isMPolarity @polarity $ interpretTypeArgument st
    case fromMPolarSingle at of
        MkAnyInKind t -> do
            aargs <- interpretArgs sgt dv stt
            case aargs of
                MkAnyW args -> return $ MkAnyW $ ConsDolanArguments t args

interpretGroundTypeConst ::
       forall baseupdate mpolarity. Is MPolarityType mpolarity
    => SyntaxGroundType
    -> PinaforeSourceScoped baseupdate (PinaforeGroundTypeM baseupdate mpolarity)
interpretGroundTypeConst UnitSyntaxGroundType =
    return $
    MkPinaforeGroundTypeM representative $
    toMPolar $ MkAnyW $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType UnitLiteralType
interpretGroundTypeConst FunctionSyntaxGroundType =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW FuncPinaforeGroundType
interpretGroundTypeConst MorphismSyntaxGroundType =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW MorphismPinaforeGroundType
interpretGroundTypeConst ListSyntaxGroundType =
    return $
    MkPinaforeGroundTypeM representative $
    toMPolar $ MkAnyW $ EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType
interpretGroundTypeConst PairSyntaxGroundType =
    return $
    MkPinaforeGroundTypeM representative $
    toMPolar $
    MkAnyW $ EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Maybe") =
    return $
    MkPinaforeGroundTypeM representative $
    toMPolar $ MkAnyW $ EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Either") =
    return $
    MkPinaforeGroundTypeM representative $
    toMPolar $
    MkAnyW $ EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Ref") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW RefPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "SetRef") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW SetRefPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "FiniteSetRef") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW FiniteSetRefPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Action") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW ActionPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Order") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW OrderPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "UI") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW UserInterfacePinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Notifier") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW NotifierPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Window") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW WindowPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "MenuItem") =
    return $ MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW MenuItemPinaforeGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "Entity") =
    return $
    MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW $ EntityPinaforeGroundType NilListType TopEntityGroundType
interpretGroundTypeConst (ConstSyntaxGroundType "NewEntity") =
    return $
    MkPinaforeGroundTypeM representative $ toMPolar $ MkAnyW $ EntityPinaforeGroundType NilListType NewEntityGroundType
interpretGroundTypeConst (ConstSyntaxGroundType n)
    | Just (MkAnyW lt) <- nameToLiteralType n =
        return $
        MkPinaforeGroundTypeM representative $
        toMPolar $ MkAnyW $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType lt
interpretGroundTypeConst (ConstSyntaxGroundType n) = do
    (tid, nt) <- lookupNamedType n
    case nt of
        OpenEntityNamedType ->
            valueToWitness tid $ \sw ->
                return $
                MkPinaforeGroundTypeM representative $
                toMPolar $ MkAnyW $ EntityPinaforeGroundType NilListType $ OpenEntityGroundType n sw
        ClosedEntityNamedType (MkAnyW ct) ->
            valueToWitness tid $ \sw ->
                return $
                MkPinaforeGroundTypeM representative $
                toMPolar $ MkAnyW $ EntityPinaforeGroundType NilListType $ ClosedEntityGroundType n sw ct

interpretSubtypeRelation ::
       SyntaxType
    -> SyntaxType
    -> PinaforeSourceScoped baseupdate (WMFunction (PinaforeScoped baseupdate) (PinaforeScoped baseupdate))
interpretSubtypeRelation sta stb = do
    ata <- interpretConcreteEntityType sta
    atb <- interpretConcreteEntityType stb
    case ata of
        MkAnyW ta ->
            case ta of
                MkConcreteType (OpenEntityGroundType _ tida) NilArguments ->
                    case atb of
                        MkAnyW tb ->
                            case tb of
                                MkConcreteType (OpenEntityGroundType _ tidb) NilArguments -> withEntitySubtype tida tidb
                                _ -> throwError $ TypeNotOpenEntityError $ exprShow tb
                _ -> throwError $ TypeNotOpenEntityError $ exprShow ta
