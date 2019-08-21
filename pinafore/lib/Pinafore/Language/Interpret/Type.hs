module Pinafore.Language.Interpret.Type
    ( interpretType
    , interpretEntityType
    ) where

import Data.Shim
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Syntax
import Pinafore.Language.Type.Literal
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Show
import Shapes

type PinaforeTypeM baseedit = MPolarW (PinaforeType baseedit)

type PinaforeRangeType3 baseedit = MPolarRangeType (PinaforeType baseedit)

interpretType ::
       forall baseedit polarity. Is PolarityType polarity
    => SyntaxType
    -> PinaforeSourceScoped baseedit (AnyW (PinaforeType baseedit polarity))
interpretType st = do
    mpol <- isMPolarity @polarity $ interpretTypeM @baseedit @('Just polarity) st
    case mpol of
        SingleMPolarW atw -> return atw

interpretEntityType :: SyntaxType -> PinaforeSourceScoped baseedit (AnyW EntityType)
interpretEntityType st = do
    mpol <- interpretTypeM @_ @'Nothing st
    case mpol of
        BothMPolarW atm ->
            case atm @'Positive of
                MkAnyW tm ->
                    case pinaforeToEntityType tm of
                        Just (MkShimWit t _) -> return $ MkAnyW t
                        Nothing -> throwError $ InterpretTypeNotEntityError $ exprShow tm

interpretTypeM ::
       forall baseedit mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceScoped baseedit (PinaforeTypeM baseedit mpolarity)
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
interpretTypeM (MorphismSyntaxType st1 st2) = do
    at1 <- interpretTypeRange st1
    at2 <- interpretTypeRange st2
    return $
        toMPolar
            (\(MkAnyInKind t1) (MkAnyInKind t2) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType MorphismPinaforeGroundType $
                 ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
            at1
            at2
interpretTypeM (FunctionSyntaxType st1 st2) = do
    at1 <- invertMPolarity @mpolarity $ interpretTypeM st1
    at2 <- interpretTypeM st2
    return $
        toMPolar
            (\(MkAnyW t1) (MkAnyW t2) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType FuncPinaforeGroundType $
                 ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
            (MkInvertMPolarW at1)
            at2
interpretTypeM (MaybeSyntaxType st1) = do
    at1 <- interpretTypeM st1
    return $
        toMPolar
            (\(MkAnyW t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType
                     (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
                 ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (ListSyntaxType st1) = do
    at1 <- interpretTypeM st1
    return $
        toMPolar
            (\(MkAnyW t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType
                     (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
                 ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (EitherSyntaxType st1 st2) = do
    at1 <- interpretTypeM st1
    at2 <- interpretTypeM st2
    return $
        toMPolar
            (\(MkAnyW t1) (MkAnyW t2) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType
                     (EntityPinaforeGroundType
                          (ConsListType Refl $ ConsListType Refl NilListType)
                          EitherEntityGroundType) $
                 ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
            at1
            at2
interpretTypeM (PairSyntaxType st1 st2) = do
    at1 <- interpretTypeM st1
    at2 <- interpretTypeM st2
    return $
        toMPolar
            (\(MkAnyW t1) (MkAnyW t2) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType
                     (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                 ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
            at1
            at2
interpretTypeM (ActionSyntaxType st1) = do
    at1 <- interpretTypeM st1
    return $
        toMPolar
            (\(MkAnyW t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (UISyntaxType st1) = do
    at1 <- interpretTypeM st1
    return $
        toMPolar
            (\(MkAnyW t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType UserInterfacePinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (RefSyntaxType st1) = do
    at1 <- interpretTypeRange st1
    return $
        toMPolar
            (\(MkAnyInKind t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (SetSyntaxType st1) = do
    at1 <- interpretTypeRange st1
    return $
        toMPolar
            (\(MkAnyInKind t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (OrderSyntaxType st1) = do
    at1 <- invertMPolarity @mpolarity $ interpretTypeM st1
    return $
        toMPolar
            (\(MkAnyW t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            (MkInvertMPolarW at1)
interpretTypeM (VarSyntaxType name) =
    nameToSymbolType name $ \t -> return $ toMPolar $ MkAnyW $ singlePinaforeType $ VarPinaforeSingularType t
interpretTypeM UnitSyntaxType = return $ toMPolar $ MkAnyW $ literalPinaforeType UnitLiteralType
interpretTypeM (ConstSyntaxType name) = interpretTypeConst name
interpretTypeM (RangeSyntaxType _) = throwError InterpretTypeRangeInTypeError

interpretTypeRangeFromType ::
       forall baseedit mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceScoped baseedit (PinaforeRangeType3 baseedit mpolarity)
interpretTypeRangeFromType st = do
    t <- interpretTypeM @baseedit @'Nothing st
    let
        ff :: forall polarity. Is PolarityType polarity
           => PinaforeTypeM baseedit 'Nothing
           -> AnyInKind (RangeType (PinaforeType baseedit) polarity)
        ff (BothMPolarW atw) =
            case (invertPolarity @polarity $ atw @(InvertPolarity polarity), atw @polarity) of
                (MkAnyW tp, MkAnyW tq) -> MkAnyInKind $ MkRangeType tp tq
    return $ toMPolar $ ff t

interpretTypeRange ::
       forall baseedit mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceScoped baseedit (PinaforeRangeType3 baseedit mpolarity)
interpretTypeRange (RangeSyntaxType ss) = do
    tt <- for ss interpretTypeRangeItem
    return $ mconcat tt
interpretTypeRange st = interpretTypeRangeFromType st

interpretTypeRangeItem ::
       forall baseedit mpolarity. Is MPolarityType mpolarity
    => (Maybe SyntaxVariance, SyntaxType)
    -> PinaforeSourceScoped baseedit (PinaforeRangeType3 baseedit mpolarity)
interpretTypeRangeItem (Just CoSyntaxVariance, st) = do
    atq <- interpretTypeM st
    return $ toMPolar (\(MkAnyW tq) -> MkAnyInKind $ MkRangeType NilPinaforeType tq) atq
interpretTypeRangeItem (Just ContraSyntaxVariance, st) = do
    atp <- invertMPolarity @mpolarity $ interpretTypeM st
    return $ toMPolar (\(MkAnyW tp) -> MkAnyInKind $ MkRangeType tp NilPinaforeType) (MkInvertMPolarW atp)
interpretTypeRangeItem (Nothing, st) = interpretTypeRangeFromType st

interpretTypeConst ::
       forall baseedit mpolarity. Is MPolarityType mpolarity
    => Name
    -> PinaforeSourceScoped baseedit (PinaforeTypeM baseedit mpolarity)
interpretTypeConst "Window" =
    return $
    toMPolar $ MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments
interpretTypeConst "MenuItem" =
    return $
    toMPolar $ MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments
interpretTypeConst "Entity" =
    return $
    toMPolar $
    MkAnyW $
    singlePinaforeType $
    GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments
interpretTypeConst "NewEntity" =
    return $
    toMPolar $
    MkAnyW $
    singlePinaforeType $
    GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments
interpretTypeConst n
    | Just (MkAnyW lt) <- nameToLiteralType n = return $ toMPolar $ MkAnyW $ literalPinaforeType lt
interpretTypeConst n = do
    (tid, nt) <- lookupNamedType n
    case nt of
        OpenEntityNamedType ->
            valueToWitness tid $ \sw ->
                return $
                toMPolar $
                MkAnyW $
                singlePinaforeType $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType NilListType $ OpenEntityGroundType n sw)
                    NilDolanArguments
        ClosedEntityNamedType (MkAnyW ct) ->
            valueToWitness tid $ \sw ->
                return $
                toMPolar $
                MkAnyW $
                singlePinaforeType $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType n sw ct)
                    NilDolanArguments
