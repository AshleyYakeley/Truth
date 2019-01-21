module Pinafore.Language.Interpret.Type
    ( interpretType
    , interpretEntityType
    ) where

import Pinafore.Language.EntityType
import Pinafore.Language.Literal
import Pinafore.Language.Name
import Pinafore.Language.Syntax
import Pinafore.Language.Type
import Shapes

type PinaforeTypeM baseedit = MPolarW (PinaforeType baseedit)

type PinaforeRangeType3 baseedit = MPolarRangeType (PinaforeType baseedit)

interpretType ::
       forall baseedit polarity. Is PolarityType polarity
    => SyntaxType
    -> PinaforeSourceScoped baseedit (AnyW (PinaforeType baseedit polarity))
interpretType st = do
    SingleMPolarW atw <- isMPolarity @polarity $ interpretTypeM @baseedit @('Just polarity) st
    return atw

interpretEntityType :: SyntaxType -> PinaforeSourceScoped baseedit (AnyW EntityType)
interpretEntityType st = do
    BothMPolarW atm <- interpretTypeM @_ @'Nothing st
    case atm @'Positive of
        MkAnyW tm ->
            case typeToEntityType tm of
                Just t -> return t
                Nothing -> fail $ show tm <> " is not an Entity type"

interpretTypeM ::
       forall baseedit mpolarity. Is MPolarityType mpolarity
    => SyntaxType
    -> PinaforeSourceScoped baseedit (PinaforeTypeM baseedit mpolarity)
interpretTypeM BottomSyntaxType =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> return $ toMPolar mempty
        MNegativeType -> fail $ "\"None\" not allowed in negative types"
        MBothType -> fail $ "\"None\" not allowed in negative types"
interpretTypeM TopSyntaxType =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> fail $ "\"Any\" not allowed in positive types"
        MNegativeType -> return $ toMPolar mempty
        MBothType -> fail $ "\"Any\" not allowed in positive types"
interpretTypeM (OrSyntaxType st1 st2) =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> do
            t1 <- interpretTypeM st1
            t2 <- interpretTypeM st2
            return $ toMPolar (<>) t1 t2
        MNegativeType -> fail $ "\"|\" not allowed in negative types"
        MBothType -> fail $ "\"|\" not allowed in negative types"
interpretTypeM (AndSyntaxType st1 st2) =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> fail $ "\"&\" not allowed in positive types"
        MNegativeType -> do
            t1 <- interpretTypeM st1
            t2 <- interpretTypeM st2
            return $ toMPolar (<>) t1 t2
        MBothType -> fail $ "\"&\" not allowed in positive types"
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
                 GroundPinaforeSingularType MaybePinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (EitherSyntaxType st1 st2) = do
    at1 <- interpretTypeM st1
    at2 <- interpretTypeM st2
    return $
        toMPolar
            (\(MkAnyW t1) (MkAnyW t2) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType EitherPinaforeGroundType $
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
                 GroundPinaforeSingularType PairPinaforeGroundType $
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
                 GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (SetSyntaxType st1) = do
    at1 <- interpretTypeRange st1
    return $
        toMPolar
            (\(MkAnyInKind t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
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
interpretTypeM (ListSyntaxType st1) = do
    at1 <- interpretTypeM st1
    return $
        toMPolar
            (\(MkAnyW t1) ->
                 MkAnyW $
                 singlePinaforeType $
                 GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
            at1
interpretTypeM (VarSyntaxType name) =
    toSymbolWitness (unpack name) $ \t -> return $ toMPolar $ MkAnyW $ singlePinaforeType $ VarPinaforeSingularType t
interpretTypeM UnitSyntaxType = return $ toMPolar $ MkAnyW $ literalPinaforeType UnitLiteralType
interpretTypeM (ConstSyntaxType name) = interpretTypeConst name
interpretTypeM (RangeSyntaxType _) = fail "range not allowed in type"

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
interpretTypeConst "Entity" =
    return $
    toMPolar $
    MkAnyW $
    singlePinaforeType $
    GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments
interpretTypeConst "NewEntity" =
    return $
    toMPolar $
    MkAnyW $
    singlePinaforeType $
    GroundPinaforeSingularType (SimpleEntityPinaforeGroundType NewSimpleEntityType) NilDolanArguments
interpretTypeConst n
    | Just (MkAnyW lt) <- nameToLiteralType n = return $ toMPolar $ MkAnyW $ literalPinaforeType lt
interpretTypeConst n = do
    nt <- lookupNamedType n
    case nt of
        EntityNamedType (MkAnyW sw) ->
            return $
            toMPolar $
            MkAnyW $
            singlePinaforeType $
            GroundPinaforeSingularType (SimpleEntityPinaforeGroundType $ NamedSimpleEntityType sw) NilDolanArguments

nameToLiteralType :: Name -> Maybe (AnyW LiteralType)
nameToLiteralType "Literal" = Just $ MkAnyW LiteralLiteralType
nameToLiteralType "Text" = Just $ MkAnyW TextLiteralType
nameToLiteralType "Number" = Just $ MkAnyW NumberLiteralType
nameToLiteralType "Boolean" = Just $ MkAnyW BooleanLiteralType
nameToLiteralType _ = Nothing
