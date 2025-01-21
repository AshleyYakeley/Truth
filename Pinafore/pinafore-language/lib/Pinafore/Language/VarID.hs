module Pinafore.Language.VarID
    ( VarID (..)
    , VarIDState
    , varIDName
    , mkLambdaVarID
    , mkBadVarID
    , mkAppNotationVarID
    , mkPolymorphicVarID
    , shouldMergeVarID
    )
where

import Import

newtype VarIDState
    = MkVarIDState Int
    deriving newtype Sequential

data VarID
    = LambdaVarID
        Int
        FullName
    | PolymorphicVarID
        Int
        FullName
    | AppNotationVarID Int
    | ImplicitVarID ImplicitName
    | BadVarID
        SourcePos
        FullNameRef

-- used fot Eq and Ord
data VarVal
    = LambdaVarVal Int
    | PolymorphicVarVal Int
    | AppNotationVarVal Int
    | ImplicitVarVal ImplicitName
    | BadVarVal FullNameRef
    deriving stock (Eq, Ord)

toVarVal :: VarID -> VarVal
toVarVal =
    \case
        LambdaVarID s _ -> LambdaVarVal s
        PolymorphicVarID s _ -> PolymorphicVarVal s
        AppNotationVarID s -> AppNotationVarVal s
        ImplicitVarID n -> ImplicitVarVal n
        BadVarID _ n -> BadVarVal n

instance Eq VarID where
    a == b = toVarVal a == toVarVal b

instance Ord VarID where
    compare a b = compare (toVarVal a) (toVarVal b)

instance ExprShow VarID where
    exprShowPrec (LambdaVarID _ n) = exprShowPrec n
    exprShowPrec (PolymorphicVarID _ n) = exprShowPrec n
    exprShowPrec (AppNotationVarID i) = identifierPrecNamedText $ "%appn-" <> showText i
    exprShowPrec (ImplicitVarID n) = exprShowPrec n
    exprShowPrec (BadVarID _ n) = exprShowPrec n

instance Show VarID where
    show = exprShowShow

varIDName :: VarID -> Either FullNameRef ImplicitName
varIDName =
    \case
        LambdaVarID _ fn -> Left $ fullNameRef fn
        PolymorphicVarID _ fn -> Left $ fullNameRef fn
        AppNotationVarID i -> Left $ UnqualifiedFullNameRef $ MkName $ "%appn-" <> showText i
        ImplicitVarID n -> Right n
        BadVarID _ fnr -> Left fnr

mkLambdaVarID :: VarIDState -> Maybe FullName -> (VarID, FullName)
mkLambdaVarID (MkVarIDState s) mname = let
    name :: FullName
    name = fromMaybe (fromString $ "%var-" <> show s) mname
    in (LambdaVarID s name, name)

mkAppNotationVarID :: VarIDState -> VarID
mkAppNotationVarID (MkVarIDState s) = AppNotationVarID s

mkPolymorphicVarID :: VarIDState -> FullName -> VarID
mkPolymorphicVarID (MkVarIDState s) = PolymorphicVarID s

-- We could just throw an exception here, but this way we get to see the type of the missing variable.
mkBadVarID :: SourcePos -> FullNameRef -> VarID
mkBadVarID = BadVarID

shouldMergeVarID :: VarID -> VarID -> Bool
shouldMergeVarID (LambdaVarID s1 _) (LambdaVarID s2 _) = s1 == s2
shouldMergeVarID (AppNotationVarID s1) (AppNotationVarID s2) = s1 == s2
shouldMergeVarID (BadVarID _ n1) (BadVarID _ n2) = n1 == n2
shouldMergeVarID _ _ = False
