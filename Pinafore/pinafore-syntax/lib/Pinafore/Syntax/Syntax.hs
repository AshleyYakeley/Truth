module Pinafore.Syntax.Syntax where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Syntax.Name
import Pinafore.Syntax.Text
import Shapes
import Text.Parsec (SourcePos)

data WithSourcePos t =
    MkWithSourcePos SourcePos
                    t
    deriving (Eq)

getSourcePos :: WithSourcePos t -> SourcePos
getSourcePos (MkWithSourcePos spos _) = spos

instance ExprShow t => ExprShow (WithSourcePos t) where
    exprShowPrec (MkWithSourcePos _ expr) = exprShowPrec expr

data SyntaxWithDoc t =
    MkSyntaxWithDoc RawMarkdown
                    t
    deriving (Eq)

data SyntaxDataConstructor extra
    = PlainSyntaxConstructor [SyntaxType]
                             extra
    | RecordSyntaxConstructor [SyntaxSignature]
    deriving (Eq)

data SyntaxConstructorOrSubtype extra
    = ConstructorSyntaxConstructorOrSubtype FullName
                                            (SyntaxDataConstructor extra)
    | SubtypeSyntaxConstructorOrSubtype FullName
                                        [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    deriving (Eq)

type SyntaxStorableDatatypeConstructorOrSubtype = SyntaxConstructorOrSubtype Anchor

data SyntaxSignature'
    = SupertypeConstructorSyntaxSignature FullNameRef
    | ValueSyntaxSignature Name
                           SyntaxType
                           (Maybe SyntaxExpression)
    deriving (Eq)

type SyntaxSignature = SyntaxWithDoc (WithSourcePos SyntaxSignature')

data SyntaxTypeParameter
    = PositiveSyntaxTypeParameter Name
    | NegativeSyntaxTypeParameter Name
    | RangeSyntaxTypeParameter Name
                               Name -- negative, positive
    | DoubleRangeSyntaxTypeParameter Name
    deriving (Eq)

instance ExprShow SyntaxTypeParameter where
    exprShowPrec (PositiveSyntaxTypeParameter v) = namedTextPrec 0 $ "+" <> exprShow v
    exprShowPrec (NegativeSyntaxTypeParameter v) = namedTextPrec 0 $ "-" <> exprShow v
    exprShowPrec (RangeSyntaxTypeParameter vn vp) = namedTextPrec 0 $ "{-" <> exprShow vn <> ",+" <> exprShow vp <> "}"
    exprShowPrec (DoubleRangeSyntaxTypeParameter v) = namedTextPrec 0 $ exprShow v

type SyntaxPlainDatatypeConstructorOrSubtype = SyntaxConstructorOrSubtype ()

data SyntaxRecursiveTypeDeclaration
    = StorableDatatypeSyntaxRecursiveTypeDeclaration [SyntaxTypeParameter]
                                                     [SyntaxWithDoc SyntaxStorableDatatypeConstructorOrSubtype]
    | PlainDatatypeSyntaxRecursiveTypeDeclaration [SyntaxTypeParameter]
                                                  (Maybe SyntaxType)
                                                  [SyntaxWithDoc SyntaxPlainDatatypeConstructorOrSubtype]
    | OpenEntitySyntaxRecursiveTypeDeclaration
    deriving (Eq)

data SyntaxNonrecursiveTypeDeclaration =
    PredicateSyntaxNonrecursiveTypeDeclaration Bool
                                               SyntaxType
                                               SyntaxExpression
    deriving (Eq)

data SyntaxRecursiveDeclaration'
    = TypeSyntaxDeclaration FullName
                            SyntaxRecursiveTypeDeclaration
    | SubtypeSyntaxDeclaration TrustOrVerify
                               SyntaxType
                               SyntaxType
                               (Maybe SyntaxExpression)
    | BindingSyntaxDeclaration SyntaxBinding
    deriving (Eq)

type SyntaxRecursiveDeclaration = SyntaxWithDoc (WithSourcePos SyntaxRecursiveDeclaration')

data SyntaxNameRefItem
    = NameSyntaxNameRefItem FullNameRef
    | NamespaceSyntaxNameRefItem NamespaceRef
    deriving (Eq)

data SyntaxDeclaration'
    = DirectSyntaxDeclaration SyntaxRecursiveDeclaration'
    | NonrecursiveTypeSyntaxDeclaration FullName
                                        SyntaxNonrecursiveTypeDeclaration
    | RecordSyntaxDeclaration FullName
                              [SyntaxSignature]
                              (Maybe SyntaxType)
                              SyntaxExpression
    | DeclaratorSyntaxDeclaration SyntaxDeclarator
    | DeclaratorInSyntaxDeclaration SyntaxDeclarator
                                    SyntaxDeclaration
    | ExposeDeclaration [SyntaxNameRefItem]
    | NamespaceSyntaxDeclaration Bool
                                 Namespace
                                 [SyntaxDeclaration]
    | DocSectionSyntaxDeclaration Text
                                  [SyntaxDeclaration]
    | DebugSyntaxDeclaration FullNameRef
    deriving (Eq)

type SyntaxDeclaration = SyntaxWithDoc (WithSourcePos SyntaxDeclaration')

data SyntaxNamespaceWith =
    MkSyntaxNamespaceWith Namespace
                          (Maybe (Bool, [SyntaxNameRefItem]))
                          Namespace
    deriving (Eq)

data SyntaxDeclarator
    = SDLetSeq [SyntaxDeclaration]
    | SDLetRec [SyntaxRecursiveDeclaration]
    | SDImport [ModuleName]
    | SDWith [SyntaxNamespaceWith]
    deriving (Eq)

data SyntaxVariance
    = CoSyntaxVariance
    | ContraSyntaxVariance
    deriving (Eq)

instance ExprShow SyntaxVariance where
    exprShowPrec CoSyntaxVariance = "+"
    exprShowPrec ContraSyntaxVariance = "-"

newtype SyntaxGroundType =
    ConstSyntaxGroundType FullNameRef
    deriving (Eq)

data SyntaxTypeArgument =
    MkSyntaxTypeArgument [(Maybe SyntaxVariance, SyntaxType)]
    deriving (Eq)

pattern SimpleSyntaxTypeArgument ::
        SyntaxType -> SyntaxTypeArgument

pattern SimpleSyntaxTypeArgument t =
        MkSyntaxTypeArgument [(Nothing, t)]

instance ExprShow SyntaxTypeArgument where
    exprShowPrec (SimpleSyntaxTypeArgument t) = exprShowPrec t
    exprShowPrec (MkSyntaxTypeArgument args) = let
        showArg (mv, t) = exprShow mv <> exprShow t
        in namedTextPrec 0 $ "{" <> ointercalate "," (fmap showArg args) <> "}"

data SyntaxType'
    = SingleSyntaxType SyntaxGroundType
                       [SyntaxTypeArgument]
    | VarSyntaxType Name
    | OrSyntaxType SyntaxType
                   SyntaxType
    | AndSyntaxType SyntaxType
                    SyntaxType
    | TopSyntaxType
    | BottomSyntaxType
    | RecursiveSyntaxType Name
                          SyntaxType
    deriving (Eq)

data FixAssoc
    = AssocNone
    | AssocLeft
    | AssocRight
    deriving (Eq)

data Fixity = MkFixity
    { fixityAssoc :: FixAssoc
    , fixityPrec :: Int
    } deriving (Eq)

typeOperatorFixity :: Name -> Fixity
typeOperatorFixity "->" = MkFixity AssocRight 0
typeOperatorFixity "+:" = MkFixity AssocRight 2
typeOperatorFixity "*:" = MkFixity AssocRight 3
typeOperatorFixity _ = MkFixity AssocLeft 3

instance ExprShow SyntaxType' where
    exprShowPrec (VarSyntaxType v) = exprShowPrec v
    exprShowPrec (OrSyntaxType ta tb) = namedTextPrec 7 $ exprPrecShow 6 ta <> " | " <> exprPrecShow 6 tb
    exprShowPrec (AndSyntaxType ta tb) = namedTextPrec 7 $ exprPrecShow 6 ta <> " & " <> exprPrecShow 6 tb
    exprShowPrec TopSyntaxType = "None"
    exprShowPrec BottomSyntaxType = "Any"
    exprShowPrec (RecursiveSyntaxType n pt) = namedTextPrec 7 $ "rec " <> exprShow n <> ". " <> exprPrecShow 7 pt
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType (MkFullNameRef n _)) [ta, tb])
        | nameIsInfix n = let
            MkFixity assc level = typeOperatorFixity n
            prec = 6 - level
            in namedTextPrec prec $
               case assc of
                   AssocRight -> exprPrecShow (pred prec) ta <> " " <> exprShow n <> " " <> exprPrecShow prec tb
                   AssocLeft -> exprPrecShow prec ta <> " " <> exprShow n <> " " <> exprPrecShow (pred prec) tb
                   AssocNone -> exprPrecShow (pred prec) ta <> " " <> exprShow n <> " " <> exprPrecShow (pred prec) tb
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType n) []) = namedTextPrec 0 $ exprShow n
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType n) args) =
        namedTextPrec 2 $ exprShow n <> concatmap (\arg -> " " <> exprPrecShow 0 arg) args

type SyntaxType = WithSourcePos SyntaxType'

data SyntaxBinding =
    MkSyntaxBinding SyntaxPattern
                    SyntaxExpression
    deriving (Eq)

data SyntaxConstructor
    = SLNumber Number
    | SLString Text
    | SLNamedConstructor FullNameRef
                         (Maybe [(Name, SyntaxExpression)])
    | SLPair
    | SLUnit
    deriving (Eq)

instance ExprShow SyntaxConstructor where
    exprShowPrec (SLNumber x) = identifierPrecNamedText $ pack $ show x
    exprShowPrec (SLString x) = identifierPrecNamedText x
    exprShowPrec (SLNamedConstructor x mv) =
        namedTextPrec 7 $
        exprPrecShow 6 x <>
        case mv of
            Nothing -> ""
            Just vv -> " of " <> intercalate ";" (fmap (\(n, _) -> exprPrecShow 6 n <> "=<expr>") vv) <> " end"
    exprShowPrec SLPair = "(,)"
    exprShowPrec SLUnit = "()"

data SyntaxPattern'
    = AnySyntaxPattern
    | VarSyntaxPattern FullName
    | BothSyntaxPattern SyntaxPattern
                        SyntaxPattern
    | ConstructorSyntaxPattern Namespace
                               SyntaxConstructor
                               [SyntaxPattern]
    | TypedSyntaxPattern SyntaxPattern
                         SyntaxType
    | DynamicTypedSyntaxPattern SyntaxPattern
                                SyntaxType
    | NamespaceSyntaxPattern SyntaxPattern
                             NamespaceRef
    | DebugSyntaxPattern Text
                         SyntaxPattern
    deriving (Eq)

instance ExprShow SyntaxPattern' where
    exprShowPrec AnySyntaxPattern = "_"
    exprShowPrec (VarSyntaxPattern v) = exprShowPrec v
    exprShowPrec (BothSyntaxPattern a b) = namedTextPrec 6 $ exprPrecShow 5 a <> "@" <> exprPrecShow 5 b
    exprShowPrec (ConstructorSyntaxPattern ns c pp) =
        namedTextPrec 7 $ exprPrecShow 6 ns <> " " <> exprPrecShow 6 c <> concatmap (\p -> " " <> exprPrecShow 6 p) pp
    exprShowPrec (TypedSyntaxPattern p t) = namedTextPrec 7 $ exprPrecShow 6 p <> ": " <> exprPrecShow 6 t
    exprShowPrec (DynamicTypedSyntaxPattern p t) = namedTextPrec 7 $ exprPrecShow 6 p <> ":? " <> exprPrecShow 6 t
    exprShowPrec (NamespaceSyntaxPattern p n) = namedTextPrec 7 $ exprPrecShow 6 p <> " as " <> exprPrecShow 6 n
    exprShowPrec (DebugSyntaxPattern t p) = namedTextPrec 7 $ "debug " <> fromString (show t) <> " " <> exprPrecShow 6 p

type SyntaxPattern = WithSourcePos SyntaxPattern'

data SyntaxCase =
    MkSyntaxCase SyntaxPattern
                 SyntaxExpression
    deriving (Eq)

data SyntaxMulticase (n :: PeanoNat) =
    MkSyntaxMulticase (FixedList n SyntaxPattern)
                      SyntaxExpression

syntaxMulticaseLength :: SyntaxMulticase n -> PeanoNatType n
syntaxMulticaseLength (MkSyntaxMulticase l _) = fixedListLength l

instance TestEquality SyntaxMulticase where
    testEquality (MkSyntaxMulticase patsa expa) (MkSyntaxMulticase patsb expb) = do
        Refl <- testEquality (fixedListLength patsa) (fixedListLength patsb)
        if expa == expb
            then Just Refl
            else Nothing

instance Eq (SyntaxMulticase n) where
    a == b = isJust $ testEquality a b

data SyntaxAnnotation
    = SAType SyntaxType
    | SAAnchor Anchor
    deriving (Eq)

data SyntaxConstant
    = SCIfThenElse
    | SCConstructor SyntaxConstructor
    deriving (Eq)

data SyntaxMulticaseList =
    forall (n :: PeanoNat). MkSyntaxMulticaseList (PeanoNatType n)
                                                  [SyntaxMulticase n]

instance Eq SyntaxMulticaseList where
    MkSyntaxMulticaseList na aa == MkSyntaxMulticaseList nb bb =
        fromMaybe False $ do
            Refl <- testEquality na nb
            return $ aa == bb

data SyntaxExpression'
    = SESubsume SyntaxExpression
                SyntaxType
    | SEConst SyntaxConstant
    | SEVar Namespace
            FullNameRef
            (Maybe [(Name, SyntaxExpression)])
    | SEImplicitVar ImplicitName
    | SESpecialForm FullNameRef
                    (NonEmpty SyntaxAnnotation)
    | SEApply SyntaxExpression
              SyntaxExpression
    | SEAbstract SyntaxCase
    | SEAbstracts (Some SyntaxMulticase)
    | SEMatch [SyntaxCase]
    | SEMatches SyntaxMulticaseList
    | SEAppQuote SyntaxExpression
    | SEAppUnquote SyntaxExpression
    | SEImply [(ImplicitName, Maybe SyntaxType, SyntaxExpression)]
              SyntaxExpression
    | SEDecl SyntaxDeclarator
             SyntaxExpression
    | SEList [SyntaxExpression]
    | SEDebug Text
              SyntaxExpression
    deriving (Eq)

seConst :: SourcePos -> SyntaxConstant -> SyntaxExpression
seConst spos sc = MkWithSourcePos spos $ SEConst sc

seAbstract :: SourcePos -> SyntaxPattern -> SyntaxExpression -> SyntaxExpression
seAbstract spos pat expr = MkWithSourcePos spos $ SEAbstract $ MkSyntaxCase pat expr

seApply :: SourcePos -> SyntaxExpression -> SyntaxExpression -> SyntaxExpression
seApply spos f a = MkWithSourcePos spos $ SEApply f a

seApplys :: SourcePos -> SyntaxExpression -> [SyntaxExpression] -> SyntaxExpression
seApplys _ f [] = f
seApplys spos f (a:aa) = seApplys spos (seApply spos f a) aa

type SyntaxExpression = WithSourcePos SyntaxExpression'

data SyntaxModule =
    MkSyntaxModule [SyntaxDeclaration]
