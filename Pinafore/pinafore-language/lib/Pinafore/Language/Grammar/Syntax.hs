module Pinafore.Language.Grammar.Syntax where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes
import Text.Parsec (SourcePos)

data SyntaxConstructorOrSubtype extra
    = ConstructorSyntaxConstructorOrSubtype FullName
                                            [SyntaxType]
                                            extra
    | SubtypeSyntaxConstructorOrSubtype FullName
                                        [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    | RecordSyntaxConstructorOrSubtype FullName
                                       [SyntaxSignature]
    deriving (Eq)

type SyntaxStorableDatatypeConstructorOrSubtype = SyntaxConstructorOrSubtype Anchor

data SyntaxSignature' =
    ValueSyntaxSignature Name
                         SyntaxType
    deriving (Eq)

type SyntaxSignature = SyntaxWithDoc (WithSourcePos SyntaxSignature')

data SyntaxTypeParameter
    = PositiveSyntaxTypeParameter Name
    | NegativeSyntaxTypeParameter Name
    | RangeSyntaxTypeParameter Name
                               Name -- negative, positive
    deriving (Eq)

instance ExprShow SyntaxTypeParameter where
    exprShowPrec (PositiveSyntaxTypeParameter v) = namedTextPrec 0 $ "+" <> exprShow v
    exprShowPrec (NegativeSyntaxTypeParameter v) = namedTextPrec 0 $ "-" <> exprShow v
    exprShowPrec (RangeSyntaxTypeParameter vn vp) = namedTextPrec 0 $ "{-" <> exprShow vn <> ",+" <> exprShow vp <> "}"

type SyntaxPlainDatatypeConstructorOrSubtype = SyntaxConstructorOrSubtype ()

data SyntaxDynamicEntityConstructor
    = AnchorSyntaxDynamicEntityConstructor Anchor
    | NameSyntaxDynamicEntityConstructor Namespace
                                         FullNameRef
    deriving (Eq)

data SyntaxTypeDeclaration
    = StorableDatatypeSyntaxTypeDeclaration [SyntaxTypeParameter]
                                            [SyntaxWithDoc SyntaxStorableDatatypeConstructorOrSubtype]
    | PlainDatatypeSyntaxTypeDeclaration [SyntaxTypeParameter]
                                         [SyntaxWithDoc SyntaxPlainDatatypeConstructorOrSubtype]
    | OpenEntitySyntaxTypeDeclaration
    | DynamicEntitySyntaxTypeDeclaration (NonEmpty SyntaxDynamicEntityConstructor)
    deriving (Eq)

data SyntaxRecursiveDeclaration'
    = TypeSyntaxDeclaration FullName
                            SyntaxTypeDeclaration
    | SubtypeSyntaxDeclaration TrustOrVerify
                               SyntaxType
                               SyntaxType
                               (Maybe SyntaxExpression)
    | BindingSyntaxDeclaration SyntaxBinding
    deriving (Eq)

type SyntaxRecursiveDeclaration = SyntaxWithDoc (WithSourcePos SyntaxRecursiveDeclaration')

data SyntaxWithDoc t =
    MkSyntaxWithDoc RawMarkdown
                    t
    deriving (Eq)

data SyntaxExposeItem
    = NameSyntaxExposeItem FullNameRef
    | NamespaceSyntaxExposeItem Namespace
    deriving (Eq)

data SyntaxExposeDeclaration =
    MkSyntaxExposeDeclaration [SyntaxExposeItem]
                              [SyntaxDeclaration]
    deriving (Eq)

data SyntaxDeclaration'
    = DirectSyntaxDeclaration SyntaxRecursiveDeclaration'
    | ImportSyntaxDeclaration ModuleName
    | ExposeSyntaxDeclaration SyntaxExposeDeclaration
    | RecursiveSyntaxDeclaration [SyntaxRecursiveDeclaration]
    | UsingSyntaxDeclaration Namespace
    | NamespaceSyntaxDeclaration Namespace
                                 [SyntaxDeclaration]
    | DebugSyntaxDeclaration FullNameRef
    deriving (Eq)

type SyntaxDeclaration = SyntaxWithDoc (WithSourcePos SyntaxDeclaration')

data WithSourcePos t =
    MkWithSourcePos SourcePos
                    t
    deriving (Eq)

instance ExprShow t => ExprShow (WithSourcePos t) where
    exprShowPrec (MkWithSourcePos _ expr) = exprShowPrec expr

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

data SyntaxTypeArgument
    = SimpleSyntaxTypeArgument SyntaxType
    | RangeSyntaxTypeArgument [(Maybe SyntaxVariance, SyntaxType)]
    deriving (Eq)

instance ExprShow SyntaxTypeArgument where
    exprShowPrec (SimpleSyntaxTypeArgument t) = exprShowPrec t
    exprShowPrec (RangeSyntaxTypeArgument args) = let
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
        namedTextPrec 2 $ exprShow n <> mconcat (fmap (\arg -> " " <> exprPrecShow 0 arg) args)

type SyntaxType = WithSourcePos SyntaxType'

data SyntaxBinding =
    MkSyntaxBinding SyntaxPattern
                    SyntaxExpression
    deriving (Eq)

data SyntaxConstructor
    = SLNumber Number
    | SLString Text
    | SLNamedConstructor FullNameRef
    | SLPair
    | SLUnit
    deriving (Eq)

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
    deriving (Eq)

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
    | SCBind
    | SCBind_
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
    | SESpecialForm FullNameRef
                    (NonEmpty SyntaxAnnotation)
    | SEApply SyntaxExpression
              SyntaxExpression
    | SEAbstract SyntaxCase
    | SEAbstracts (Some SyntaxMulticase)
    | SEMatch [SyntaxCase]
    | SEMatches SyntaxMulticaseList
    | SERef SyntaxExpression
    | SEUnref SyntaxExpression
    | SELet [SyntaxDeclaration]
            SyntaxExpression
    | SEList [SyntaxExpression]
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

type SyntaxModule = SyntaxExposeDeclaration

data SyntaxTopDeclarations =
    MkSyntaxTopDeclarations SourcePos
                            [SyntaxDeclaration]

class HasSourcePos t where
    getSourcePos :: t -> SourcePos

instance HasSourcePos (WithSourcePos t) where
    getSourcePos (MkWithSourcePos spos _) = spos

instance HasSourcePos SyntaxTopDeclarations where
    getSourcePos (MkSyntaxTopDeclarations spos _) = spos
