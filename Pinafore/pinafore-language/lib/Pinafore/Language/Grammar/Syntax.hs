module Pinafore.Language.Grammar.Syntax where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes
import Text.Parsec (SourcePos)

data SyntaxConstructorOrSubtype extra
    = ConstructorSyntaxConstructorOrSubtype Name
                                            [SyntaxType]
                                            extra
    | SubtypeSyntaxConstructorOrSubtype Name
                                        [SyntaxConstructorOrSubtype extra]
    deriving (Eq)

type SyntaxClosedEntityConstructorOrSubtype = SyntaxConstructorOrSubtype Anchor

data SyntaxTypeParameter
    = PositiveSyntaxTypeParameter Name
    | NegativeSyntaxTypeParameter Name
    | RangeSyntaxTypeParameter Name
                               Name -- negative, positive
    deriving (Eq)

instance ExprShow SyntaxTypeParameter where
    exprShowPrec (PositiveSyntaxTypeParameter v) = ("+" <> exprShow v, 0)
    exprShowPrec (NegativeSyntaxTypeParameter v) = ("-" <> exprShow v, 0)
    exprShowPrec (RangeSyntaxTypeParameter vn vp) = ("{-" <> exprShow vn <> ",+" <> exprShow vp <> "}", 0)

type SyntaxDatatypeConstructorOrSubtype = SyntaxConstructorOrSubtype ()

data SyntaxDynamicEntityConstructor
    = AnchorSyntaxDynamicEntityConstructor Anchor
    | NameSyntaxDynamicEntityConstructor ReferenceName
    deriving (Eq)

data SyntaxTypeDeclaration
    = ClosedEntitySyntaxTypeDeclaration [SyntaxTypeParameter]
                                        [SyntaxClosedEntityConstructorOrSubtype]
    | DatatypeSyntaxTypeDeclaration [SyntaxTypeParameter]
                                    [SyntaxDatatypeConstructorOrSubtype]
    | OpenEntitySyntaxTypeDeclaration
    | DynamicEntitySyntaxTypeDeclaration (NonEmpty SyntaxDynamicEntityConstructor)
    deriving (Eq)

data SyntaxExpose
    = SExpExpose SourcePos
                 [Name]
    | SExpLet [SyntaxWithDoc SyntaxDeclaration]
              SyntaxExpose
    deriving (Eq)

data SyntaxRecursiveDeclaration
    = TypeSyntaxDeclaration SourcePos
                            Name
                            SyntaxTypeDeclaration
    | SubtypeSyntaxDeclaration SourcePos
                               SyntaxType
                               SyntaxType
    | BindingSyntaxDeclaration SyntaxBinding
    deriving (Eq)

data SyntaxWithDoc t =
    MkSyntaxWithDoc Markdown
                    t
    deriving (Eq)

data SyntaxDeclaration
    = DirectSyntaxDeclaration SyntaxRecursiveDeclaration
    | ImportSyntaxDeclaration SourcePos
                              ModuleName
                              (Maybe [Name])
    | ExposeSyntaxDeclaration SourcePos
                              SyntaxExpose
    | RecursiveSyntaxDeclaration SourcePos
                                 [SyntaxWithDoc SyntaxRecursiveDeclaration]
    deriving (Eq)

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
    exprShowPrec CoSyntaxVariance = ("+", 0)
    exprShowPrec ContraSyntaxVariance = ("-", 0)

newtype SyntaxGroundType =
    ConstSyntaxGroundType ReferenceName
    deriving (Eq)

data SyntaxTypeArgument
    = SimpleSyntaxTypeArgument SyntaxType
    | RangeSyntaxTypeArgument [(Maybe SyntaxVariance, SyntaxType)]
    deriving (Eq)

instance ExprShow SyntaxTypeArgument where
    exprShowPrec (SimpleSyntaxTypeArgument t) = exprShowPrec t
    exprShowPrec (RangeSyntaxTypeArgument args) = let
        showArg (mv, t) = exprShow mv <> exprShow t
        in ("{" <> intercalate "," (fmap showArg args) <> "}", 0)

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

typeOperatorFixity :: Name -> Fixity
typeOperatorFixity "->" = MkFixity AssocRight 0
typeOperatorFixity "~>" = MkFixity AssocRight 1
typeOperatorFixity ":+:" = MkFixity AssocRight 2
typeOperatorFixity ":*:" = MkFixity AssocRight 3
typeOperatorFixity _ = MkFixity AssocLeft 3

instance ExprShow SyntaxType' where
    exprShowPrec (VarSyntaxType v) = exprShowPrec v
    exprShowPrec (OrSyntaxType ta tb) = (exprPrecShow 6 ta <> " | " <> exprPrecShow 6 tb, 7)
    exprShowPrec (AndSyntaxType ta tb) = (exprPrecShow 6 ta <> " & " <> exprPrecShow 6 tb, 7)
    exprShowPrec TopSyntaxType = ("None", 0)
    exprShowPrec BottomSyntaxType = ("Any", 0)
    exprShowPrec (RecursiveSyntaxType n pt) = ("rec " <> exprShow n <> ". " <> exprPrecShow 7 pt, 7)
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType (UnqualifiedReferenceName n)) [ta, tb])
        | nameIsInfix n = let
            MkFixity assc level = typeOperatorFixity n
            prec = 6 - level
            in case assc of
                   AssocRight -> (exprPrecShow (pred prec) ta <> " " <> exprShow n <> " " <> exprPrecShow prec tb, prec)
                   AssocLeft -> (exprPrecShow prec ta <> " " <> exprShow n <> " " <> exprPrecShow (pred prec) tb, prec)
                   AssocNone ->
                       (exprPrecShow (pred prec) ta <> " " <> exprShow n <> " " <> exprPrecShow (pred prec) tb, prec)
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType n) []) = (exprShow n, 0)
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType n) args) =
        (exprShow n <> mconcat (fmap (\arg -> " " <> exprPrecShow 0 arg) args), 2)

type SyntaxType = WithSourcePos SyntaxType'

data SyntaxBinding =
    MkSyntaxBinding SourcePos
                    (Maybe SyntaxType)
                    Name
                    SyntaxExpression
    deriving (Eq)

data SyntaxConstructor
    = SLNumber Number
    | SLString Text
    | SLNamedConstructor ReferenceName
    | SLPair
    | SLUnit
    deriving (Eq)

data SyntaxPattern'
    = AnySyntaxPattern
    | VarSyntaxPattern Name
    | BothSyntaxPattern SyntaxPattern
                        SyntaxPattern
    | ConstructorSyntaxPattern SyntaxConstructor
                               [SyntaxPattern]
    | TypedSyntaxPattern SyntaxPattern
                         SyntaxType
    deriving (Eq)

type SyntaxPattern = WithSourcePos SyntaxPattern'

data SyntaxCase =
    MkSyntaxCase SyntaxPattern
                 SyntaxExpression
    deriving (Eq)

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

data SyntaxExpression'
    = SESubsume SyntaxExpression
                SyntaxType
    | SEConst SyntaxConstant
    | SEVar ReferenceName
    | SESpecialForm ReferenceName
                    (NonEmpty SyntaxAnnotation)
    | SEApply SyntaxExpression
              SyntaxExpression
    | SEAbstract SyntaxPattern
                 SyntaxExpression
    | SERef SyntaxExpression
    | SEUnref SyntaxExpression
    | SELet [SyntaxWithDoc SyntaxDeclaration]
            SyntaxExpression
    | SECase SyntaxExpression
             [SyntaxCase]
    | SELambdaCase [SyntaxCase]
    | SEList [SyntaxExpression]
    deriving (Eq)

seConst :: SourcePos -> SyntaxConstant -> SyntaxExpression
seConst spos sc = MkWithSourcePos spos $ SEConst sc

seAbstract :: SourcePos -> SyntaxPattern -> SyntaxExpression -> SyntaxExpression
seAbstract spos pat expr = MkWithSourcePos spos $ SEAbstract pat expr

seAbstracts :: SourcePos -> [SyntaxPattern] -> SyntaxExpression -> SyntaxExpression
seAbstracts _ [] expr = expr
seAbstracts spos (p:pp) expr = seAbstract spos p $ seAbstracts spos pp expr

seApply :: SourcePos -> SyntaxExpression -> SyntaxExpression -> SyntaxExpression
seApply spos f a = MkWithSourcePos spos $ SEApply f a

seApplys :: SourcePos -> SyntaxExpression -> [SyntaxExpression] -> SyntaxExpression
seApplys _ f [] = f
seApplys spos f (a:aa) = seApplys spos (seApply spos f a) aa

type SyntaxExpression = WithSourcePos SyntaxExpression'

type SyntaxModule = SyntaxExpose

data SyntaxTopDeclarations =
    MkSyntaxTopDeclarations SourcePos
                            [SyntaxWithDoc SyntaxDeclaration]

class HasSourcePos t where
    getSourcePos :: t -> SourcePos

instance HasSourcePos (WithSourcePos t) where
    getSourcePos (MkWithSourcePos spos _) = spos

instance HasSourcePos SyntaxBinding where
    getSourcePos (MkSyntaxBinding spos _ _ _) = spos

instance HasSourcePos SyntaxTopDeclarations where
    getSourcePos (MkSyntaxTopDeclarations spos _) = spos

instance HasSourcePos SyntaxRecursiveDeclaration where
    getSourcePos (BindingSyntaxDeclaration bind) = getSourcePos bind
    getSourcePos (TypeSyntaxDeclaration spos _ _) = spos
    getSourcePos (SubtypeSyntaxDeclaration spos _ _) = spos

instance HasSourcePos SyntaxDeclaration where
    getSourcePos (DirectSyntaxDeclaration decl) = getSourcePos decl
    getSourcePos (ImportSyntaxDeclaration spos _ _) = spos
    getSourcePos (ExposeSyntaxDeclaration spos _) = spos
    getSourcePos (RecursiveSyntaxDeclaration spos _) = spos

class SyntaxFreeVariables t where
    syntaxFreeVariables :: t -> FiniteSet Name

instance SyntaxFreeVariables t => SyntaxFreeVariables [t] where
    syntaxFreeVariables tt = mconcat $ fmap syntaxFreeVariables tt

instance SyntaxFreeVariables st => SyntaxFreeVariables (WithSourcePos st) where
    syntaxFreeVariables (MkWithSourcePos _ e) = syntaxFreeVariables e

instance SyntaxFreeVariables SyntaxCase where
    syntaxFreeVariables (MkSyntaxCase pat expr) = difference (syntaxFreeVariables expr) (syntaxBindingVariables pat)

instance SyntaxFreeVariables SyntaxExpression' where
    syntaxFreeVariables (SESubsume expr _) = syntaxFreeVariables expr
    syntaxFreeVariables (SEConst _) = mempty
    syntaxFreeVariables (SEVar (UnqualifiedReferenceName name)) = opoint name
    syntaxFreeVariables (SEVar (QualifiedReferenceName _ _)) = mempty
    syntaxFreeVariables (SESpecialForm _ _) = mempty
    syntaxFreeVariables (SEApply f arg) = union (syntaxFreeVariables f) (syntaxFreeVariables arg)
    syntaxFreeVariables (SEAbstract pat expr) = difference (syntaxFreeVariables expr) (syntaxBindingVariables pat)
    syntaxFreeVariables (SERef expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SEUnref expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SELet binds expr) =
        difference (syntaxFreeVariables binds <> syntaxFreeVariables expr) (syntaxBindingVariables binds)
    syntaxFreeVariables (SECase expr cases) = union (syntaxFreeVariables expr) (syntaxFreeVariables cases)
    syntaxFreeVariables (SELambdaCase cases) = syntaxFreeVariables cases
    syntaxFreeVariables (SEList exprs) = syntaxFreeVariables exprs

instance SyntaxFreeVariables SyntaxBinding where
    syntaxFreeVariables (MkSyntaxBinding _ _ _ expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables t => SyntaxFreeVariables (SyntaxWithDoc t) where
    syntaxFreeVariables (MkSyntaxWithDoc _ decl) = syntaxFreeVariables decl

instance SyntaxFreeVariables SyntaxRecursiveDeclaration where
    syntaxFreeVariables (BindingSyntaxDeclaration bind) = syntaxFreeVariables bind
    syntaxFreeVariables _ = mempty

instance SyntaxFreeVariables SyntaxDeclaration where
    syntaxFreeVariables (DirectSyntaxDeclaration bind) = syntaxFreeVariables bind
    syntaxFreeVariables (RecursiveSyntaxDeclaration _ decls) = syntaxFreeVariables decls
    syntaxFreeVariables _ = mempty

class SyntaxBindingVariables t where
    syntaxBindingVariables :: t -> FiniteSet Name

instance SyntaxBindingVariables t => SyntaxBindingVariables [t] where
    syntaxBindingVariables tt = mconcat $ fmap syntaxBindingVariables tt

instance SyntaxBindingVariables st => SyntaxBindingVariables (WithSourcePos st) where
    syntaxBindingVariables (MkWithSourcePos _ pat) = syntaxBindingVariables pat

instance SyntaxBindingVariables SyntaxPattern' where
    syntaxBindingVariables AnySyntaxPattern = mempty
    syntaxBindingVariables (VarSyntaxPattern name) = singletonSet name
    syntaxBindingVariables (BothSyntaxPattern pat1 pat2) =
        union (syntaxBindingVariables pat1) (syntaxBindingVariables pat2)
    syntaxBindingVariables (ConstructorSyntaxPattern _ pats) = syntaxBindingVariables pats
    syntaxBindingVariables (TypedSyntaxPattern pat _) = syntaxBindingVariables pat

instance SyntaxBindingVariables t => SyntaxBindingVariables (SyntaxWithDoc t) where
    syntaxBindingVariables (MkSyntaxWithDoc _ decl) = syntaxBindingVariables decl

instance SyntaxBindingVariables SyntaxRecursiveDeclaration where
    syntaxBindingVariables (BindingSyntaxDeclaration bind) = syntaxBindingVariables bind
    syntaxBindingVariables _ = mempty

instance SyntaxBindingVariables SyntaxDeclaration where
    syntaxBindingVariables (DirectSyntaxDeclaration bind) = syntaxBindingVariables bind
    syntaxBindingVariables (RecursiveSyntaxDeclaration _ decls) = syntaxBindingVariables decls
    syntaxBindingVariables _ = mempty

instance SyntaxBindingVariables SyntaxBinding where
    syntaxBindingVariables (MkSyntaxBinding _ _ name _) = singletonSet name

checkSyntaxBindingsDuplicates ::
       forall m. MonadThrow ErrorType m
    => [SyntaxBinding]
    -> m ()
checkSyntaxBindingsDuplicates = let
    checkDuplicates :: [Name] -> m ()
    checkDuplicates nn =
        case nonEmpty $ duplicates nn of
            Nothing -> return ()
            Just b -> throw $ InterpretBindingsDuplicateError b
    in checkDuplicates . fmap (\(MkSyntaxBinding _ _ name _) -> name)
