module Pinafore.Language.Grammar.Syntax where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes
import Text.Parsec (SourcePos)

data SyntaxClosedEntityConstructor =
    MkSyntaxClosedEntityConstructor Name
                                    [SyntaxType]
                                    Anchor

data SyntaxDatatypeParameter
    = PositiveSyntaxDatatypeParameter Name
    | NegativeSyntaxDatatypeParameter Name
    | RangeSyntaxDatatypeParameter Name
                                   Name -- negative, positive

data SyntaxDatatypeConstructor =
    MkSyntaxDatatypeConstructor Name
                                [SyntaxType]

data SyntaxDynamicEntityConstructor
    = AnchorSyntaxDynamicEntityConstructor Anchor
    | NameSyntaxDynamicEntityConstructor ReferenceName

data SyntaxTypeDeclaration
    = ClosedEntitySyntaxTypeDeclaration [SyntaxClosedEntityConstructor]
    | DatatypeSyntaxTypeDeclaration [SyntaxDatatypeParameter]
                                    [SyntaxDatatypeConstructor]
    | OpenEntitySyntaxTypeDeclaration
    | DynamicEntitySyntaxTypeDeclaration (NonEmpty SyntaxDynamicEntityConstructor)

data SyntaxExpose
    = SExpExpose SourcePos
                 [Name]
    | SExpLet [SyntaxWithDoc SyntaxDeclaration]
              SyntaxExpose

data SyntaxDirectDeclaration
    = TypeSyntaxDeclaration SourcePos
                            Name
                            SyntaxTypeDeclaration
    | SubtypeSyntaxDeclaration SourcePos
                               SyntaxType
                               SyntaxType
    | BindingSyntaxDeclaration SyntaxBinding

data SyntaxWithDoc t =
    MkSyntaxWithDoc Markdown
                    t

data SyntaxDeclaration
    = DirectSyntaxDeclaration SyntaxDirectDeclaration
    | ImportSyntaxDeclaration SourcePos
                              ModuleName
                              (Maybe [Name])
    | ExposeSyntaxDeclaration SourcePos
                              SyntaxExpose
    | RecursiveSyntaxDeclaration SourcePos
                                 [SyntaxWithDoc SyntaxDirectDeclaration]

data WithSourcePos t =
    MkWithSourcePos SourcePos
                    t

instance ExprShow t => ExprShow (WithSourcePos t) where
    exprShowPrec (MkWithSourcePos _ expr) = exprShowPrec expr

data SyntaxVariance
    = CoSyntaxVariance
    | ContraSyntaxVariance

instance ExprShow SyntaxVariance where
    exprShowPrec CoSyntaxVariance = ("+", 0)
    exprShowPrec ContraSyntaxVariance = ("-", 0)

data SyntaxGroundType
    = ConstSyntaxGroundType ReferenceName
    | FunctionSyntaxGroundType
    | MorphismSyntaxGroundType
    | ListSyntaxGroundType
    | PairSyntaxGroundType
    | UnitSyntaxGroundType

data SyntaxTypeArgument
    = SimpleSyntaxTypeArgument SyntaxType
    | RangeSyntaxTypeArgument [(Maybe SyntaxVariance, SyntaxType)]

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

instance ExprShow SyntaxType' where
    exprShowPrec (VarSyntaxType v) = exprShowPrec v
    exprShowPrec (OrSyntaxType ta tb) = (exprPrecShow 2 ta <> " | " <> exprPrecShow 2 tb, 3)
    exprShowPrec (AndSyntaxType ta tb) = (exprPrecShow 2 ta <> " & " <> exprPrecShow 2 tb, 3)
    exprShowPrec TopSyntaxType = ("None", 0)
    exprShowPrec BottomSyntaxType = ("Any", 0)
    exprShowPrec (RecursiveSyntaxType n pt) = ("rec " <> exprShow n <> ". " <> exprShow pt, 4)
    exprShowPrec (SingleSyntaxType UnitSyntaxGroundType []) = ("()", 0)
    exprShowPrec (SingleSyntaxType PairSyntaxGroundType [ta, tb]) = ("(" <> exprShow ta <> "," <> exprShow tb <> ")", 0)
    exprShowPrec (SingleSyntaxType ListSyntaxGroundType [ta]) = ("[" <> exprShow ta <> "]", 0)
    exprShowPrec (SingleSyntaxType FunctionSyntaxGroundType [ta, tb]) =
        (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
    exprShowPrec (SingleSyntaxType MorphismSyntaxGroundType [ta, tb]) =
        (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType n) []) = (exprShow n, 0)
    exprShowPrec (SingleSyntaxType (ConstSyntaxGroundType n) args) =
        (exprShow n <> mconcat (fmap (\arg -> " " <> exprPrecShow 0 arg) args), 2)
    exprShowPrec (SingleSyntaxType _ _) = ("UNKNOWN", 0)

type SyntaxType = WithSourcePos SyntaxType'

data SyntaxBinding =
    MkSyntaxBinding SourcePos
                    (Maybe SyntaxType)
                    Name
                    SyntaxExpression

data SyntaxConstructor
    = SLNumber Number
    | SLString Text
    | SLNamedConstructor ReferenceName
    | SLPair
    | SLUnit

data SyntaxPattern'
    = AnySyntaxPattern
    | VarSyntaxPattern Name
    | BothSyntaxPattern SyntaxPattern
                        SyntaxPattern
    | ConstructorSyntaxPattern SyntaxConstructor
                               [SyntaxPattern]
    | TypedSyntaxPattern SyntaxPattern
                         SyntaxType

type SyntaxPattern = WithSourcePos SyntaxPattern'

data SyntaxCase =
    MkSyntaxCase SyntaxPattern
                 SyntaxExpression

data SyntaxAnnotation
    = SAType SyntaxType
    | SAAnchor Anchor

data SyntaxConstant
    = SCIfThenElse
    | SCBind
    | SCBind_
    | SCConstructor SyntaxConstructor

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
    | SEList [SyntaxExpression]

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

instance HasSourcePos SyntaxDirectDeclaration where
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
    syntaxFreeVariables (SEList exprs) = syntaxFreeVariables exprs

instance SyntaxFreeVariables SyntaxBinding where
    syntaxFreeVariables (MkSyntaxBinding _ _ _ expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables t => SyntaxFreeVariables (SyntaxWithDoc t) where
    syntaxFreeVariables (MkSyntaxWithDoc _ decl) = syntaxFreeVariables decl

instance SyntaxFreeVariables SyntaxDirectDeclaration where
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

instance SyntaxBindingVariables SyntaxDirectDeclaration where
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
    duplicates ::
           forall a. Eq a
        => [a]
        -> [a]
    duplicates [] = []
    duplicates (a:aa)
        | elem a aa = a : duplicates aa
    duplicates (_:aa) = duplicates aa
    checkDuplicates :: [Name] -> m ()
    checkDuplicates nn =
        case nub $ duplicates nn of
            [] -> return ()
            b -> throw $ InterpretBindingsDuplicateError b
    in checkDuplicates . fmap (\(MkSyntaxBinding _ _ name _) -> name)
