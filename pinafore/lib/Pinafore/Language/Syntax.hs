module Pinafore.Language.Syntax where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Shapes

data SyntaxClosedEntityConstructor =
    MkSyntaxClosedEntityConstructor Name
                                    [SyntaxType]
                                    Anchor

data SyntaxDatatypeConstructor =
    MkSyntaxDatatypeConstructor Name
                                [SyntaxType]

data SyntaxTypeDeclaration
    = ClosedEntitySyntaxTypeDeclaration [SyntaxClosedEntityConstructor]
    | DatatypeSyntaxTypeDeclaration [SyntaxDatatypeConstructor]
    | OpenEntitySyntaxTypeDeclaration

data SyntaxDeclaration
    = TypeSyntaxDeclaration SourcePos
                            Name
                            SyntaxTypeDeclaration
    | SubtypeDeclaration SourcePos
                         SyntaxType
                         SyntaxType
    | BindingSyntaxDeclaration SyntaxBinding

data WithSourcePos t =
    MkWithSourcePos SourcePos
                    t

data SyntaxVariance
    = CoSyntaxVariance
    | ContraSyntaxVariance

data SyntaxGroundType
    = ConstSyntaxGroundType Name
    | FunctionSyntaxGroundType
    | MorphismSyntaxGroundType
    | ListSyntaxGroundType
    | PairSyntaxGroundType
    | UnitSyntaxGroundType

data SyntaxTypeArgument
    = SimpleSyntaxTypeArgument SyntaxType
    | RangeSyntaxTypeArgument [(Maybe SyntaxVariance, SyntaxType)]

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

type SyntaxType = WithSourcePos SyntaxType'

data SyntaxBinding =
    MkSyntaxBinding SourcePos
                    (Maybe SyntaxType)
                    Name
                    SyntaxExpression

data SyntaxConstructor
    = SLNumber Number
    | SLString Text
    | SLNamedConstructor Name
    | SLPair
    | SLUnit

data SyntaxPattern'
    = AnySyntaxPattern
    | VarSyntaxPattern Name
    | BothSyntaxPattern SyntaxPattern
                        SyntaxPattern
    | ConstructorSyntaxPattern SyntaxConstructor
                               [SyntaxPattern]

type SyntaxPattern = WithSourcePos SyntaxPattern'

data SyntaxCase =
    MkSyntaxCase SyntaxPattern
                 SyntaxExpression

data SyntaxSpecialForm
    = SSFProperty SyntaxType
                  SyntaxType
                  Anchor
    | SSFOpenEntity SyntaxType
                    Anchor
    | SSFNewOpenEntity SyntaxType
    | SSFEvaluate SyntaxType

data SyntaxConstant
    = SCIfThenElse
    | SCBind
    | SCBind_
    | SCConstructor SyntaxConstructor
    | SCSpecialForm SyntaxSpecialForm

data SyntaxExpression'
    = SEConst SyntaxConstant
    | SEVar Name
    | SEApply SyntaxExpression
              SyntaxExpression
    | SEAbstract SyntaxPattern
                 SyntaxExpression
    | SERef SyntaxExpression
    | SEUnref SyntaxExpression
    | SELet [SyntaxDeclaration]
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

data SyntaxTopDeclarations =
    MkSyntaxTopDeclarations SourcePos
                            [SyntaxDeclaration]

class HasSourcePos t where
    getSourcePos :: t -> SourcePos

instance HasSourcePos (WithSourcePos t) where
    getSourcePos (MkWithSourcePos spos _) = spos

instance HasSourcePos SyntaxBinding where
    getSourcePos (MkSyntaxBinding spos _ _ _) = spos

instance HasSourcePos SyntaxTopDeclarations where
    getSourcePos (MkSyntaxTopDeclarations spos _) = spos

instance HasSourcePos SyntaxDeclaration where
    getSourcePos (BindingSyntaxDeclaration bind) = getSourcePos bind
    getSourcePos (TypeSyntaxDeclaration spos _ _) = spos
    getSourcePos (SubtypeDeclaration spos _ _) = spos

class SyntaxFreeVariables t where
    syntaxFreeVariables :: t -> FiniteSet Name

instance SyntaxFreeVariables t => SyntaxFreeVariables [t] where
    syntaxFreeVariables tt = mconcat $ fmap syntaxFreeVariables tt

instance SyntaxFreeVariables st => SyntaxFreeVariables (WithSourcePos st) where
    syntaxFreeVariables (MkWithSourcePos _ e) = syntaxFreeVariables e

instance SyntaxFreeVariables SyntaxCase where
    syntaxFreeVariables (MkSyntaxCase pat expr) = difference (syntaxFreeVariables expr) (syntaxBindingVariables pat)

instance SyntaxFreeVariables SyntaxExpression' where
    syntaxFreeVariables (SEConst _) = mempty
    syntaxFreeVariables (SEVar name) = opoint name
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

instance SyntaxFreeVariables SyntaxDeclaration where
    syntaxFreeVariables (BindingSyntaxDeclaration bind) = syntaxFreeVariables bind
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

instance SyntaxBindingVariables SyntaxDeclaration where
    syntaxBindingVariables (BindingSyntaxDeclaration bind) = syntaxBindingVariables bind
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
