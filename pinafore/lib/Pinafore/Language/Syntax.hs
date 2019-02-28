module Pinafore.Language.Syntax where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Type
import Shapes

newtype TypeDecls baseedit =
    MkTypeDecls (forall a. RefNotation baseedit a -> RefNotation baseedit a)

instance Semigroup (TypeDecls baseedit) where
    (MkTypeDecls a) <> (MkTypeDecls b) = MkTypeDecls (a . b)

instance Monoid (TypeDecls baseedit) where
    mempty = MkTypeDecls id
    mappend = (<>)

data SyntaxDeclarations baseedit =
    MkSyntaxDeclarations (PinaforeScoped baseedit (TypeDecls baseedit))
                         [SyntaxBinding baseedit]

instance Semigroup (SyntaxDeclarations baseedit) where
    (MkSyntaxDeclarations ta ba) <> (MkSyntaxDeclarations tb bb) = MkSyntaxDeclarations (ta <> tb) (ba <> bb)

instance Monoid (SyntaxDeclarations baseedit) where
    mempty = MkSyntaxDeclarations mempty mempty
    mappend = (<>)

typeSyntaxDeclarations :: PinaforeScoped baseedit (TypeDecls baseedit) -> SyntaxDeclarations baseedit
typeSyntaxDeclarations td = MkSyntaxDeclarations td mempty

data SyntaxVariance
    = CoSyntaxVariance
    | ContraSyntaxVariance

data SyntaxType
    = ConstSyntaxType Name
    | VarSyntaxType Name
    | UnitSyntaxType
    | ActionSyntaxType SyntaxType
    | OrderSyntaxType SyntaxType
    | RefSyntaxType SyntaxType
    | UISyntaxType SyntaxType
    | SetSyntaxType SyntaxType
    | ListSyntaxType SyntaxType
    | MorphismSyntaxType SyntaxType
                         SyntaxType
    | FunctionSyntaxType SyntaxType
                         SyntaxType
    | MaybeSyntaxType SyntaxType
    | EitherSyntaxType SyntaxType
                       SyntaxType
    | PairSyntaxType SyntaxType
                     SyntaxType
    | RangeSyntaxType [(Maybe SyntaxVariance, SyntaxType)]
    | OrSyntaxType SyntaxType
                   SyntaxType
    | AndSyntaxType SyntaxType
                    SyntaxType
    | TopSyntaxType
    | BottomSyntaxType

data SyntaxBinding baseedit =
    MkSyntaxBinding SourcePos
                    (Maybe SyntaxType)
                    Name
                    (SyntaxExpression baseedit)

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

data SyntaxPattern =
    MkSyntaxPattern SourcePos
                    SyntaxPattern'

data SyntaxCase baseedit =
    MkSyntaxCase SyntaxPattern
                 (SyntaxExpression baseedit)

data SyntaxConstant
    = SCIfThenElse
    | SCBind
    | SCBind_
    | SCConstructor SyntaxConstructor

data SyntaxExpression' baseedit
    = SEConst SyntaxConstant
    | SEVar Name
    | SEApply (SyntaxExpression baseedit)
              (SyntaxExpression baseedit)
    | SEAbstract SyntaxPattern
                 (SyntaxExpression baseedit)
    | SERef (SyntaxExpression baseedit)
    | SEUnref (SyntaxExpression baseedit)
    | SELet (SyntaxDeclarations baseedit)
            (SyntaxExpression baseedit)
    | SECase (SyntaxExpression baseedit)
             [SyntaxCase baseedit]
    | SEList [SyntaxExpression baseedit]
    | SEProperty SyntaxType
                 SyntaxType
                 Anchor
    | SEEntity SyntaxType
               Anchor

seConst :: SourcePos -> SyntaxConstant -> SyntaxExpression baseedit
seConst spos sc = MkSyntaxExpression spos $ SEConst sc

seAbstract :: SourcePos -> SyntaxPattern -> SyntaxExpression baseedit -> SyntaxExpression baseedit
seAbstract spos pat expr = MkSyntaxExpression spos $ SEAbstract pat expr

seAbstracts :: SourcePos -> [SyntaxPattern] -> SyntaxExpression baseedit -> SyntaxExpression baseedit
seAbstracts _ [] expr = expr
seAbstracts spos (p:pp) expr = seAbstract spos p $ seAbstracts spos pp expr

seApply :: SourcePos -> SyntaxExpression baseedit -> SyntaxExpression baseedit -> SyntaxExpression baseedit
seApply spos f a = MkSyntaxExpression spos $ SEApply f a

seApplys :: SourcePos -> SyntaxExpression baseedit -> [SyntaxExpression baseedit] -> SyntaxExpression baseedit
seApplys _ f [] = f
seApplys spos f (a:aa) = seApplys spos (seApply spos f a) aa

data SyntaxExpression baseedit =
    MkSyntaxExpression SourcePos
                       (SyntaxExpression' baseedit)

data SyntaxTopDeclarations baseedit =
    MkSyntaxTopDeclarations SourcePos
                            (SyntaxDeclarations baseedit)

class HasSourcePos t where
    getSourcePos :: t -> SourcePos

instance HasSourcePos SyntaxPattern where
    getSourcePos (MkSyntaxPattern spos _) = spos

instance HasSourcePos (SyntaxBinding baseedit) where
    getSourcePos (MkSyntaxBinding spos _ _ _) = spos

instance HasSourcePos (SyntaxExpression baseedit) where
    getSourcePos (MkSyntaxExpression spos _) = spos

instance HasSourcePos (SyntaxTopDeclarations baseedit) where
    getSourcePos (MkSyntaxTopDeclarations spos _) = spos

class SyntaxFreeVariables t where
    syntaxFreeVariables :: t -> FiniteSet Name

instance SyntaxFreeVariables t => SyntaxFreeVariables [t] where
    syntaxFreeVariables tt = mconcat $ fmap syntaxFreeVariables tt

instance SyntaxFreeVariables (SyntaxExpression baseedit) where
    syntaxFreeVariables (MkSyntaxExpression _ e) = syntaxFreeVariables e

instance SyntaxFreeVariables (SyntaxCase baseedit) where
    syntaxFreeVariables (MkSyntaxCase pat expr) = difference (syntaxFreeVariables expr) (syntaxBindingVariables pat)

instance SyntaxFreeVariables (SyntaxExpression' baseedit) where
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
    syntaxFreeVariables (SEProperty _ _ _) = mempty
    syntaxFreeVariables (SEEntity _ _) = mempty

instance SyntaxFreeVariables (SyntaxBinding baseedit) where
    syntaxFreeVariables (MkSyntaxBinding _ _ _ expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables (SyntaxDeclarations baseedit) where
    syntaxFreeVariables (MkSyntaxDeclarations _ binds) = syntaxFreeVariables binds

class SyntaxBindingVariables t where
    syntaxBindingVariables :: t -> FiniteSet Name

instance SyntaxBindingVariables t => SyntaxBindingVariables [t] where
    syntaxBindingVariables tt = mconcat $ fmap syntaxBindingVariables tt

instance SyntaxBindingVariables SyntaxPattern where
    syntaxBindingVariables (MkSyntaxPattern _ pat) = syntaxBindingVariables pat

instance SyntaxBindingVariables SyntaxPattern' where
    syntaxBindingVariables AnySyntaxPattern = mempty
    syntaxBindingVariables (VarSyntaxPattern name) = singletonSet name
    syntaxBindingVariables (BothSyntaxPattern pat1 pat2) =
        union (syntaxBindingVariables pat1) (syntaxBindingVariables pat2)
    syntaxBindingVariables (ConstructorSyntaxPattern _ pats) = syntaxBindingVariables pats

instance SyntaxBindingVariables (SyntaxDeclarations baseedit) where
    syntaxBindingVariables (MkSyntaxDeclarations _ binds) = syntaxBindingVariables binds

instance SyntaxBindingVariables (SyntaxBinding baseedit) where
    syntaxBindingVariables (MkSyntaxBinding _ _ name _) = singletonSet name

checkSyntaxBindingsDuplicates :: MonadFail m => [SyntaxBinding baseedit] -> m ()
checkSyntaxBindingsDuplicates = let
    duplicates ::
           forall a. Eq a
        => [a]
        -> [a]
    duplicates [] = []
    duplicates (a:aa)
        | elem a aa = a : duplicates aa
    duplicates (_:aa) = duplicates aa
    checkDuplicates ::
           forall m name. (Show name, Eq name, MonadFail m)
        => [name]
        -> m ()
    checkDuplicates nn =
        case nub $ duplicates nn of
            [] -> return ()
            b -> fail $ "duplicate bindings: " <> (intercalate ", " $ fmap show b)
    in checkDuplicates . fmap (\(MkSyntaxBinding _ _ name _) -> name)
