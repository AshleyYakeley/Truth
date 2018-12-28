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
    | OrderSyntaxType SyntaxType
    | RefSyntaxType SyntaxType
    | SetSyntaxType SyntaxType
    | ListSyntaxType SyntaxType
    | MorphismSyntaxType SyntaxType
                         SyntaxType
    | FunctionSyntaxType SyntaxType
                         SyntaxType
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
                    [SyntaxPattern]
                    (SyntaxExpression baseedit)

data SyntaxPattern =
    MkSyntaxPattern Name

data SyntaxCase baseedit =
    MkSyntaxCase SyntaxPattern
                 (SyntaxExpression baseedit)

data SyntaxExpression' baseedit
    = SEConst (QValue baseedit)
    | SEVar Name
    | SEApply (SyntaxExpression baseedit)
              [SyntaxExpression baseedit]
    | SEAbstract [SyntaxPattern]
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

data SyntaxExpression baseedit =
    MkSyntaxExpression SourcePos
                       (SyntaxExpression' baseedit)

data SyntaxTopDeclarations baseedit =
    MkSyntaxTopDeclarations SourcePos
                            (SyntaxDeclarations baseedit)

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
    syntaxFreeVariables (SEApply f args) = union (syntaxFreeVariables f) (syntaxFreeVariables args)
    syntaxFreeVariables (SEAbstract pats expr) = difference (syntaxFreeVariables expr) (syntaxBindingVariables pats)
    syntaxFreeVariables (SERef expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SEUnref expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SELet binds expr) =
        difference (syntaxFreeVariables binds <> syntaxFreeVariables expr) (syntaxBindingVariables binds)
    syntaxFreeVariables (SECase expr cases) = union (syntaxFreeVariables expr) (syntaxFreeVariables cases)
    syntaxFreeVariables (SEList exprs) = syntaxFreeVariables exprs
    syntaxFreeVariables (SEProperty _ _ _) = mempty
    syntaxFreeVariables (SEEntity _ _) = mempty

instance SyntaxFreeVariables (SyntaxBinding baseedit) where
    syntaxFreeVariables (MkSyntaxBinding _ _ _ pats expr) =
        difference (syntaxFreeVariables expr) (syntaxBindingVariables pats)

instance SyntaxFreeVariables (SyntaxDeclarations baseedit) where
    syntaxFreeVariables (MkSyntaxDeclarations _ binds) = syntaxFreeVariables binds

class SyntaxBindingVariables t where
    syntaxBindingVariables :: t -> FiniteSet Name

instance SyntaxBindingVariables t => SyntaxBindingVariables [t] where
    syntaxBindingVariables tt = mconcat $ fmap syntaxBindingVariables tt

instance SyntaxBindingVariables SyntaxPattern where
    syntaxBindingVariables (MkSyntaxPattern name) = singletonSet name

instance SyntaxBindingVariables (SyntaxDeclarations baseedit) where
    syntaxBindingVariables (MkSyntaxDeclarations _ binds) = syntaxBindingVariables binds

instance SyntaxBindingVariables (SyntaxBinding baseedit) where
    syntaxBindingVariables (MkSyntaxBinding _ _ name _ _) = singletonSet name

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
    in checkDuplicates . fmap (\(MkSyntaxBinding _ _ name _ _) -> name)
