module Pinafore.Language.Grammar.FreeVars
    ( syntaxExpressionFreeVariables
    , syntaxPatternBindingVariables
    ) where

import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes

-- | Actually a superset, some of these might not be free.
syntaxExpressionFreeVariables :: SyntaxExpression -> [FullName]
syntaxExpressionFreeVariables expr = toList $ syntaxFreeVariables expr

data BindingThing
    = VarBindingThing FullName
    | ConsBindingThing Namespace
                       FullNameRef
    deriving (Eq)

btGetVar :: BindingThing -> Maybe FullName
btGetVar (VarBindingThing x) = Just x
btGetVar _ = Nothing

btGetCons :: BindingThing -> Maybe (Namespace, FullNameRef)
btGetCons (ConsBindingThing ns x) = Just (ns, x)
btGetCons _ = Nothing

syntaxPatternBindingVariables :: SyntaxPattern -> ([FullName], [(Namespace, FullNameRef)])
syntaxPatternBindingVariables pat = let
    bts = toList $ syntaxBindingVariables pat
    in (mapMaybe btGetVar bts, mapMaybe btGetCons bts)

class SyntaxFreeVariables t where
    syntaxFreeVariables :: t -> FiniteSet FullName

instance SyntaxFreeVariables t => SyntaxFreeVariables [t] where
    syntaxFreeVariables tt = mconcat $ fmap syntaxFreeVariables tt

instance SyntaxFreeVariables t => SyntaxFreeVariables (NonEmpty t) where
    syntaxFreeVariables tt = syntaxFreeVariables $ toList tt

instance SyntaxFreeVariables t => SyntaxFreeVariables (FixedList n t) where
    syntaxFreeVariables tt = syntaxFreeVariables $ toList tt

instance SyntaxFreeVariables st => SyntaxFreeVariables (WithSourcePos st) where
    syntaxFreeVariables (MkWithSourcePos _ e) = syntaxFreeVariables e

instance SyntaxFreeVariables SyntaxCase where
    syntaxFreeVariables (MkSyntaxCase pat expr) =
        difference (syntaxFreeVariables expr) (mapMaybe btGetVar $ syntaxBindingVariables pat)

instance SyntaxFreeVariables (SyntaxMulticase n) where
    syntaxFreeVariables (MkSyntaxMulticase pats expr) =
        difference (syntaxFreeVariables expr) (mapMaybe btGetVar $ syntaxBindingVariables pats)

instance SyntaxFreeVariables (Some SyntaxMulticase) where
    syntaxFreeVariables (MkSome m) = syntaxFreeVariables m

instance SyntaxFreeVariables SyntaxMulticaseList where
    syntaxFreeVariables (MkSyntaxMulticaseList _ l) = syntaxFreeVariables l

instance SyntaxFreeVariables SyntaxExpression' where
    syntaxFreeVariables (SESubsume expr _) = syntaxFreeVariables expr
    syntaxFreeVariables (SEConst _) = mempty
    syntaxFreeVariables (SEVar ns name) = opoint $ namespaceConcatFullName ns name
    syntaxFreeVariables (SESpecialForm _ _) = mempty
    syntaxFreeVariables (SEApply f arg) = union (syntaxFreeVariables f) (syntaxFreeVariables arg)
    syntaxFreeVariables (SEAbstract match) = syntaxFreeVariables match
    syntaxFreeVariables (SEAbstracts match) = syntaxFreeVariables match
    syntaxFreeVariables (SEMatch match) = syntaxFreeVariables match
    syntaxFreeVariables (SEMatches match) = syntaxFreeVariables match
    syntaxFreeVariables (SERef expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SEUnref expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SELet binds expr) =
        difference
            (syntaxFreeVariables binds <> syntaxFreeVariables expr)
            (mapMaybe btGetVar $ syntaxBindingVariables binds)
    syntaxFreeVariables (SEList exprs) = syntaxFreeVariables exprs
    syntaxFreeVariables (SEDebug _ expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables SyntaxBinding where
    syntaxFreeVariables (MkSyntaxBinding _ expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables t => SyntaxFreeVariables (SyntaxWithDoc t) where
    syntaxFreeVariables (MkSyntaxWithDoc _ decl) = syntaxFreeVariables decl

instance SyntaxFreeVariables SyntaxRecursiveDeclaration' where
    syntaxFreeVariables (BindingSyntaxDeclaration bind) = syntaxFreeVariables bind
    syntaxFreeVariables _ = mempty

instance SyntaxFreeVariables SyntaxDeclaration' where
    syntaxFreeVariables (DirectSyntaxDeclaration bind) = syntaxFreeVariables bind
    syntaxFreeVariables (RecursiveSyntaxDeclaration decls) = syntaxFreeVariables decls
    syntaxFreeVariables (NamespaceSyntaxDeclaration _ decls) = syntaxFreeVariables decls
    syntaxFreeVariables _ = mempty

class SyntaxBindingVariables t where
    syntaxBindingVariables :: t -> FiniteSet BindingThing

instance SyntaxBindingVariables t => SyntaxBindingVariables [t] where
    syntaxBindingVariables tt = mconcat $ fmap syntaxBindingVariables tt

instance SyntaxBindingVariables t => SyntaxBindingVariables (NonEmpty t) where
    syntaxBindingVariables tt = syntaxBindingVariables $ toList tt

instance SyntaxBindingVariables t => SyntaxBindingVariables (FixedList n t) where
    syntaxBindingVariables tt = syntaxBindingVariables $ toList tt

instance SyntaxBindingVariables st => SyntaxBindingVariables (WithSourcePos st) where
    syntaxBindingVariables (MkWithSourcePos _ pat) = syntaxBindingVariables pat

constructorBindingVariables :: Namespace -> SyntaxConstructor -> FiniteSet BindingThing
constructorBindingVariables ns (SLNamedConstructor name) = singletonSet $ ConsBindingThing ns name
constructorBindingVariables _ _ = mempty

instance SyntaxBindingVariables SyntaxPattern' where
    syntaxBindingVariables AnySyntaxPattern = mempty
    syntaxBindingVariables (VarSyntaxPattern name) = singletonSet $ VarBindingThing name
    syntaxBindingVariables (BothSyntaxPattern pat1 pat2) =
        union (syntaxBindingVariables pat1) (syntaxBindingVariables pat2)
    syntaxBindingVariables (ConstructorSyntaxPattern ns c pats) =
        constructorBindingVariables ns c <> syntaxBindingVariables pats
    syntaxBindingVariables (TypedSyntaxPattern pat _) = syntaxBindingVariables pat
    syntaxBindingVariables (DynamicTypedSyntaxPattern pat _) = syntaxBindingVariables pat
    syntaxBindingVariables (NamespaceSyntaxPattern pat _) = syntaxBindingVariables pat
    syntaxBindingVariables (DebugSyntaxPattern _ pat) = syntaxBindingVariables pat

instance SyntaxBindingVariables t => SyntaxBindingVariables (SyntaxWithDoc t) where
    syntaxBindingVariables (MkSyntaxWithDoc _ decl) = syntaxBindingVariables decl

instance SyntaxBindingVariables SyntaxRecursiveDeclaration' where
    syntaxBindingVariables (BindingSyntaxDeclaration bind) = syntaxBindingVariables bind
    syntaxBindingVariables _ = mempty

instance SyntaxBindingVariables SyntaxDeclaration' where
    syntaxBindingVariables (DirectSyntaxDeclaration bind) = syntaxBindingVariables bind
    syntaxBindingVariables (RecursiveSyntaxDeclaration decls) = syntaxBindingVariables decls
    syntaxBindingVariables _ = mempty

instance SyntaxBindingVariables SyntaxBinding where
    syntaxBindingVariables (MkSyntaxBinding pat _) = syntaxBindingVariables pat
