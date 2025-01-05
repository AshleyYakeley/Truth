module Pinafore.Language.Interpret.FreeVars
    ( syntaxExpressionFreeVariables
    , syntaxPatternBindingVariables
    ) where

import Import

-- | Actually a superset, some of these might not be free.
syntaxExpressionFreeVariables :: SyntaxExpression -> [FullName]
syntaxExpressionFreeVariables expr = toList $ syntaxFreeVariables expr

data BindingThing
    = VarBindingThing FullName
    | ConsBindingThing Namespace
                       FullNameRef
    deriving stock (Eq)

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
    syntaxFreeVariables :: t -> ListSet FullName

instance SyntaxFreeVariables t => SyntaxFreeVariables (Maybe t) where
    syntaxFreeVariables Nothing = mempty
    syntaxFreeVariables (Just t) = syntaxFreeVariables t

instance SyntaxFreeVariables t => SyntaxFreeVariables [t] where
    syntaxFreeVariables tt = concatmap syntaxFreeVariables tt

instance SyntaxFreeVariables t => SyntaxFreeVariables (NonEmpty t) where
    syntaxFreeVariables tt = syntaxFreeVariables $ toList tt

instance SyntaxFreeVariables t => SyntaxFreeVariables (FixedList n t) where
    syntaxFreeVariables tt = syntaxFreeVariables $ toList tt

instance SyntaxFreeVariables st => SyntaxFreeVariables (WithSourcePos st) where
    syntaxFreeVariables (MkWithSourcePos _ e) = syntaxFreeVariables e

instance SyntaxFreeVariables (Name, SyntaxExpression) where
    syntaxFreeVariables (_, expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables SyntaxCase where
    syntaxFreeVariables (MkSyntaxCase pat expr) =
        difference (syntaxFreeVariables expr) (listSetMapMaybe btGetVar $ syntaxBindingVariables pat)

instance SyntaxFreeVariables (SyntaxMulticase n) where
    syntaxFreeVariables (MkSyntaxMulticase pats expr) =
        difference (syntaxFreeVariables expr) (listSetMapMaybe btGetVar $ syntaxBindingVariables pats)

instance SyntaxFreeVariables (Some SyntaxMulticase) where
    syntaxFreeVariables (MkSome m) = syntaxFreeVariables m

instance SyntaxFreeVariables SyntaxMulticaseList where
    syntaxFreeVariables (MkSyntaxMulticaseList _ l) = syntaxFreeVariables l

instance SyntaxFreeVariables SyntaxDeclarator where
    syntaxFreeVariables (SDLetSeq sdecls) = syntaxFreeVariables sdecls
    syntaxFreeVariables (SDLetRec sdecls) = syntaxFreeVariables sdecls
    syntaxFreeVariables SDImport {} = mempty
    syntaxFreeVariables SDWith {} = mempty

instance SyntaxFreeVariables SyntaxConstructor where
    syntaxFreeVariables (SLNamedConstructor _ mexprs) = syntaxFreeVariables mexprs
    syntaxFreeVariables _ = mempty

instance SyntaxFreeVariables SyntaxConstant where
    syntaxFreeVariables SCIfThenElse = mempty
    syntaxFreeVariables (SCConstructor sc) = syntaxFreeVariables sc

instance SyntaxFreeVariables SyntaxExpression' where
    syntaxFreeVariables (SESubsume expr _) = syntaxFreeVariables expr
    syntaxFreeVariables (SEConst sc) = syntaxFreeVariables sc
    syntaxFreeVariables (SEImplicitVar _) = mempty
    syntaxFreeVariables (SEVar ns name mb) = (opoint $ namespaceConcatFullName ns name) <> syntaxFreeVariables mb
    syntaxFreeVariables (SEApply f arg) = union (syntaxFreeVariables f) (syntaxFreeVariables arg)
    syntaxFreeVariables (SEAbstract match) = syntaxFreeVariables match
    syntaxFreeVariables (SEAbstracts match) = syntaxFreeVariables match
    syntaxFreeVariables (SEMatch match) = syntaxFreeVariables match
    syntaxFreeVariables (SEMatches match) = syntaxFreeVariables match
    syntaxFreeVariables (SEAppQuote expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SEAppUnquote expr) = syntaxFreeVariables expr
    syntaxFreeVariables (SEImply binds expr) =
        concatmap (\(_, _, e) -> syntaxFreeVariables e) binds <> syntaxFreeVariables expr
    syntaxFreeVariables (SEDecl decl expr) =
        difference
            (syntaxFreeVariables decl <> syntaxFreeVariables expr)
            (listSetMapMaybe btGetVar $ syntaxBindingVariables decl)
    syntaxFreeVariables (SEList exprs) = syntaxFreeVariables exprs
    syntaxFreeVariables (SESplice _) = mempty
    syntaxFreeVariables (SEQuoteExpression _) = mempty
    syntaxFreeVariables (SEQuoteScope _) = mempty
    syntaxFreeVariables (SEQuoteType _) = mempty
    syntaxFreeVariables (SEQuoteAnchor _) = mempty
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
    syntaxFreeVariables (NamespaceSyntaxDeclaration _ _ decls) = syntaxFreeVariables decls
    syntaxFreeVariables _ = mempty

class SyntaxBindingVariables t where
    syntaxBindingVariables :: t -> ListSet BindingThing

instance SyntaxBindingVariables t => SyntaxBindingVariables [t] where
    syntaxBindingVariables tt = concatmap syntaxBindingVariables tt

instance SyntaxBindingVariables t => SyntaxBindingVariables (NonEmpty t) where
    syntaxBindingVariables tt = syntaxBindingVariables $ toList tt

instance SyntaxBindingVariables t => SyntaxBindingVariables (FixedList n t) where
    syntaxBindingVariables tt = syntaxBindingVariables $ toList tt

instance SyntaxBindingVariables st => SyntaxBindingVariables (WithSourcePos st) where
    syntaxBindingVariables (MkWithSourcePos _ pat) = syntaxBindingVariables pat

constructorBindingVariables :: Namespace -> SyntaxConstructor -> ListSet BindingThing
constructorBindingVariables ns (SLNamedConstructor name _) = singletonSet $ ConsBindingThing ns name
constructorBindingVariables _ _ = mempty

instance SyntaxBindingVariables FullName where
    syntaxBindingVariables fname = singletonSet $ VarBindingThing fname

instance SyntaxBindingVariables SyntaxPattern' where
    syntaxBindingVariables AnySyntaxPattern = mempty
    syntaxBindingVariables (VarSyntaxPattern name) = syntaxBindingVariables name
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

instance SyntaxBindingVariables SyntaxDeclarator where
    syntaxBindingVariables (SDLetSeq sdecls) = syntaxBindingVariables sdecls
    syntaxBindingVariables (SDLetRec sdecls) = syntaxBindingVariables sdecls
    syntaxBindingVariables SDImport {} = mempty
    syntaxBindingVariables SDWith {} = mempty

instance SyntaxBindingVariables SyntaxDeclaration' where
    syntaxBindingVariables (DirectSyntaxDeclaration bind) = syntaxBindingVariables bind
    syntaxBindingVariables (RecordSyntaxDeclaration name _ _ _) = syntaxBindingVariables name
    syntaxBindingVariables (DeclaratorSyntaxDeclaration declarator) = syntaxBindingVariables declarator
    syntaxBindingVariables (DeclaratorInSyntaxDeclaration _ decl) = syntaxBindingVariables decl
    syntaxBindingVariables _ = mempty

instance SyntaxBindingVariables SyntaxBinding where
    syntaxBindingVariables (MkSyntaxBinding pat _) = syntaxBindingVariables pat
