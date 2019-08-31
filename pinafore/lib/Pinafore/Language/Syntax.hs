module Pinafore.Language.Syntax where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.TypeSystem
import Shapes

data TypeDecls baseupdate = MkTypeDecls
    { tdTypes :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
    , tdRelations :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
    }

instance Semigroup (TypeDecls baseupdate) where
    (MkTypeDecls at ar) <> (MkTypeDecls bt br) = MkTypeDecls (at . bt) (ar . br)

instance Monoid (TypeDecls baseupdate) where
    mempty = MkTypeDecls id id
    mappend = (<>)

data SyntaxDeclarations baseupdate =
    MkSyntaxDeclarations (PinaforeScoped baseupdate (TypeDecls baseupdate))
                         [SyntaxBinding baseupdate]

instance Semigroup (SyntaxDeclarations baseupdate) where
    (MkSyntaxDeclarations ta ba) <> (MkSyntaxDeclarations tb bb) = MkSyntaxDeclarations (ta <> tb) (ba <> bb)

instance Monoid (SyntaxDeclarations baseupdate) where
    mempty = MkSyntaxDeclarations mempty mempty
    mappend = (<>)

typeSyntaxDeclarations :: PinaforeScoped baseupdate (TypeDecls baseupdate) -> SyntaxDeclarations baseupdate
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

data SyntaxBinding baseupdate =
    MkSyntaxBinding SourcePos
                    (Maybe SyntaxType)
                    Name
                    (SyntaxExpression baseupdate)

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

data SyntaxCase baseupdate =
    MkSyntaxCase SyntaxPattern
                 (SyntaxExpression baseupdate)

data SyntaxConstant
    = SCIfThenElse
    | SCBind
    | SCBind_
    | SCConstructor SyntaxConstructor

data SyntaxExpression' baseupdate
    = SEConst SyntaxConstant
    | SEVar Name
    | SEApply (SyntaxExpression baseupdate)
              (SyntaxExpression baseupdate)
    | SEAbstract SyntaxPattern
                 (SyntaxExpression baseupdate)
    | SERef (SyntaxExpression baseupdate)
    | SEUnref (SyntaxExpression baseupdate)
    | SELet (SyntaxDeclarations baseupdate)
            (SyntaxExpression baseupdate)
    | SECase (SyntaxExpression baseupdate)
             [SyntaxCase baseupdate]
    | SEList [SyntaxExpression baseupdate]
    | SEProperty SyntaxType
                 SyntaxType
                 Anchor
    | SEEntity SyntaxType
               Anchor

seConst :: SourcePos -> SyntaxConstant -> SyntaxExpression baseupdate
seConst spos sc = MkSyntaxExpression spos $ SEConst sc

seAbstract :: SourcePos -> SyntaxPattern -> SyntaxExpression baseupdate -> SyntaxExpression baseupdate
seAbstract spos pat expr = MkSyntaxExpression spos $ SEAbstract pat expr

seAbstracts :: SourcePos -> [SyntaxPattern] -> SyntaxExpression baseupdate -> SyntaxExpression baseupdate
seAbstracts _ [] expr = expr
seAbstracts spos (p:pp) expr = seAbstract spos p $ seAbstracts spos pp expr

seApply :: SourcePos -> SyntaxExpression baseupdate -> SyntaxExpression baseupdate -> SyntaxExpression baseupdate
seApply spos f a = MkSyntaxExpression spos $ SEApply f a

seApplys :: SourcePos -> SyntaxExpression baseupdate -> [SyntaxExpression baseupdate] -> SyntaxExpression baseupdate
seApplys _ f [] = f
seApplys spos f (a:aa) = seApplys spos (seApply spos f a) aa

data SyntaxExpression baseupdate =
    MkSyntaxExpression SourcePos
                       (SyntaxExpression' baseupdate)

data SyntaxTopDeclarations baseupdate =
    MkSyntaxTopDeclarations SourcePos
                            (SyntaxDeclarations baseupdate)

class HasSourcePos t where
    getSourcePos :: t -> SourcePos

instance HasSourcePos SyntaxPattern where
    getSourcePos (MkSyntaxPattern spos _) = spos

instance HasSourcePos (SyntaxBinding baseupdate) where
    getSourcePos (MkSyntaxBinding spos _ _ _) = spos

instance HasSourcePos (SyntaxExpression baseupdate) where
    getSourcePos (MkSyntaxExpression spos _) = spos

instance HasSourcePos (SyntaxTopDeclarations baseupdate) where
    getSourcePos (MkSyntaxTopDeclarations spos _) = spos

class SyntaxFreeVariables t where
    syntaxFreeVariables :: t -> FiniteSet Name

instance SyntaxFreeVariables t => SyntaxFreeVariables [t] where
    syntaxFreeVariables tt = mconcat $ fmap syntaxFreeVariables tt

instance SyntaxFreeVariables (SyntaxExpression baseupdate) where
    syntaxFreeVariables (MkSyntaxExpression _ e) = syntaxFreeVariables e

instance SyntaxFreeVariables (SyntaxCase baseupdate) where
    syntaxFreeVariables (MkSyntaxCase pat expr) = difference (syntaxFreeVariables expr) (syntaxBindingVariables pat)

instance SyntaxFreeVariables (SyntaxExpression' baseupdate) where
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

instance SyntaxFreeVariables (SyntaxBinding baseupdate) where
    syntaxFreeVariables (MkSyntaxBinding _ _ _ expr) = syntaxFreeVariables expr

instance SyntaxFreeVariables (SyntaxDeclarations baseupdate) where
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

instance SyntaxBindingVariables (SyntaxDeclarations baseupdate) where
    syntaxBindingVariables (MkSyntaxDeclarations _ binds) = syntaxBindingVariables binds

instance SyntaxBindingVariables (SyntaxBinding baseupdate) where
    syntaxBindingVariables (MkSyntaxBinding _ _ name _) = singletonSet name

checkSyntaxBindingsDuplicates ::
       forall baseupdate m. MonadError ErrorType m
    => [SyntaxBinding baseupdate]
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
            b -> throwError $ InterpretBindingsDuplicateError b
    in checkDuplicates . fmap (\(MkSyntaxBinding _ _ name _) -> name)
