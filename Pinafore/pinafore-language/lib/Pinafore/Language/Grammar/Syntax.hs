module Pinafore.Language.Grammar.Syntax where

import Language.Expression.Dolan
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
                                        [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
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
                                        [SyntaxWithDoc SyntaxClosedEntityConstructorOrSubtype]
    | DatatypeSyntaxTypeDeclaration [SyntaxTypeParameter]
                                    [SyntaxWithDoc SyntaxDatatypeConstructorOrSubtype]
    | OpenEntitySyntaxTypeDeclaration
    | DynamicEntitySyntaxTypeDeclaration (NonEmpty SyntaxDynamicEntityConstructor)
    deriving (Eq)

data SyntaxRecursiveDeclaration'
    = TypeSyntaxDeclaration Name
                            SyntaxTypeDeclaration
    | SubtypeSyntaxDeclaration TrustOrVerify
                               SyntaxType
                               SyntaxType
                               (Maybe SyntaxExpression)
    | BindingSyntaxDeclaration SyntaxBinding
    deriving (Eq)

type SyntaxRecursiveDeclaration = SyntaxWithDoc (WithSourcePos SyntaxRecursiveDeclaration')

data SyntaxWithDoc t =
    MkSyntaxWithDoc Markdown
                    t
    deriving (Eq)

data SyntaxExposeDeclaration =
    MkSyntaxExposeDeclaration [Name]
                              [SyntaxDeclaration]
    deriving (Eq)

data SyntaxDeclaration'
    = DirectSyntaxDeclaration SyntaxRecursiveDeclaration'
    | ImportSyntaxDeclaration ModuleName
    | ExposeSyntaxDeclaration SyntaxExposeDeclaration
    | RecursiveSyntaxDeclaration [SyntaxRecursiveDeclaration]
    | UsingSyntaxDeclaration ModuleName
    | NamespaceSyntaxDeclaration ModuleName
                                 [SyntaxDeclaration]
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
typeOperatorFixity "+:" = MkFixity AssocRight 2
typeOperatorFixity "*:" = MkFixity AssocRight 3
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
    MkSyntaxBinding (Maybe SyntaxType)
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
    | NamespaceSyntaxPattern SyntaxPattern
                             ModuleName
    deriving (Eq)

type SyntaxPattern = WithSourcePos SyntaxPattern'

data SyntaxMatch =
    MkSyntaxMatch SyntaxPattern
                  SyntaxExpression
    deriving (Eq)

data SyntaxMultimatch (n :: PeanoNat) =
    MkSyntaxMultimatch (FixedList n SyntaxPattern)
                       SyntaxExpression

syntaxMultimatchLength :: SyntaxMultimatch n -> PeanoNatType n
syntaxMultimatchLength (MkSyntaxMultimatch l _) = fixedListLength l

instance TestEquality SyntaxMultimatch where
    testEquality (MkSyntaxMultimatch patsa expa) (MkSyntaxMultimatch patsb expb) = do
        Refl <- testEquality (fixedListLength patsa) (fixedListLength patsb)
        if expa == expb
            then Just Refl
            else Nothing

instance Eq (SyntaxMultimatch n) where
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

data SyntaxMultimatchList =
    forall (n :: PeanoNat). MkSyntaxMultimatchList (PeanoNatType n)
                                                   [SyntaxMultimatch n]

instance Eq SyntaxMultimatchList where
    MkSyntaxMultimatchList na aa == MkSyntaxMultimatchList nb bb =
        fromMaybe False $ do
            Refl <- testEquality na nb
            return $ aa == bb

data SyntaxExpression'
    = SESubsume SyntaxExpression
                SyntaxType
    | SEConst SyntaxConstant
    | SEVar ReferenceName
    | SESpecialForm ReferenceName
                    (NonEmpty SyntaxAnnotation)
    | SEApply SyntaxExpression
              SyntaxExpression
    | SEAbstract SyntaxMatch
    | SEAbstracts (Some SyntaxMultimatch)
    | SEMatch [SyntaxMatch]
    | SEMatches SyntaxMultimatchList
    | SERef SyntaxExpression
    | SEUnref SyntaxExpression
    | SELet [SyntaxDeclaration]
            SyntaxExpression
    | SEList [SyntaxExpression]
    deriving (Eq)

seConst :: SourcePos -> SyntaxConstant -> SyntaxExpression
seConst spos sc = MkWithSourcePos spos $ SEConst sc

seAbstract :: SourcePos -> SyntaxPattern -> SyntaxExpression -> SyntaxExpression
seAbstract spos pat expr = MkWithSourcePos spos $ SEAbstract $ MkSyntaxMatch pat expr

seAbstracts :: SourcePos -> [SyntaxPattern] -> SyntaxExpression -> SyntaxExpression
seAbstracts _ [] expr = expr
seAbstracts spos (p:pp) expr = seAbstract spos p $ seAbstracts spos pp expr

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
    in checkDuplicates . fmap (\(MkSyntaxBinding _ name _) -> name)
