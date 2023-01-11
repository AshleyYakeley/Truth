{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fconstraint-solver-iterations=20  #-}

module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DefDoc
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Markdown
import Shapes

data ScopeEntry context
    = BindScopeEntry FullNameRef
                     (Maybe (context -> QInterpreterBinding))
    | SubtypeScopeEntry (SubtypeConversionEntry QGroundType)

instance Contravariant ScopeEntry where
    contramap ab (BindScopeEntry name f) = BindScopeEntry name $ fmap (\cb -> cb . ab) f
    contramap _ (SubtypeScopeEntry entry) = SubtypeScopeEntry entry

data BindDoc context = MkBindDoc
    { bdScopeEntry :: Maybe (ScopeEntry context)
    , bdDoc :: DefDoc
    }

instance Contravariant BindDoc where
    contramap ab (MkBindDoc se d) = MkBindDoc (fmap (contramap ab) $ se) d

type BindDocTree context = Tree (BindDoc context)

data LibraryModule context = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: BindDocTree context
    }

instance Contravariant LibraryModule where
    contramap ab (MkLibraryModule n tt) = MkLibraryModule n $ fmap (contramap ab) tt

libraryModuleEntries :: LibraryModule context -> [BindDoc context]
libraryModuleEntries (MkLibraryModule _ lmod) = toList lmod

libraryModuleDocumentation :: LibraryModule context -> Tree DefDoc
libraryModuleDocumentation (MkLibraryModule _ lmod) = fmap bdDoc lmod

type EnA = MeetType Entity A

qPositiveTypeDescription ::
       forall t. HasQType 'Positive t
    => NamedText
qPositiveTypeDescription =
    case toPolarShimWit @Type @(QPolyShim Type) @(QType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. HasQType 'Negative t
    => NamedText
qNegativeTypeDescription =
    case fromPolarShimWit @Type @(QPolyShim Type) @(QType 'Negative) @t of
        MkShimWit w _ -> exprShow w

instance NamespaceConcat (ScopeEntry context) where
    namespaceConcat nsn (BindScopeEntry fn b) = BindScopeEntry (namespaceConcat nsn fn) b
    namespaceConcat _ se = se

instance NamespaceConcat (BindDoc context) where
    namespaceConcat nsn bd =
        bd {bdScopeEntry = namespaceConcat nsn $ bdScopeEntry bd, bdDoc = namespaceConcat nsn $ bdDoc bd}

headingBDT :: MarkdownText -> RawMarkdown -> [BindDocTree context] -> BindDocTree context
headingBDT name desc = Node $ MkBindDoc Nothing $ MkDefDoc (HeadingDocItem name) desc

namespaceBDT :: NamespaceRef -> RawMarkdown -> [BindDocTree context] -> BindDocTree context
namespaceBDT name desc tree =
    Node (MkBindDoc Nothing $ MkDefDoc (NamespaceDocItem name) desc) $ namespaceConcat name tree

valBDT ::
       forall context t. HasQType 'Positive t
    => FullNameRef
    -> RawMarkdown
    -> ((?qcontext :: context) => t)
    -> BindDocTree context
valBDT name docDescription val = let
    bdScopeEntry =
        Just $
        BindScopeEntry name $
        Just $ \context -> let
            ?qcontext = context
            in ValueBinding (qConstExprAny $ jmToValue val) Nothing
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValueDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

newTypeParameter :: State [Name] Name
newTypeParameter = do
    nn <- get
    case nn of
        n:nr -> do
            put nr
            return n
        [] -> return "a"

getTypeParameter :: CCRVarianceType a -> State [Name] SyntaxTypeParameter
getTypeParameter CoCCRVarianceType = do
    v <- newTypeParameter
    return $ PositiveSyntaxTypeParameter v
getTypeParameter ContraCCRVarianceType = do
    v <- newTypeParameter
    return $ NegativeSyntaxTypeParameter v
getTypeParameter RangeCCRVarianceType = do
    vn <- newTypeParameter
    vp <- newTypeParameter
    return $ RangeSyntaxTypeParameter vn vp

getTypeParameters :: [Name] -> DolanVarianceType dv -> [NamedText]
getTypeParameters supply dvt = fmap exprShow $ evalState (listTypeForList dvt getTypeParameter) supply

nameSupply :: [Name]
nameSupply = fmap (\c -> MkName $ pack [c]) ['a' .. 'z']

mkTypeBDT :: forall context. FullNameRef -> RawMarkdown -> QBoundType -> [BindDocTree context] -> BindDocTree context
mkTypeBDT name docDescription t bdChildren = let
    bdScopeEntry = Just $ BindScopeEntry name $ Just $ \_ -> TypeBinding t
    diName = name
    diParams =
        case t of
            MkSomeGroundType pt -> getTypeParameters nameSupply $ pgtVarianceType pt
    docItem = TypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in Node MkBindDoc {..} bdChildren

typeBDT :: forall context. FullNameRef -> RawMarkdown -> QBoundType -> [BindDocTree context] -> BindDocTree context
typeBDT name docDescription t bdChildren = mkTypeBDT name docDescription t bdChildren

subtypeRelationBDT ::
       forall context a b.
       TrustOrVerify
    -> RawMarkdown
    -> QGroundedShimWit 'Negative a
    -> QGroundedShimWit 'Positive b
    -> QPolyShim Type a b
    -> BindDocTree context
subtypeRelationBDT trustme docDescription ta tb conv = let
    diSubtype = exprShow ta
    diSupertype = exprShow tb
    bdScopeEntry = Just $ SubtypeScopeEntry $ subtypeConversionEntry trustme ta tb $ pure conv
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

hasSubtypeRelationBDT ::
       forall a b context. (HasQType 'Negative a, HasQType 'Positive b)
    => TrustOrVerify
    -> RawMarkdown
    -> QPolyShim Type a b
    -> BindDocTree context
hasSubtypeRelationBDT trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationBDT trustme doc ta tb conv

valPatBDT ::
       forall context t v lt.
       (HasQType 'Positive t, HasQType 'Negative v, ToListShimWit (QPolyShim Type) (QType 'Positive) lt)
    => FullNameRef
    -> RawMarkdown
    -> t
    -> PurityFunction Maybe v (ListProduct lt)
    -> BindDocTree context
valPatBDT name docDescription val pat = let
    bdScopeEntry =
        Just $
        BindScopeEntry name $
        Just $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValuePatternDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

specialFormBDT ::
       forall context.
       FullNameRef
    -> RawMarkdown
    -> [NamedText]
    -> NamedText
    -> ((?qcontext :: context) => QSpecialForm)
    -> BindDocTree context
specialFormBDT name docDescription params diType sf = let
    bdScopeEntry =
        Just $
        BindScopeEntry name $
        Just $ \pc -> let
            ?qcontext = pc
            in SpecialFormBinding sf
    diName = name
    diParams = params
    docItem = SpecialFormDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

eqEntries ::
       forall context (a :: Type). (Eq a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocTree context]
eqEntries = [valBDT "==" "Equal." $ (==) @a, valBDT "/=" "Not equal." $ (/=) @a]

applicativeEntries ::
       forall context (f :: Type -> Type).
       ( Applicative f
       , HasQType 'Negative (f TopType)
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       , HasQType 'Positive (f C)
       , HasQType 'Negative (f C)
       , HasQType 'Positive (f (A, B))
       , HasQType 'Negative (f (A -> B))
       )
    => [BindDocTree context]
applicativeEntries =
    [ valBDT "return" "" (pure :: A -> f A)
    , valBDT "ap" "" ((<*>) :: f (A -> B) -> f A -> f B)
    , valBDT "liftA2" "" (liftA2 :: (A -> B -> C) -> f A -> f B -> f C)
    , valBDT "**" "" (liftA2 (,) :: f A -> f B -> f (A, B))
    , valBDT ">>" "" ((*>) :: f TopType -> f A -> f A)
    ]

monadEntries ::
       forall context (f :: Type -> Type).
       ( Monad f
       , HasQType 'Negative (f TopType)
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       , HasQType 'Positive (f C)
       , HasQType 'Negative (f C)
       , HasQType 'Positive (f (A, B))
       , HasQType 'Negative (f (A -> B))
       )
    => [BindDocTree context]
monadEntries = applicativeEntries @context @f <> [valBDT ">>=" "" ((>>=) :: f A -> (A -> f B) -> f B)]

semigroupEntries ::
       forall context (a :: Type). (Semigroup a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocTree context]
semigroupEntries = [valBDT "<>" "" $ (<>) @a, valBDT "concat1" "" $ sconcat @a]

monoidEntries ::
       forall context (a :: Type). (Monoid a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocTree context]
monoidEntries = semigroupEntries @context @a <> [valBDT "empty" "" $ mempty @a, valBDT "concat" "" $ mconcat @a]
