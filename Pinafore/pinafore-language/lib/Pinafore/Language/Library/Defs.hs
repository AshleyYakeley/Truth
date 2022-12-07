{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
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
    { bdScopeEntry :: ScopeEntry context
    , bdDoc :: DefDoc
    }

instance Contravariant BindDoc where
    contramap ab (MkBindDoc se d) = MkBindDoc (contramap ab se) d

type BindDocTree context = Tree (BindDoc context)

data LibraryModule context = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: DocTree (BindDocTree context)
    }

instance Contravariant LibraryModule where
    contramap ab (MkLibraryModule n tt) = MkLibraryModule n $ fmap (fmap $ contramap ab) tt

libraryModuleEntries :: LibraryModule context -> [BindDoc context]
libraryModuleEntries (MkLibraryModule _ lmod) = mconcat $ fmap toList $ toList lmod

libraryModuleDocumentation :: LibraryModule context -> DocTree (Tree DefDoc)
libraryModuleDocumentation (MkLibraryModule _ lmod) = fmap (fmap bdDoc) lmod

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

instance NamespaceRelative (ScopeEntry context) where
    namespaceRelative nsn (BindScopeEntry fn b) = BindScopeEntry (namespaceRelative nsn fn) b
    namespaceRelative _ se = se

instance NamespaceRelative (BindDoc context) where
    namespaceRelative nsn bd =
        bd {bdScopeEntry = namespaceRelative nsn $ bdScopeEntry bd, bdDoc = namespaceRelative nsn $ bdDoc bd}

instance NamespaceRelative (BindDocTree context) where
    namespaceRelative nsn bd =
        bd {rootLabel = namespaceRelative nsn $ rootLabel bd, subForest = fmap (namespaceRelative nsn) $ subForest bd}

mkValEntry ::
       forall context t. HasQType 'Positive t
    => FullNameRef
    -> Markdown
    -> ((?qcontext :: context) => t)
    -> DocTreeEntry (BindDocTree context)
mkValEntry name docDescription val = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \context -> let
            ?qcontext = context
            in ValueBinding (qConstExprAny $ jmToValue val) Nothing
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValueDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry $ pure MkBindDoc {..}

mkSupertypeEntry ::
       forall context t. HasQType 'Positive t
    => FullNameRef
    -> Markdown
    -> ((?qcontext :: context) => t)
    -> DocTreeEntry (BindDocTree context)
mkSupertypeEntry name docDescription _val = let
    bdScopeEntry = BindScopeEntry name Nothing
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = SupertypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry $ pure MkBindDoc {..}

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

mkTypeBDT :: forall context. FullNameRef -> Markdown -> QBoundType -> [BindDocTree context] -> BindDocTree context
mkTypeBDT name docDescription t bdChildren = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> TypeBinding t
    diName = name
    diParams =
        case t of
            MkSomeGroundType pt -> getTypeParameters nameSupply $ pgtVarianceType pt
    docItem = TypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in Node MkBindDoc {..} bdChildren

mkTypeEntry ::
       forall context.
       FullNameRef
    -> Markdown
    -> QBoundType
    -> [BindDocTree context]
    -> DocTreeEntry (BindDocTree context)
mkTypeEntry name docDescription t bdChildren = EntryDocTreeEntry $ mkTypeBDT name docDescription t bdChildren

subtypeRelationBDT ::
       forall context a b.
       TrustOrVerify
    -> Markdown
    -> QGroundedShimWit 'Negative a
    -> QGroundedShimWit 'Positive b
    -> QPolyShim Type a b
    -> BindDocTree context
subtypeRelationBDT trustme docDescription ta tb conv = let
    diSubtype = exprShow ta
    diSupertype = exprShow tb
    bdScopeEntry = SubtypeScopeEntry $ subtypeConversionEntry trustme ta tb $ pure conv
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

hasSubtypeRelationBindDoc ::
       forall a b context. (HasQType 'Negative a, HasQType 'Positive b)
    => TrustOrVerify
    -> Markdown
    -> QPolyShim Type a b
    -> BindDocTree context
hasSubtypeRelationBindDoc trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationBDT trustme doc ta tb conv

hasSubtypeRelationEntry ::
       forall a b context. (HasQType 'Negative a, HasQType 'Positive b)
    => TrustOrVerify
    -> Markdown
    -> QPolyShim Type a b
    -> DocTreeEntry (BindDocTree context)
hasSubtypeRelationEntry trustme doc conv = EntryDocTreeEntry $ hasSubtypeRelationBindDoc trustme doc conv

mkValPatBDT ::
       forall context t v lt.
       (HasQType 'Positive t, HasQType 'Negative v, ToListShimWit (QPolyShim Type) (QType 'Positive) lt)
    => FullNameRef
    -> Markdown
    -> t
    -> PurityFunction Maybe v (ListProduct lt)
    -> BindDocTree context
mkValPatBDT name docDescription val pat = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValuePatternDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

mkSpecialFormEntry ::
       forall context.
       FullNameRef
    -> Markdown
    -> [NamedText]
    -> NamedText
    -> ((?qcontext :: context) => QSpecialForm)
    -> DocTreeEntry (BindDocTree context)
mkSpecialFormEntry name docDescription params diType sf = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \pc -> let
            ?qcontext = pc
            in SpecialFormBinding sf
    diName = name
    diParams = params
    docItem = SpecialFormDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry $ pure MkBindDoc {..}
