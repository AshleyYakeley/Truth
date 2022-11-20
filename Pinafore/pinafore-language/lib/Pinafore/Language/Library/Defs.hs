{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
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
    = BindScopeEntry FullName
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

data LibraryModule context = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: DocTree (BindDoc context)
    }

instance Contravariant LibraryModule where
    contramap ab (MkLibraryModule n tt) = MkLibraryModule n $ fmap (contramap ab) tt

libraryModuleEntries :: LibraryModule context -> [BindDoc context]
libraryModuleEntries (MkLibraryModule _ lmod) = toList lmod

libraryModuleDocumentation :: LibraryModule context -> DocTree DefDoc
libraryModuleDocumentation (MkLibraryModule _ lmod) = fmap bdDoc lmod

type EnA = MeetType Entity A

qPositiveTypeDescription ::
       forall t. HasQType 'Positive t
    => Text
qPositiveTypeDescription =
    case toPolarShimWit @Type @(QPolyShim Type) @(QType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. HasQType 'Negative t
    => Text
qNegativeTypeDescription =
    case fromPolarShimWit @Type @(QPolyShim Type) @(QType 'Negative) @t of
        MkShimWit w _ -> exprShow w

instance NamespaceRelative t => NamespaceRelative (DocTree t) where
    namespaceRelative nsn = fmap $ namespaceRelative nsn

instance NamespaceRelative t => NamespaceRelative (DocTreeEntry t) where
    namespaceRelative nsn = fmap $ namespaceRelative nsn

instance NamespaceRelative (ScopeEntry context) where
    namespaceRelative nsn (BindScopeEntry fn b) = BindScopeEntry (namespaceRelative nsn fn) b
    namespaceRelative _ se = se

instance NamespaceRelative (BindDoc context) where
    namespaceRelative nsn bd =
        bd {bdScopeEntry = namespaceRelative nsn $ bdScopeEntry bd, bdDoc = namespaceRelative nsn $ bdDoc bd}

mkValEntry ::
       forall context t. HasQType 'Positive t
    => FullName
    -> Markdown
    -> ((?qcontext :: context) => t)
    -> DocTreeEntry (BindDoc context)
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
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall context t. HasQType 'Positive t
    => FullName
    -> Markdown
    -> ((?qcontext :: context) => t)
    -> DocTreeEntry (BindDoc context)
mkSupertypeEntry name docDescription _val = let
    bdScopeEntry = BindScopeEntry name Nothing
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = SupertypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

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

getTypeParameters :: [Name] -> DolanVarianceType dv -> [Text]
getTypeParameters supply dvt = fmap exprShow $ evalState (listTypeForList dvt getTypeParameter) supply

nameSupply :: [Name]
nameSupply = fmap (\c -> MkName $ pack [c]) ['a' .. 'z']

mkTypeEntry :: forall context. FullName -> Markdown -> QBoundType -> DocTreeEntry (BindDoc context)
mkTypeEntry name docDescription t = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> TypeBinding t
    diName = name
    diParams =
        case t of
            MkSomeGroundType pt -> getTypeParameters nameSupply $ pgtVarianceType pt
    docItem = TypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSubtypeRelationEntry ::
       forall context. Text -> Text -> Markdown -> SubtypeConversionEntry QGroundType -> DocTreeEntry (BindDoc context)
mkSubtypeRelationEntry diSubtype diSupertype docDescription scentry = let
    bdScopeEntry = SubtypeScopeEntry scentry
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

subtypeRelationEntry ::
       forall context a b.
       TrustOrVerify
    -> Markdown
    -> QGroundedShimWit 'Negative a
    -> QGroundedShimWit 'Positive b
    -> QPolyShim Type a b
    -> DocTreeEntry (BindDoc context)
subtypeRelationEntry trustme desc ta tb conv =
    mkSubtypeRelationEntry (exprShow ta) (exprShow tb) desc $ subtypeConversionEntry trustme ta tb $ pure conv

hasSubtypeRelationEntry ::
       forall a b context. (HasQType 'Negative a, HasQType 'Positive b)
    => TrustOrVerify
    -> Markdown
    -> QPolyShim Type a b
    -> DocTreeEntry (BindDoc context)
hasSubtypeRelationEntry trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationEntry trustme doc ta tb conv

mkValPatEntry ::
       forall context t v lt.
       (HasQType 'Positive t, HasQType 'Negative v, ToListShimWit (QPolyShim Type) (QType 'Positive) lt)
    => FullName
    -> Markdown
    -> t
    -> PurityFunction Maybe v (ListProduct lt)
    -> DocTreeEntry (BindDoc context)
mkValPatEntry name docDescription val pat = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValuePatternDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSpecialFormEntry ::
       forall context.
       FullName
    -> Markdown
    -> [Text]
    -> Text
    -> ((?qcontext :: context) => QSpecialForm)
    -> DocTreeEntry (BindDoc context)
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
    in EntryDocTreeEntry MkBindDoc {..}
