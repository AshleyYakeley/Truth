{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Context
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

data LibraryModule = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: DocTree BindDoc
    }

type EnA = MeetType Entity A

data ScopeEntry
    = BindScopeEntry FullName
                     (Maybe (QContext -> QInterpreterBinding))
    | SubtypeScopeEntry (SubtypeConversionEntry QGroundType)

data BindDoc = MkBindDoc
    { bdScopeEntry :: ScopeEntry
    , bdDoc :: DefDoc
    }

instance NamespaceRelative t => NamespaceRelative (DocTree t) where
    namespaceRelative nsn = fmap $ namespaceRelative nsn

instance NamespaceRelative t => NamespaceRelative (DocTreeEntry t) where
    namespaceRelative nsn = fmap $ namespaceRelative nsn

instance NamespaceRelative BindDoc where
    namespaceRelative nsn bd =
        case bdScopeEntry bd of
            BindScopeEntry fn b -> bd {bdScopeEntry = BindScopeEntry (namespaceRelative nsn fn) b}
            _ -> bd

mkValEntry ::
       forall t. HasQType 'Positive t
    => FullName
    -> Markdown
    -> ((?qcontext :: QContext) => t)
    -> DocTreeEntry BindDoc
mkValEntry name docDescription val = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \pc -> let
            ?qcontext = pc
            in ValueBinding (qConstExprAny $ jmToValue val) Nothing
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValueDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall t. HasQType 'Positive t
    => FullName
    -> Markdown
    -> ((?qcontext :: QContext) => t)
    -> DocTreeEntry BindDoc
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
getTypeParameters supply dvt = fmap exprShow $ evalState (listTypeFor dvt getTypeParameter) supply

nameSupply :: [Name]
nameSupply = fmap (\c -> MkName $ pack [c]) ['a' .. 'z']

mkTypeEntry :: FullName -> Markdown -> QBoundType -> DocTreeEntry BindDoc
mkTypeEntry name docDescription t = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> TypeBinding t
    diName = name
    diParams =
        case t of
            MkSomeGroundType pt -> getTypeParameters nameSupply $ pgtVarianceType pt
    docItem = TypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSubtypeRelationEntry :: Text -> Text -> Markdown -> SubtypeConversionEntry QGroundType -> DocTreeEntry BindDoc
mkSubtypeRelationEntry diSubtype diSupertype docDescription scentry = let
    bdScopeEntry = SubtypeScopeEntry scentry
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

subtypeRelationEntry ::
       forall a b.
       TrustOrVerify
    -> Markdown
    -> QGroundedShimWit 'Negative a
    -> QGroundedShimWit 'Positive b
    -> QPolyShim Type a b
    -> DocTreeEntry BindDoc
subtypeRelationEntry trustme desc ta tb conv =
    mkSubtypeRelationEntry (exprShow ta) (exprShow tb) desc $ subtypeConversionEntry trustme ta tb $ pure conv

hasSubtypeRelationEntry ::
       forall a b. (HasQType 'Negative a, HasQType 'Positive b)
    => TrustOrVerify
    -> Markdown
    -> QPolyShim Type a b
    -> DocTreeEntry BindDoc
hasSubtypeRelationEntry trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationEntry trustme doc ta tb conv

mkValPatEntry ::
       forall t v lt. (HasQType 'Positive t, HasQType 'Negative v, ToListShimWit (QPolyShim Type) (QType 'Positive) lt)
    => FullName
    -> Markdown
    -> t
    -> PurityFunction Maybe v (ListProduct lt)
    -> DocTreeEntry BindDoc
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
       FullName -> Markdown -> [Text] -> Text -> ((?qcontext :: QContext) => QSpecialForm) -> DocTreeEntry BindDoc
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
