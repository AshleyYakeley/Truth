module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

qPositiveTypeDescription ::
       forall t. ToPinaforeType t
    => Text
qPositiveTypeDescription =
    case toShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. FromPinaforeType t
    => Text
qNegativeTypeDescription =
    case fromShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Negative) @t of
        MkShimWit w _ -> exprShow w

type LibraryModule = DocTree BindDoc

type EnA = MeetType Entity A

data ScopeEntry
    = BindScopeEntry Name
                     (Maybe (PinaforeContext -> PinaforeBinding))
    | SubtypeScopeEntry [SubypeConversionEntry PinaforeGroundType]

data BindDoc = MkBindDoc
    { bdScopeEntry :: ScopeEntry
    , bdDoc :: DefDoc
    }

mkValEntry ::
       forall t. ToPinaforeType t
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkValEntry name docDescription val = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \pc -> let
            ?pinafore = pc
            in ValueBinding (qConstExprAny $ jmToValue val) Nothing
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docType = ValueDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall t. ToPinaforeType t
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkSupertypeEntry name docDescription _val = let
    bdScopeEntry = BindScopeEntry name Nothing
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docType = SupertypeDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkTypeEntry :: Name -> Text -> PinaforeBoundType -> DocTreeEntry BindDoc
mkTypeEntry name docDescription t = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> TypeBinding t
    docName = unName name
    docValueType = ""
    docType = TypeDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSubtypeRelationEntry :: Text -> Text -> Text -> [SubypeConversionEntry PinaforeGroundType] -> DocTreeEntry BindDoc
mkSubtypeRelationEntry ta tb docDescription scentries = let
    bdScopeEntry = SubtypeScopeEntry scentries
    docName = ta <> " <: " <> tb
    docValueType = ""
    docType = SubtypeRelationDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkValPatEntry ::
       forall t v lt.
       (ToPinaforeType t, FromPinaforeType v, ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt)
    => Name
    -> Text
    -> t
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkValPatEntry name docDescription val pat = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docType = ValuePatternDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSpecialFormEntry :: Name -> Text -> Text -> Text -> PinaforeSpecialForm -> DocTreeEntry BindDoc
mkSpecialFormEntry name docDescription params docValueType sf = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> SpecialFormBinding sf
    docName = unName name <> " " <> params
    docType = ValueDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
