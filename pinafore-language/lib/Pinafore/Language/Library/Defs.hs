module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Markdown
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
    -> Markdown
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkValEntry name docDescription val = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \pc -> let
            ?pinafore = pc
            in ValueBinding (qConstExprAny $ jmToValue val) Nothing
    docName = toText name
    docValueType = qPositiveTypeDescription @t
    docType = ValueDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall t. ToPinaforeType t
    => Name
    -> Markdown
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkSupertypeEntry name docDescription _val = let
    bdScopeEntry = BindScopeEntry name Nothing
    docName = toText name
    docValueType = qPositiveTypeDescription @t
    docType = SupertypeDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkTypeEntry :: Name -> Markdown -> PinaforeBoundType -> DocTreeEntry BindDoc
mkTypeEntry name docDescription t = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> TypeBinding t
    docName = toText name
    docValueType = ""
    docType = TypeDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSubtypeRelationEntry :: Text -> Text -> Markdown -> [SubypeConversionEntry PinaforeGroundType] -> DocTreeEntry BindDoc
mkSubtypeRelationEntry ta tb docDescription scentries = let
    bdScopeEntry = SubtypeScopeEntry scentries
    docName = ta <> " <: " <> tb
    docValueType = ""
    docType = SubtypeRelationDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

-- | The 'Monoid' trick of representing @Monoid T@ as @[T] <: T@.
monoidSubypeConversionEntry ::
       forall dv gt. Is (SaturatedConstraintWitness Monoid) gt
    => PinaforeGroundType dv gt
    -> SubypeConversionEntry PinaforeGroundType
monoidSubypeConversionEntry t =
    simpleSubtypeConversionEntry (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) t $
    MkSubtypeConversion $ \sc (ConsDolanArguments ta NilDolanArguments :: _ pola _) -> do
        margs <- saturateGroundType t
        case margs of
            MkAnyW args ->
                case saturateArgsConstraint (representative @_ @(SaturatedConstraintWitness Monoid) @gt) args of
                    Compose Dict -> let
                        tb = singleDolanType $ GroundDolanSingularType t args
                        sconv = subtypeConvert sc ta tb
                        cshim :: forall a. JMShim Type (JoinMeetType pola a (LimitType pola)) a
                        cshim =
                            case polarityType @pola of
                                PositiveType -> iJoinL1
                                NegativeType -> iMeetL1
                        in return $
                           MkSubtypeArguments args $
                           fmap (\conv -> functionToShim "mconcat" mconcat . applyCoPolyShim cid (cshim . conv)) sconv

mkValPatEntry ::
       forall t v lt.
       (ToPinaforeType t, FromPinaforeType v, ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt)
    => Name
    -> Markdown
    -> t
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkValPatEntry name docDescription val pat = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat
    docName = toText name
    docValueType = qPositiveTypeDescription @t
    docType = ValuePatternDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSpecialFormEntry ::
       Name
    -> Markdown
    -> Text
    -> Text
    -> ((?pinafore :: PinaforeContext) => PinaforeSpecialForm)
    -> DocTreeEntry BindDoc
mkSpecialFormEntry name docDescription params docValueType sf = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \pc -> let
            ?pinafore = pc
            in SpecialFormBinding sf
    docName = toText name <> " " <> params
    docType = ValueDocType
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
