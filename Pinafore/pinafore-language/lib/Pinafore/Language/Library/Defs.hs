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
       forall t. HasPinaforeType 'Positive t
    => Text
qPositiveTypeDescription =
    case toPolarShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. HasPinaforeType 'Negative t
    => Text
qNegativeTypeDescription =
    case fromPolarShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Negative) @t of
        MkShimWit w _ -> exprShow w

type LibraryModule = DocTree BindDoc

type EnA = MeetType Entity A

data ScopeEntry
    = BindScopeEntry Name
                     (Maybe (PinaforeContext -> PinaforeBinding))
    | SubtypeScopeEntry (SubtypeConversionEntry PinaforeGroundType)

data BindDoc = MkBindDoc
    { bdScopeEntry :: ScopeEntry
    , bdDoc :: DefDoc
    }

mkValEntry ::
       forall t. HasPinaforeType 'Positive t
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
    diName = name
    diType = qPositiveTypeDescription @t
    docItem = ValueDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall t. HasPinaforeType 'Positive t
    => Name
    -> Markdown
    -> ((?pinafore :: PinaforeContext) => t)
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

mkTypeEntry :: Name -> Markdown -> PinaforeBoundType -> DocTreeEntry BindDoc
mkTypeEntry name docDescription t = let
    bdScopeEntry = BindScopeEntry name $ Just $ \_ -> TypeBinding t
    diName = name
    diParams =
        case t of
            MkBoundType pt -> getTypeParameters nameSupply $ pgtVarianceType pt
    docItem = TypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSubtypeRelationEntry :: Text -> Text -> Markdown -> SubtypeConversionEntry PinaforeGroundType -> DocTreeEntry BindDoc
mkSubtypeRelationEntry diSubtype diSupertype docDescription scentry = let
    bdScopeEntry = SubtypeScopeEntry scentry
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

subtypeRelationEntry ::
       forall dva gta a dvb gtb b.
       Markdown
    -> PinaforeGroundType dva gta
    -> DolanArgumentsShimWit PinaforePolyShim dva PinaforeType gta 'Negative a
    -> PinaforeGroundType dvb gtb
    -> DolanArgumentsShimWit PinaforePolyShim dvb PinaforeType gtb 'Positive b
    -> PinaforePolyShim Type a b
    -> DocTreeEntry BindDoc
subtypeRelationEntry desc gta argsa gtb argsb conv = let
    ta = groundedDolanShimWit gta argsa
    tb = groundedDolanShimWit gtb argsb
    in mkSubtypeRelationEntry (exprShow ta) (exprShow tb) desc $ subtypeConversionEntry gta argsa gtb argsb conv

hasSubtypeRelationEntry ::
       forall a b. (HasPinaforeType 'Negative a, HasPinaforeType 'Positive b)
    => Markdown
    -> PinaforePolyShim Type a b
    -> DocTreeEntry BindDoc
hasSubtypeRelationEntry doc conv =
    fromJust $
    toGroundedDolanShimWit pinaforeType $ \gta argsa ->
        fromJust $ toGroundedDolanShimWit pinaforeType $ \gtb argsb -> subtypeRelationEntry doc gta argsa gtb argsb conv

-- | The 'Monoid' trick of representing @Monoid T@ as @List T <: T@.
monoidSubtypeRelationEntry ::
       forall t. (HasPinaforeType 'Negative t, HasPinaforeType 'Positive t, Monoid t)
    => DocTreeEntry BindDoc
monoidSubtypeRelationEntry = hasSubtypeRelationEntry @[t] @t "Monoidal relationship" $ functionToShim "mconcat" mconcat

mkValPatEntry ::
       forall t v lt.
       ( HasPinaforeType 'Positive t
       , HasPinaforeType 'Negative v
       , ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt
       )
    => Name
    -> Markdown
    -> t
    -> (v -> Maybe (ListProduct lt))
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
       Name
    -> Markdown
    -> [Text]
    -> Text
    -> ((?pinafore :: PinaforeContext) => PinaforeSpecialForm)
    -> DocTreeEntry BindDoc
mkSpecialFormEntry name docDescription params diType sf = let
    bdScopeEntry =
        BindScopeEntry name $
        Just $ \pc -> let
            ?pinafore = pc
            in SpecialFormBinding sf
    diName = name
    diParams = params
    docItem = SpecialFormDocItem {..}
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
