module Pinafore.Language.Grammar.Interpret.TypeDecl.ClosedEntity
    ( makeClosedEntityTypeBox
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

-- data ClosedEntityConstructor t = MkClosedEntityConstructor Name Anchor (HListWit MonoEntityType t)
data ClosedEntityType (t :: Type) where
    NilClosedEntityType :: ClosedEntityType Void
    ConsClosedEntityType
        :: Name
        -> Anchor
        -> ListType MonoEntityType tl
        -> ClosedEntityType tt
        -> ClosedEntityType (Either (HList tl) tt)

instance TestEquality ClosedEntityType where
    testEquality NilClosedEntityType NilClosedEntityType = Just Refl
    testEquality (ConsClosedEntityType _ a1 l1 t1) (ConsClosedEntityType _ a2 l2 t2)
        | a1 == a2 = do
            Refl <- testEquality l1 l2
            Refl <- testEquality t1 t2
            Just Refl
    testEquality _ _ = Nothing

instance Show (ClosedEntityType t) where
    show NilClosedEntityType = "nil"
    show (ConsClosedEntityType name a tt NilClosedEntityType) = show name <> " " <> show tt <> " " <> show a
    show (ConsClosedEntityType name a tt rest) = show name <> " " <> show tt <> " " <> show a <> " | " <> show rest

closedEntityTypeEq :: ClosedEntityType t -> Dict (Eq t)
closedEntityTypeEq NilClosedEntityType = Dict
closedEntityTypeEq (ConsClosedEntityType _ _ t1 tr) =
    case (hListEq monoEntityTypeEq t1, closedEntityTypeEq tr) of
        (Dict, Dict) -> Dict

closedEntityTypeAdapter :: ClosedEntityType t -> EntityAdapter t
closedEntityTypeAdapter NilClosedEntityType = pNone
closedEntityTypeAdapter (ConsClosedEntityType _ a cc rest) =
    constructorEntityAdapter a (mapListType monoEntityAdapter cc) <+++> closedEntityTypeAdapter rest

closedTypeConstructors :: ClosedEntityType t -> [Constructor (HListWit MonoEntityType) t]
closedTypeConstructors NilClosedEntityType = []
closedTypeConstructors (ConsClosedEntityType name _ a1 ar) =
    MkConstructor name (MkHListWit a1) leftCodec : fmap extendConstructor (closedTypeConstructors ar)

interpretClosedEntityTypeConstructor ::
       SyntaxClosedEntityConstructorOrSubtype -> PinaforeInterpreter (Name, Anchor, AnyW (ListType MonoEntityType))
interpretClosedEntityTypeConstructor (ConstructorSyntaxClosedEntityConstructorOrSubtype consName stypes anchor) = do
    etypes <- for stypes interpretMonoEntityType
    return (consName, anchor, assembleListType etypes)
interpretClosedEntityTypeConstructor (SubtypeSyntaxClosedEntityConstructorOrSubtype _subtypeName _conss) =
    throw $ KnownIssueError 132 "Subtypes not supported in closed entity types"

assembleClosedEntityType :: [(Name, Anchor, AnyW (ListType MonoEntityType))] -> AnyW ClosedEntityType
assembleClosedEntityType [] = MkAnyW NilClosedEntityType
assembleClosedEntityType ((n, a, MkAnyW el):cc) =
    case assembleClosedEntityType cc of
        MkAnyW ct -> MkAnyW $ ConsClosedEntityType n a el ct

makeClosedEntityTypeBox ::
       Name -> Markdown -> [SyntaxClosedEntityConstructorOrSubtype] -> PinaforeInterpreter PinaforeTypeBox
makeClosedEntityTypeBox name doc sconss =
    newTypeID $ \(tidsym :: TypeIDType tid) ->
        case unsafeIdentifyKind @_ @Type tidsym of
            Identity Refl -> let
                mktype cet = let
                    epKind = NilListType
                    epCovaryMap = covarymap
                    epEq ::
                           forall (ta :: Type).
                           Arguments (MonoType EntityGroundType) (Identified tid) ta
                        -> Dict (Eq ta)
                    epEq NilArguments = closedEntityTypeEq cet
                    epAdapter :: forall ta. Arguments MonoEntityType (Identified tid) ta -> EntityAdapter ta
                    epAdapter NilArguments = closedEntityTypeAdapter cet
                    epShowType = exprShowPrec name
                    in closedEntityGroundType $
                       MkClosedEntityFamily tidsym $ MkSealedEntityProperties MkEntityProperties {..}
                in mkTypeFixBox name doc (MkBoundType . mktype) $ \_ -> do
                       tconss <- for sconss interpretClosedEntityTypeConstructor
                       case assembleClosedEntityType tconss of
                           MkAnyW (ct :: ClosedEntityType t) -> do
                               tident <- unsafeIdentify @_ @t tidsym
                               let
                                   conss = closedTypeConstructors ct
                                   cti :: ClosedEntityType (Identified tid)
                                   cti = (reflId $ applyRefl id $ invert tident) ct
                                   ctf :: forall polarity. Is PolarityType polarity
                                       => PinaforeShimWit polarity (Identified tid)
                                   ctf =
                                       singleDolanShimWit $
                                       mkPolarShimWit $ GroundedDolanSingularType (mktype cti) NilCCRArguments
                               patts <-
                                   for conss $ \(MkConstructor cname (MkHListWit lt) codec) -> do
                                       ltp <- return $ mapListType monoToPositiveDolanType lt
                                       ltn <- mapMListType monoEntityToNegativePinaforeType lt
                                       let
                                           expr =
                                               qConstExprAny $
                                               MkAnyValue
                                                   (qFunctionPosWitnesses
                                                        ltn
                                                        (mapPolarShimWit (reflId $ invert tident) ctf))
                                                   (encode codec)
                                           pc = toPatternConstructor ctf ltp $ decode codec . reflId tident
                                       withNewPatternConstructor cname doc expr pc
                               return (cti, mconcat patts)
