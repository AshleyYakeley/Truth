module Pinafore.Language.Grammar.Interpret.TypeDecl.ClosedEntity
    ( makeClosedEntityTypeBox
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

data ClosedEntityBox =
    forall t. MkClosedEntityBox (ClosedEntityType t)
                                [Constructor MonoEntityType t]

interpretClosedEntityTypeConstructor ::
       SyntaxClosedEntityConstructorOrSubtype -> PinaforeInterpreter (Name, Anchor, AnyW (ListType MonoEntityType))
interpretClosedEntityTypeConstructor (ConstructorSyntaxClosedEntityConstructorOrSubtype consName stypes anchor) = do
    etypes <- for stypes interpretMonoEntityType
    return (consName, anchor, assembleListType etypes)
interpretClosedEntityTypeConstructor (SubtypeSyntaxClosedEntityConstructorOrSubtype _subtypeName _conss) =
    throw $ KnownIssueError 132 "Subtypes not supported in closed entity types"

assembleClosedEntityType :: [(Name, Anchor, AnyW (ListType MonoEntityType))] -> ClosedEntityBox
assembleClosedEntityType [] = MkClosedEntityBox NilClosedEntityType []
assembleClosedEntityType ((n, a, MkAnyW el):cc) =
    case assembleClosedEntityType cc of
        MkClosedEntityBox ct conss ->
            MkClosedEntityBox (ConsClosedEntityType a el ct) $
            (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

makeClosedEntityTypeBox ::
       Name -> Markdown -> [SyntaxClosedEntityConstructorOrSubtype] -> PinaforeInterpreter PinaforeTypeBox
makeClosedEntityTypeBox name doc sconss =
    newTypeID $ \(tidsym :: TypeIDType n) ->
        case unsafeIdentifyKind @_ @Type tidsym of
            Identity Refl -> let
                mktype t = MkBoundType $ closedEntityGroundType $ MkClosedEntityFamily name tidsym t
                in mkTypeFixBox name doc mktype $ \_ -> do
                       tconss <- for sconss interpretClosedEntityTypeConstructor
                       MkClosedEntityBox (ct :: ClosedEntityType t) conss <- return $ assembleClosedEntityType tconss
                       tident <- unsafeIdentify @_ @t tidsym
                       let
                           cti :: ClosedEntityType (Identified n)
                           cti = (reflId $ applyRefl id $ invert tident) ct
                           ctf :: forall polarity. Is PolarityType polarity
                               => PinaforeShimWit polarity (Identified n)
                           ctf =
                               singleDolanShimWit $
                               mkPolarShimWit $
                               GroundedDolanSingularType
                                   (closedEntityGroundType $ MkClosedEntityFamily name tidsym cti)
                                   NilCCRArguments
                       patts <-
                           for conss $ \(MkConstructor cname lt at tma) -> do
                               ltp <- return $ mapListType monoToPositiveDolanType lt
                               ltn <- mapMListType monoEntityToNegativePinaforeType lt
                               let
                                   expr =
                                       qConstExprAny $
                                       MkAnyValue
                                           (qFunctionPosWitnesses ltn (mapPolarShimWit (reflId $ invert tident) ctf))
                                           at
                                   pc = toPatternConstructor ctf ltp $ tma . reflId tident
                               withNewPatternConstructor cname doc expr pc
                       return (cti, mconcat patts)
