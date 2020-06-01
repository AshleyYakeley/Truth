module Pinafore.Language.Interpret.TypeDecl
    ( interpretTypeDeclarations
    ) where

import qualified Data.List as List
import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Sealed
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Syntax
import Pinafore.Language.Type.Data
import Pinafore.Language.Type.Identified
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Nonpolar
import Pinafore.Language.TypeSystem.Show
import Shapes

data Constructor w t =
    forall a. MkConstructor Name
                            (ListType w a)
                            (HList a -> t)
                            (t -> Maybe (HList a))

extendConstructor :: Constructor w t -> Constructor w (Either a t)
extendConstructor (MkConstructor n lt at tma) = MkConstructor n lt (Right . at) (\t -> eitherRight t >>= tma)

data ClosedEntityBox =
    forall t. MkClosedEntityBox (ClosedEntityType t)
                                [Constructor ConcreteEntityType t]

assembleClosedEntityType :: [(Name, Anchor, AnyW (ListType ConcreteEntityType))] -> ClosedEntityBox
assembleClosedEntityType [] = MkClosedEntityBox NilClosedEntityType []
assembleClosedEntityType ((n, a, MkAnyW el):cc) =
    case assembleClosedEntityType cc of
        MkClosedEntityBox ct conss ->
            MkClosedEntityBox (ConsClosedEntityType a el ct) $
            (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

data DataBox =
    forall t. MkDataBox (PinaforeDataType t)
                        [Constructor (PinaforeNonpolarType '[]) t]

assembleDataType :: [(Name, AnyW (ListType (PinaforeNonpolarType '[])))] -> DataBox
assembleDataType [] = MkDataBox NilDataType []
assembleDataType ((n, MkAnyW el):cc) =
    case assembleDataType cc of
        MkDataBox ct conss ->
            MkDataBox (ConsDataType el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

datatypeIOWitness :: IOWitness ('MkWitKind IdentifiedType)
datatypeIOWitness = $(iowitness [t|'MkWitKind IdentifiedType|])

constructorFreeVariables :: Constructor (PinaforeNonpolarType '[]) t -> [AnyW SymbolType]
constructorFreeVariables (MkConstructor _ lt _ _) = mconcat $ listTypeToList nonPolarTypeFreeVariables lt

interpretClosedEntityTypeConstructor ::
       SyntaxClosedEntityConstructor -> PinaforeSourceScoped (Name, Anchor, AnyW (ListType ConcreteEntityType))
interpretClosedEntityTypeConstructor (MkSyntaxClosedEntityConstructor consName stypes anchor) = do
    etypes <- for stypes interpretConcreteEntityType
    return (consName, anchor, assembleListType etypes)

interpretDataTypeConstructor ::
       SyntaxDatatypeConstructor -> PinaforeSourceScoped (Name, AnyW (ListType (PinaforeNonpolarType '[])))
interpretDataTypeConstructor (MkSyntaxDatatypeConstructor consName stypes) = do
    etypes <- for stypes interpretNonpolarType
    return (consName, assembleListType etypes)

interpretTypeDeclaration ::
       Name -> TypeID -> SyntaxTypeDeclaration -> PinaforeTypeBox (WMFunction PinaforeScoped PinaforeScoped)
interpretTypeDeclaration name tid OpenEntitySyntaxTypeDeclaration =
    MkTypeBox name (\_ -> OpenEntityNamedType tid) $ return ((), id)
interpretTypeDeclaration name tid (ClosedEntitySyntaxTypeDeclaration sconss) =
    valueToWitness tid $ \(tidsym :: TypeIDType n) -> let
        mktype t = ClosedEntityNamedType tidsym t
        in MkTypeBox name mktype $ do
               tconss <- for sconss interpretClosedEntityTypeConstructor
               MkClosedEntityBox (ct :: ClosedEntityType t) conss <- return $ assembleClosedEntityType tconss
               tident :: Identified n :~: t <- unsafeGetIdentification
               let
                   cti :: ClosedEntityType (Identified n)
                   cti = (reflId $ applyRefl id $ invert tident) ct
                   ctf :: forall polarity. Is PolarityType polarity
                       => PinaforeTypeShimWit polarity (Identified n)
                   ctf =
                       singlePinaforeShimWit $
                       mkShimWit $
                       GroundPinaforeSingularType
                           (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType name tidsym cti)
                           NilDolanArguments
               patts <-
                   for conss $ \(MkConstructor cname lt at tma) -> do
                       ltp <- return $ mapListType concreteEntityToPositivePinaforeType lt
                       patt <- withNewPatternConstructor cname $ toPatternConstructor ctf ltp $ tma . reflId tident
                       ltn <- mapMListType concreteEntityToNegativePinaforeType lt
                       bind <-
                           return $
                           MkWMFunction $
                           withNewBindings $
                           singletonMap cname $
                           qConstExprAny $
                           MkAnyValue (qFunctionPosWitnesses ltn (mapShimWit (reflId $ invert tident) ctf)) at
                       return $ patt . bind
               return (cti, compAll patts)
interpretTypeDeclaration name tid (DatatypeSyntaxTypeDeclaration sconss) =
    valueToWitness tid $ \tidsym -> let
        pt = MkProvidedType datatypeIOWitness $ MkIdentifiedType tidsym
        mktype _ = SimpleNamedType NilListType NilDolanVarianceMap (exprShowPrec name) pt
        in MkTypeBox name mktype $ do
               tconss <- for sconss interpretDataTypeConstructor
               MkDataBox _dt conss <- return $ assembleDataType tconss
               let
                   freevars :: [AnyW SymbolType]
                   freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                   declaredvars :: [AnyW SymbolType]
                   declaredvars = [] -- ISSUE #41
                   unboundvars :: [AnyW SymbolType]
                   unboundvars = freevars List.\\ declaredvars
               case nonEmpty unboundvars of
                   Nothing -> return ()
                   Just vv -> throw $ InterpretUnboundTypeVariables $ fmap (\(MkAnyW s) -> symbolTypeToName s) vv
               let
                   ctf :: forall polarity. Is PolarityType polarity
                       => PinaforeTypeShimWit polarity _
                   ctf =
                       singlePinaforeShimWit $
                       mkShimWit $
                       GroundPinaforeSingularType
                           (SimpleGroundType NilListType NilDolanVarianceMap (exprShowPrec name) pt)
                           NilDolanArguments
               tident <- unsafeGetIdentification
               let tiso = reflId tident
               patts <-
                   for conss $ \(MkConstructor cname lt at tma) -> do
                       ltp <- return $ mapListType nonpolarToPinaforeType lt
                       patt <-
                           withNewPatternConstructor cname $
                           toPatternConstructor ctf ltp $ \t -> tma $ isoForwards tiso t
                       ltn <- return $ mapListType nonpolarToPinaforeType lt
                       bind <-
                           return $
                           MkWMFunction $
                           withNewBindings $
                           singletonMap cname $
                           qConstExprAny $ MkAnyValue (qFunctionPosWitnesses ltn ctf) $ \hl -> isoBackwards tiso $ at hl
                       return $ patt . bind
               return ((), compAll patts)

interpretTypeDeclarations ::
       [(SourcePos, Name, SyntaxTypeDeclaration)] -> PinaforeSourceScoped (WMFunction PinaforeScoped PinaforeScoped)
interpretTypeDeclarations decls = do
    wfs <-
        for decls $ \(spos, name, tdecl) ->
            localSourcePos spos $ do
                tid <- liftSourcePos newTypeID
                return $ interpretTypeDeclaration name tid tdecl
    (wtt, wcc) <- registerTypeNames wfs
    return $ wtt . compAll wcc
